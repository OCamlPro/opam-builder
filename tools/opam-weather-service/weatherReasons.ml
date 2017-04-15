(* This code is ported from OPAM's src/solver/opamCudf.ml *)

module List = struct

  include List

  let map f l =
    List.rev (rev_map f l)

  end

(* glue *)
module OpamPackage = struct
  let to_string (n,v) = String.concat "." [n;v]
  let name_to_string = fst
end
let dose_dummy_request = "dose-dummy-request"
let is_dose_request cpkg = cpkg.Cudf.package = dose_dummy_request
let cudf2opam cpkg =
  Cudf.lookup_package_property cpkg "opam-name",
  Cudf.lookup_package_property cpkg "opam-version"
let string_of_relop = function
    `Eq -> "="
  | `Neq -> "&neq;"
  | `Geq -> "&geq;"
  | `Gt -> "&gt;"
  | `Leq -> "&leq;"
  | `Lt -> "&lt;"
let string_of_atom (p, c) =
  let const = function
    | None       -> ""
    | Some (r,v) -> Printf.sprintf " %s %s" (string_of_relop r) v in
  Printf.sprintf "%s%s" p (const c)
let unav_reasons atom = Printf.sprintf "%s is not available" (string_of_atom atom)
let vpkg2atom cudf2opam universe (name,cstr) =
  let fake_cudf name =
    {Cudf.default_package with Cudf.pkg_extra = [
         "opam-name",`String (Common.CudfAdd.decode name);
         "opam-version",`String "dummy"
       ]} in
  match cstr with
  | None -> (fst (cudf2opam (fake_cudf name))), None
  | Some (relop,v) ->
      try
        let pkg = Cudf.lookup_package universe (name,v) in
        let name,version = cudf2opam pkg in
        name, Some (relop,version)
      with Not_found ->
        Printf.eprintf "Warning: ref to %s.%d\n" name v;
        (fst (cudf2opam (fake_cudf name))), None

module OpamFormula = struct
  type t = (string * ([`Eq|`Neq|`Geq|`Gt|`Leq|`Lt] * string) option) list list
  let ands l = [l]
  let ors l = List.map ands l
  let vcomp = Versioning.Debian.compare
  let vmin a b = if vcomp a b <= 0 then a else b
  let vmax a b = if vcomp a b >= 0 then a else b
  (* Functions to simplify version constraints (this isn't in OPAM !)
     Take care not to oversimplify conflicting constraints or we
     lose valuable info *)
  let and_cstrs c1 c2 = match c1, c2 with
    | (`Gt, a), (`Gt, b) -> [`Gt, vmax a b]
    | (`Geq, a), (`Geq, b) -> [`Geq, vmax a b]
    | (`Gt, a), (`Geq, b) | (`Geq, b), (`Gt, a) ->
        if vcomp a b >= 0 then [(`Gt, a)] else [(`Geq, b)]
    | (`Lt, a), (`Lt, b) -> [`Lt, vmin a b]
    | (`Leq, a), (`Leq, b) -> [`Leq, vmin a b]
    | (`Lt, a), (`Leq, b) | (`Leq, b), (`Lt, a) ->
        if vcomp a b <= 0 then [`Lt, a] else [`Leq, b]
    | (`Geq, a), (`Eq, b) | (`Eq, b), (`Geq, a) when vcomp a b <= 0 -> [`Eq, b]
    | (`Gt,  a), (`Eq, b) | (`Eq, b), (`Gt,  a) when vcomp a b <  0 -> [`Eq, b]
    | (`Leq, a), (`Eq, b) | (`Eq, b), (`Leq, a) when vcomp a b >= 0 -> [`Eq, b]
    | (`Lt,  a), (`Eq, b) | (`Eq, b), (`Lt,  a) when vcomp a b >  0 -> [`Eq, b]
    | (`Geq, a), (`Neq, b) | (`Neq, b), (`Geq, a) when vcomp a b >  0 -> [`Geq, a]
    | (`Gt,  a), (`Neq, b) | (`Neq, b), (`Gt,  a) when vcomp a b >= 0 -> [`Gt, a]
    | (`Leq, a), (`Neq, b) | (`Neq, b), (`Leq, a) when vcomp a b <  0 -> [`Leq, a]
    | (`Lt,  a), (`Neq, b) | (`Neq, b), (`Lt,  a) when vcomp a b <= 0 -> [`Lt, a]
    | c1, c2 -> if c1 = c2 then [c1] else [c1;c2]
  let or_cstrs c1 c2 = match c1, c2 with
    | (`Gt, a), (`Gt, b) -> [`Gt, vmin a b]
    | (`Geq, a), (`Geq, b) -> [`Geq, vmin a b]
    | (`Gt, a), (`Geq, b) | (`Geq, b), (`Gt, a) ->
        if vcomp a b < 0 then [`Gt, a] else [`Geq, b]
    | (`Lt, a), (`Lt, b) -> [`Lt, vmax a b]
    | (`Leq, a), (`Leq, b) -> [`Leq, vmax a b]
    | (`Lt, a), (`Leq, b) | (`Leq, b), (`Lt, a) ->
        if vcomp a b > 0 then [`Lt, a] else [`Leq, b]
    | (`Geq, a), (`Eq, b) | (`Eq, b), (`Geq, a) when vcomp a b <= 0 -> [`Geq, a]
    | (`Gt,  a), (`Eq, b) | (`Eq, b), (`Gt,  a) when vcomp a b <  0 -> [`Gt, a]
    | (`Leq, a), (`Eq, b) | (`Eq, b), (`Leq, a) when vcomp a b >= 0 -> [`Leq, a]
    | (`Lt,  a), (`Eq, b) | (`Eq, b), (`Lt,  a) when vcomp a b >  0 -> [`Lt, a]
    | (`Geq, a), (`Neq, b) | (`Neq, b), (`Geq, a) when vcomp a b >  0 -> [`Neq, b]
    | (`Gt,  a), (`Neq, b) | (`Neq, b), (`Gt,  a) when vcomp a b >= 0 -> [`Neq, b]
    | (`Leq, a), (`Neq, b) | (`Neq, b), (`Leq, a) when vcomp a b <  0 -> [`Neq, b]
    | (`Lt,  a), (`Neq, b) | (`Neq, b), (`Lt,  a) when vcomp a b <= 0 -> [`Neq, b]
    | c1, c2 -> if c1 = c2 then [c1] else [c1;c2]
  let rec add_and_cstr c = function
    | [] -> [c]
    | c1::r -> match and_cstrs c c1 with
      | [c] -> add_and_cstr c r
      | _ -> c1 :: add_and_cstr c r
  let rec add_or_cstr c cs =
    match c with
    | [c] -> (match cs with
        | [] -> [[c]]
        | [c1]::r -> (match or_cstrs c c1 with
            | [c] -> add_or_cstr [c] r
            | _ -> [c1] :: add_or_cstr [c] r)
        | cs -> [c] :: cs)
    | _ -> cs @ [c]

  (* -- (simplifying) printer -- *)

  (* Put back formulas within atoms to shorten the result *)

  let pp_formula t =
    let rec by_name_and (n1,cs) = function
      | (n2,None)::r ->
          if n1 = n2 then by_name_and (n1, cs) r
          else (n1, cs)::by_name_and (n2, []) r
      | (n2,Some c2)::r ->
          if n1 = n2 then by_name_and (n1, add_and_cstr c2 cs) r
          else (n1, cs)::by_name_and (n2, [c2]) r
      | [] -> [n1, cs]
    in
    let gather_names_and = function
      | (n,None)::r -> by_name_and (n,[]) r
      | (n,Some c)::r -> by_name_and (n,[c]) r
      | [] -> []
    in
    let rec by_name_or and1 f = match and1, f with
      | [n1,_], [n2,[]]::r ->
          if n1 = n2 then by_name_or [n1, []] r
          else and1::by_name_or [n2,[]] r
      | [n1,cs], [n2,c2]::r ->
          if n1 = n2 then by_name_or [n1, add_or_cstr c2 cs] r
          else and1::by_name_or [n2,[c2]] r
      | and1, ands::r ->
          and1 :: by_name_or (List.map (fun (a,c) -> a,[c]) ands) r
      | and1, [] -> [and1]
    in
    match List.rev_map gather_names_and t with
    | a::r -> by_name_or (List.map (fun (a,c) -> a,[c]) a) r
    | [] -> []

  let concat_paren c = function
    | _ :: _ :: _ as l -> Printf.sprintf "(%s)" (String.concat c l)
    | l -> String.concat c l

  let gen_print_formula a2s t =
    concat_paren " | "
      (List.map (fun l -> concat_paren " &amp; " (List.map a2s l)) t)

  let constrained_pkg_to_string (n,cf) =
    Printf.sprintf "%s %s" n
      (gen_print_formula (fun (relop,v) ->
           Printf.sprintf "%s %s" (string_of_relop relop) v
         ) cf)

  let to_string t =
    gen_print_formula constrained_pkg_to_string (pp_formula t)

  let to_string_list t =
    List.map (fun l ->
        String.concat " &amp; " (List.map constrained_pkg_to_string l))
      (pp_formula t)
end
let vpkg2opam cudf2opam cudf_universe vpkg =
  match vpkg2atom cudf2opam cudf_universe vpkg with
  | p, None -> p, None
  | p, Some (relop,v) -> p, Some (relop, v)

(* code taken from OPAM *)

module Pkg = struct
  type t = Cudf.package
  include Common.CudfAdd
  let to_string = string_of_package
  let name_to_string t = t.Cudf.package
  let version_to_string t = string_of_int t.Cudf.version
end
module Map = Map.Make(Pkg)
module Set = Set.Make(Pkg)

let strings_of_reason cudf2opam unav_reasons cudf_universe r =
  let open Algo.Diagnostic in
  match r with
  | Conflict (i,j,_) ->
    if is_dose_request i || is_dose_request j then
      let a = if is_dose_request i then j else i in
      if is_dose_request a then [] else
        let str =
          Printf.sprintf "Conflicting query for package %s"
            (OpamPackage.to_string (cudf2opam a)) in
        [str]
    else
    let nva, nvb =
      let nvi = cudf2opam i in
      let nvj = cudf2opam j in
      min nvi nvj, max nvi nvj in
    if i.Cudf.package = j.Cudf.package then
      let str = Printf.sprintf "Unsatisfiable version constraints for %s"
          (OpamPackage.name_to_string nva) in
      [str]
    else
    let str = Printf.sprintf "%s is in conflict with %s"
        (OpamPackage.to_string nva)
        (OpamPackage.to_string nvb) in
    [str]
  | Missing (p,missing) when is_dose_request p -> (* Requested pkg missing *)
    List.map (fun p ->
        unav_reasons (vpkg2atom cudf2opam cudf_universe p)
      ) missing
  | Missing (_,missing) -> (* Dependencies missing *)
      let formula =
        List.map (fun m -> [vpkg2atom cudf2opam cudf_universe m]) missing in
      List.map (fun s -> s ^ " is not available")
        (OpamFormula.to_string_list (List.sort compare formula))
  | Dependency _  -> []

let make_chains cudf_universe cudf2opam depends =
  let open Algo.Diagnostic in
  let map_addlist k v map =
    try Map.add k (v @ Map.find k map) map
    with Not_found -> Map.add k v map in
  let roots,notroots,deps,vpkgs =
    List.fold_left (fun (roots,notroots,deps,vpkgs) -> function
        | Dependency (i, vpkgl, jl) when not (is_dose_request i) ->
          Set.add i roots,
          List.fold_left (fun notroots j -> Set.add j notroots) notroots jl,
          map_addlist i jl deps,
          map_addlist i vpkgl vpkgs
        | Missing (i, vpkgl) when not (is_dose_request i) ->
          let jl =
            List.map (fun (package,_) ->
                {Cudf.default_package with Cudf.package})
              vpkgl in
          Set.add i roots,
          notroots,
          map_addlist i jl deps,
          map_addlist i vpkgl vpkgs
        | _ -> roots, notroots, deps, vpkgs)
      (Set.empty,Set.empty,Map.empty,Map.empty)
      depends
  in
  let roots = Set.diff roots notroots in
  if Set.is_empty roots then [] else
  let children cpkgs =
    List.fold_left (fun acc c ->
        List.fold_left (fun m a -> a :: m) acc
          (try Map.find c deps with Not_found -> []))
      [] cpkgs
  in
  let rec aux constrs direct_deps =
    if direct_deps = [] then [[]] else
    let depnames =
      List.fold_left (fun set p -> WeatherMisc.StringSet.add p.Cudf.package set)
        WeatherMisc.StringSet.empty direct_deps in
    WeatherMisc.StringSet.fold (fun name acc ->
        let name_deps = (* Gather all deps with the given name *)
          List.filter (fun p -> p.Cudf.package = name) direct_deps in
        let name_constrs =
          List.map (List.filter (fun (n,_) -> n = name)) constrs in
        let name_constrs = List.filter ((<>) []) name_constrs in
        let name_constrs =
          WeatherMisc.sort_nodups compare name_constrs in
        (* XXX this is changed from OPAM XXX *)
        let to_opam_and_formula constrs =
          let atoms =
            (List.map (fun p -> vpkg2opam cudf2opam cudf_universe p)
               (WeatherMisc.sort_nodups compare constrs)) in
          atoms
          (* match atoms with _::_::_ -> Block (OpamFormula.ands atoms) *)
          (*                | _ -> OpamFormula.ands atoms *) in
        let formula = List.map to_opam_and_formula name_constrs in
        let children_constrs =
          List.rev @@ List.rev_map (fun p -> try Map.find p vpkgs with Not_found -> []) name_deps in
        let chains = aux children_constrs (children name_deps) in
        List.fold_left
          (fun acc chain -> (formula :: chain) :: acc)
          acc chains
      )
      depnames []
  in
  let roots_list = Set.elements roots in
  let start_constrs =
    List.map (fun cpkg -> [cpkg.Cudf.package,None]) roots_list in
  aux start_constrs roots_list

let string_of_reasons cudf2opam unav_reasons cudf_universe reasons =
  let open Algo.Diagnostic in
  let depends, reasons =
    reasons,
    List.filter (function Dependency _ -> false | _ -> true) reasons in
  let b = Buffer.create 1024 in
  let reasons = (* Gather "missing"s *)
    let reasons, missing = List.fold_left (fun (reasons,missing) -> function
        | Missing (_, m) -> reasons, m@missing
        | r -> r::reasons, missing)
        ([],[]) reasons
    in
    let reasons = Missing (Cudf.default_package, missing) :: reasons in
    List.flatten
      (List.map
         (strings_of_reason cudf2opam unav_reasons cudf_universe)
         reasons) in
  let reasons = WeatherMisc.sort_nodups compare reasons in
  let chains = make_chains cudf_universe cudf2opam depends in
  (* XXX changed from OPAM XXX *)
  let string_of_chain c =
    let sl = List.map OpamFormula.to_string c in
    if List.exists (fun s -> String.length s >= 80) sl then
      String.concat "<br/>\n<b>&rarr;</b> " sl
    else
      String.concat " <b>&rarr;</b> " sl
  in
  if chains <> [] then
    Printf.bprintf b
      "The following dependencies couldn't be met:<ul>\n%a</ul>\n"
      (fun b ->
         List.iter
           (fun c -> Printf.bprintf b "  <li>%s</li>\n" (string_of_chain c)))
      chains;
  if reasons <> [] then
    Printf.bprintf b
      "Here are the root causes:<ul>\n%a</ul>\n"
      (fun b -> List.iter (fun r -> Printf.bprintf b " <li>%s</li>\n" r))
      reasons;
  if reasons = [] && chains = [] then (* No explanation found :( *)
    prerr_endline "Warning: No explanation found for bad package";
  Buffer.contents b

(* -- Glue again -- *)
let string_of_reasons namelink univ reasons =
  let cudf2opam cp =
    let name, version = cudf2opam cp in
    namelink name, version
  in
  string_of_reasons cudf2opam unav_reasons univ reasons
