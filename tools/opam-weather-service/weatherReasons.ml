(* This code is ported from OPAM's src/solver/opamCudf.ml *)

open StringCompat
open Algo.Diagnostic

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

(* a simplified version, does it give the same results ? *)
let vpkg2atom cudf2opam universe (name,cstr) =
  match cstr with
  | None -> Common.CudfAdd.decode name, None
  | Some (relop,v) ->
      try
        let pkg = Cudf.lookup_package universe (name,v) in
        let name,version = cudf2opam pkg in
        name, Some (relop,version)
      with Not_found ->
        Printf.eprintf "Warning: ref to %s.%d\n" name v;
        Common.CudfAdd.decode name, None


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


















let string_of_relop = function
    `Eq -> "="
  | `Neq -> "<>"
  | `Geq -> ">="
  | `Gt -> ">"
  | `Leq -> "<="
  | `Lt -> "<"

let string_of_reasons
      namelink (* from name to link *)
      (univ: Cudf.universe)
      (reasons : Algo.Diagnostic.reason list)
  =
  let cudf2opam cp =
    let name, version = cudf2opam cp in
    namelink name, (version : string)
  in
  let b = Buffer.create 1000 in
  Printf.bprintf b "RAW REASONS\n";
  let vpkg2opam = vpkg2opam cudf2opam univ in
  let string_of_vpkg (p, relop) =
    match relop with
      None -> p
    | Some (relop,v) ->
       Printf.sprintf "%s (%s %s)" p (string_of_relop relop) v
  in
  List.iter (function
             | Missing (p, vpkgl) ->
                let p,v = cudf2opam p in
                let vpkgl = List.map vpkg2opam vpkgl in

                Printf.bprintf b "* Missing %s.%s\n" p v;
                List.iter (fun vpkg ->
                    Printf.bprintf b "    with %s\n"
                                   (string_of_vpkg vpkg)
                  ) vpkgl

             | Dependency (p1,vpkgl,ps) ->
                let p1,v1 = cudf2opam p1 in
                let vpkgl = List.map vpkg2opam vpkgl in
                let ps = List.map cudf2opam ps in

                Printf.bprintf b "* Dependency %s.%s\n" p1 v1;
                List.iter (fun vpkg ->
                    Printf.bprintf b "    with %s\n"
                                   (string_of_vpkg vpkg)
                  ) vpkgl;
                List.iter (fun (p,v) ->
                    Printf.bprintf b "    on %s.%s\n" p v;
                  ) ps

             | Conflict (p1, p2, vpkg) ->
                let p1,v1 = cudf2opam p1 in
                let p2,v2 = cudf2opam p2 in
                let vpkg = vpkg2opam vpkg in

                Printf.bprintf b "* Conflict %s.%s with %s.%s because %s\n"
                               p1 v1 p2 v2 (string_of_vpkg vpkg)
            ) reasons;
  Buffer.contents b

            (*
type reason =
  |Dependency of (Cudf.package * Cudf_types.vpkg list * Cudf.package list)
  (** Not strictly a un-installability, Dependency (a,vpkglist,pkglist) is used
      to recontruct the the dependency path from the root package to the
      offending un-installable package *)
  |Missing of (Cudf.package * Cudf_types.vpkg list)
  (** Missing (a,vpkglist) means that the dependency
      [vpkglist] of package [a] cannot be satisfied *)
  |Conflict of (Cudf.package * Cudf.package * Cudf_types.vpkg)
  (** Conflict (a,b,vpkg) means that the package [a] is in conflict
      with package [b] because of vpkg *)
             *)



















type to_install = {
  ti_package : Cudf.package;

  ti_name : string;
  ti_version : string;

  mutable ti_needed_by : to_solve list;
}

and to_solve = {
  ts_name : (string * string option) list;

  mutable ts_needed_by : to_install list;
}



type broken_reason =
  | UnsatConstraint of to_solve
  | ConflictingPackages of to_install * to_install







let convert_error cudf2opam
                  (pkg : Cudf.package)
                  (univ : Cudf.universe)
                  (reasons : Algo.Diagnostic.reason list) =

  let vpkg2opam = vpkg2opam cudf2opam univ in

  let needed_to_install = Hashtbl.create 113 in
  let needed_to_solve = Hashtbl.create 113 in

  let find_to_install pkg =
    let ti_name = pkg.Cudf.package in
    let ti_vnum = pkg.Cudf.version in
    let key = (ti_name, ti_vnum) in
    try
      Hashtbl.find needed_to_install key
    with Not_found ->
      let ti_name, ti_version = cudf2opam pkg in
      let ti_name = Common.CudfAdd.decode ti_name in
      let ti = {
        ti_package = pkg;
        ti_name;
        ti_version;
        ti_needed_by = [];
      } in
      Hashtbl.add needed_to_install key ti;
      ti
  in

  let find_to_solve vpkgs =
    let vpkgs = List.sort compare vpkgs in
    let key = vpkgs in
    try
      Hashtbl.find needed_to_solve key
    with Not_found ->
      let ts_name =
        List.map (fun vpkg ->
            let (ts_name, constr) = vpkg2opam vpkg in
             match constr with
             | None -> (ts_name, None)
             | Some (ts_relop, ts_version) ->
               (ts_name, Some (
                  Printf.sprintf "(%s %s)"
                    (string_of_relop ts_relop)
                    ts_version)))
          vpkgs
      in
      let ts = {
        ts_name;
        ts_needed_by = [];
      } in
      Hashtbl.add needed_to_solve key ts;
      ts
  in

  let (requested_pkg : to_install) = find_to_install pkg in
  let broken_reasons = ref [] in

  List.iter (function
    | Dependency(needed_by, vpkgs, pkgs_to_install) ->
      let needed_by = find_to_install needed_by in
      let to_solve = find_to_solve vpkgs in
      if not (List.memq needed_by to_solve.ts_needed_by) then
        to_solve.ts_needed_by <- needed_by :: to_solve.ts_needed_by;
      List.iter (fun pkg ->
        let to_install = find_to_install pkg in
        if not (List.memq to_solve to_install.ti_needed_by) then
          to_install.ti_needed_by <- to_solve :: to_install.ti_needed_by;
      ) pkgs_to_install

    | Conflict (pkg1, pkg2, _) ->
      let pkg1 = find_to_install pkg1 in
      let pkg2 = find_to_install pkg2 in
      broken_reasons := ConflictingPackages (pkg1, pkg2) :: !broken_reasons

    | Missing (pkg, vpkgs) ->
      let to_install = find_to_install pkg in
      let to_solve = find_to_solve vpkgs in
      if not (List.memq to_install to_solve.ts_needed_by) then
        to_solve.ts_needed_by <- to_install :: to_solve.ts_needed_by;

      broken_reasons :=
        (UnsatConstraint to_solve)
        :: !broken_reasons
  ) reasons;
  requested_pkg, !broken_reasons

(*

let html_dir = "html"
let templates_dir = "templates"
let fullreport_html = Filename.concat html_dir "report_full.html"
let report_html = Filename.concat html_dir "report.html"

let date = Sys.argv.(1)
let time, _ =
  Scanf.sscanf date "%i-%i-%i-%i-%i-%i"
    (fun year mon mday hour min sec ->
      Unix.mktime {
        Unix.tm_year = year - 1900;
        Unix.tm_mon = mon - 1;
        Unix.tm_mday = mday;
        Unix.tm_hour = hour;
        Unix.tm_min = min;
        Unix.tm_sec = sec;

        Unix.tm_wday = 0;
        Unix.tm_yday = 0;
        Unix.tm_isdst = false;
      })

let reportdir = Sys.argv.(2)
let versions = Array.sub Sys.argv 3 (Array.length Sys.argv - 3)

let reporthtmldir = Filename.concat html_dir reportdir

let version_pages = ref StringMap.empty

let make_version_page p v expl =
  let key = p ^ "." ^ v ^ ".html" in
  let page = try
    StringMap.find key !version_pages
  with Not_found ->
    let page = (p, v, ref []) in
    version_pages := StringMap.add key page !version_pages;
    page
  in
  begin match expl, page with
      Some (ocaml_version, root, reasons), (_, _, list) ->
      list := (ocaml_version, root, reasons) :: !list
    | None, _ -> ()
  end;
  key
 *)

let add_to_refmap key v map =
  try
    let ref = StringMap.find key map in
    ref := v :: !ref;
    map
  with Not_found ->
    StringMap.add key (ref [v]) map

let string_of_ts_name to_record ts_name =
  String.concat "|| "
    (List.map (fun (pkgname, constr) ->
(*       (match to_record with None -> ()
                           | Some ti ->
                             st.version_unsat_constraints <-
                               add_to_refmap pkgname (ti, constr)
                                             st.version_unsat_constraints); *)
       match constr with
       | None ->
         Printf.sprintf "unknown package %S" pkgname;
       | Some ts_constr ->
         Printf.sprintf "%s %s"
           pkgname
           ts_constr)
       ts_name)

let buffer_reason b root reason =
  let rec
    fprintf_to_solve to_record root ts =
    Printf.bprintf b "%s" (string_of_ts_name to_record ts.ts_name);
    match ts.ts_needed_by with
    | [] -> assert false
    | ti :: _ ->
      Printf.bprintf b " needed by ";
      fprintf_to_install root ti
  and
    fprintf_to_install root ti =
    Printf.bprintf b "(%s %s)"
      ti.ti_name ti.ti_version;
    match ti.ti_needed_by with
    | [] -> assert (ti == root)
    | ts :: _ ->
      Printf.bprintf b " solution of ";
      fprintf_to_solve None root ts
  in
  begin
    match reason with
    | UnsatConstraint ts ->
      Printf.bprintf b "* Unsatisfied constraint:\n";
      fprintf_to_solve (Some root) root ts;
      Printf.bprintf b "\n";
    | ConflictingPackages (ti1, ti2) ->
      Printf.bprintf b "* Package:\n";
      fprintf_to_install root ti1;
      Printf.bprintf b "\n";
      Printf.bprintf b "  conflicts with package:\n";
      fprintf_to_install root ti2;
      Printf.bprintf b "\n";
  end;
  ()

let string_of_reasons
      namelink (* from name to link *)
      (pkg: Cudf.package)
      (univ: Cudf.universe)
      (reasons : Algo.Diagnostic.reason list)
  =
  let cudf2opam cp =
    let name, version = cudf2opam cp in
    namelink name, (version : string)
  in

  let s2 = string_of_reasons namelink univ reasons in

  let b = Buffer.create 1000 in
  let root_pkg, reasons = convert_error cudf2opam pkg univ reasons in
  List.iter (fun r -> buffer_reason b root_pkg r) reasons;
  let s1 = Buffer.contents b in

  s1 ^ s2

(*

let print_broken_package oc st root reasons version_page =
  Printf.bprintf oc "<p>Broken version <a href=\"%s\">%s %s</a></p>\n"
    version_page root.ti_name root.ti_version;
  Printf.bprintf oc "<table>\n";

  let reason_strings = ref StringSet.empty in
  List.iter (function reason ->
    let reason = string_of_reason st root reason in
    reason_strings := StringSet.add reason !reason_strings
  ) reasons;
  StringSet.iter (fun s ->
    Printf.bprintf oc "%s\n" s) !reason_strings;
  Printf.bprintf oc "</table>\n";
  ()


let generate_version_pages () =
  StringMap.iter (fun key (p,v, list) ->
    let key = p ^ "." ^ v ^ ".html" in
    let oc = Html.begin_page
        (Filename.concat reporthtmldir key)
        (Printf.sprintf "Package %s, version %s" p v) in
    let opam_file = Printf.sprintf "opam-repository/packages/%s/%s.%s/opam"
        p p v in
    let opam_file =
      if not (Sys.file_exists opam_file) then
        let maybe = Printf.sprintf "opam-repository/packages/%s.%s/opam"
            p v in
        if Sys.file_exists maybe then maybe else opam_file
      else opam_file
    in
    Printf.bprintf oc "<h3>%s</h3>\n" opam_file;
    begin try
      let content = string_of_file opam_file in
      Printf.bprintf oc "<pre>%s</pre>\n" content;
    with e ->
      Printf.eprintf "Warning: error while generating page for %s.%s\n%!"
        p v;
      Printf.bprintf oc "<pre>(could not be loaded)</pre>\n"
    end;

    List.iter (fun (st, root, reasons) ->
      Printf.bprintf oc "<h3>OCaml Version %s</h3>\n" st.ocaml_version;
      match reasons with
      | [] -> Printf.bprintf oc "<p>Correct</p>\n"
      | _ ->
        print_broken_package oc st root reasons key
    ) (List.sort compare !list); (* TODO: use comparison on versions !!! *)
  ) !version_pages


(*
let print_summary oc s =
  List.iter (fun bp ->
    match bp.broken with
    | [] -> ()
    | reasons ->
      print_broken_package oc bp.package reasons
  ) s.packages;
  ()
*)

let load_bin_report filename =
  let ic = open_in filename in
  let s = String.create 12 in
  really_input ic s 0 12;
  let (results : summary) = input_value ic in
  close_in ic;
  results

let repo_version =
  let ic = open_in "html/new-repo.txt" in
  let line = input_line ic in
  close_in ic;
  line

let make_report report_html results st =


    StringMap.iter (fun ti_name pkg ->
      let oc = Html.begin_page  (Filename.concat reporthtmldir
            (Printf.sprintf "%s-%s.html" st.ocaml_version ti_name))
        (Printf.sprintf "%s on version %s" ti_name st.ocaml_version) in

      Printf.bprintf oc "<h3>%s is not available on <a href=\"report-%s.html\">version %s</a></h3> " ti_name st.ocaml_version st.ocaml_version;
      Printf.bprintf oc "<h4>Incorrect versions (%d)</h4>\n"
        (StringMap.cardinal pkg.pkg_incorrect);
      Printf.bprintf oc "<ul>\n";
      StringMap.iter (fun version pkg ->
        Printf.bprintf oc "<li>\n";
        let root = pkg.package in
        let version_page = make_version_page
            root.ti_name root.ti_version (Some (st, pkg.package, pkg.broken)) in
        print_broken_package oc st pkg.package pkg.broken version_page;
        Printf.bprintf oc "</li>\n";
      ) pkg.pkg_incorrect;
      Printf.bprintf oc "</ul>\n";
    ) st.packages_unavailable;


    StringMap.iter (fun ti_name pkg ->
      let oc =
      Html.begin_page  (Filename.concat reporthtmldir
            (Printf.sprintf "%s-%s.html" st.ocaml_version ti_name))
        (Printf.sprintf "%s on version %s" ti_name st.ocaml_version) in

      Printf.bprintf oc "<h3>%s is partially available on <a href=\"report-%s.html\">version %s</a></h3> " ti_name st.ocaml_version st.ocaml_version;

      Printf.bprintf oc "<h4>Installable versions (%d)</h4>\n"
        (StringMap.cardinal pkg.pkg_correct);
      Printf.bprintf oc "<p>All dependencies for these versions can be met.</p>\n";

      Printf.bprintf oc "<ul>\n";
      StringMap.iter (fun version pkg ->
        Printf.bprintf oc "<li><a href=\"%s\">%s</a></li>\n"
          (make_version_page ti_name version (Some (st, pkg.package, [])) ) version;
      ) pkg.pkg_correct;
      Printf.bprintf oc "</ul>\n";
      Printf.bprintf oc "<h4>Incorrect versions (%d)</h4><ul>\n"
        (StringMap.cardinal pkg.pkg_incorrect);
      Printf.bprintf oc "<p>Some dependencies for these versions can NEVER be met.</p>\n";

      StringMap.iter (fun version pkg ->
        Printf.bprintf oc "<li>\n";
        let root = pkg.package in
        let version_page = make_version_page
            root.ti_name root.ti_version (Some (st, pkg.package,pkg.broken)) in
        print_broken_package oc st pkg.package pkg.broken version_page;
        Printf.bprintf oc "</li>\n";
      ) pkg.pkg_incorrect;
      Printf.bprintf oc "</ul>\n";
    ) st.packages_partial;




    StringMap.iter (fun ti_name pkg ->
      let oc = Html.begin_page (Filename.concat reporthtmldir
            (Printf.sprintf "%s-%s.html" st.ocaml_version ti_name))
        (Printf.sprintf "%s on version %s" ti_name st.ocaml_version) in

      Printf.bprintf oc "<h3>%s is fully available on <a href=\"report-%s.html\">version %s</a></h3> " ti_name st.ocaml_version st.ocaml_version;
      Printf.bprintf oc "<h4>Installable versions (%d)</h4>\n"
      (StringMap.cardinal pkg.pkg_correct);
      Printf.bprintf oc "<p>All dependencies for these versions can be met.</p>\n";
      Printf.bprintf oc "<p><ul>\n";
      StringMap.iter (fun version pkg ->
        Printf.bprintf oc "<li><a href=\"%s\">%s</a></li>\n"
          (make_version_page ti_name version (Some (st,pkg.package,[]))) version;
      ) pkg.pkg_correct;
      Printf.bprintf oc "</ul></p>\n";

    ) st.packages_correct;



  let missing_packages = ref [] in
  let with_missing_constraints = ref [] in

  StringMap.iter (fun ti_name list_ref ->
    let filename = Filename.concat reporthtmldir
        (Printf.sprintf "%s-%s.html" st.ocaml_version ti_name) in
    try
      let oc = Html.find_page filename in

      Printf.bprintf oc "<h4>Failed constraints on this package (%d)</h4>\n"
        (List.length !list_ref);
      Printf.bprintf oc "<p>The following packages/versions are not installable, because some constraint cannot be met on this package.</p>";
      Printf.bprintf oc "<p><ul>\n";
      List.iter (fun (ti, constr) ->
        Printf.bprintf oc "<li><a href=\"%s.%s.html\">%s (%s)</a></li>\n" ti.ti_name ti.ti_version  ti.ti_name ti.ti_version
      ) !list_ref;
      Printf.bprintf oc "</ul></p>\n";

      with_missing_constraints := ti_name :: !with_missing_constraints

    with Not_found ->
      let oc = Html.begin_page (Filename.concat reporthtmldir
            (Printf.sprintf "%s-%s.html" st.ocaml_version ti_name))
        (Printf.sprintf "%s on version %s" ti_name st.ocaml_version) in

      Printf.bprintf oc "<h3>%s is unknown on <a href=\"report-%s.html\">version %s</a></h3> " ti_name st.ocaml_version st.ocaml_version;
      Printf.bprintf oc "<h4>Missing in the following constraints (%d)</h4>\n"
        (List.length !list_ref);
      Printf.bprintf oc "<ul>\n";
      List.iter (fun (ti, constr) ->
        Printf.bprintf oc "<li><a href=\"%s.%s.html\">%s (%s)</a></li>\n" ti.ti_name ti.ti_version  ti.ti_name ti.ti_version
      ) !list_ref;
      Printf.bprintf oc "</ul>\n";
      missing_packages := ti_name :: !missing_packages;
  ) st.version_unsat_constraints;

  let oc =  Html.begin_page report_html
    (Printf.sprintf "Report for version %s" st.ocaml_version) in
  let tm = results.time in
  Printf.bprintf oc "<h3><a href=\"../index.html\">Return to Home</a></h3>\n";
  Printf.bprintf oc "<h3>Repository: %s</h3>\n" repo_version;
  Printf.bprintf oc "<h3>Generated on %s</h3>\n" (string_of_date tm);
  Printf.bprintf oc "<h3><a href=\"%s.cudf\">CUDF file</a></h3>"
    st.ocaml_version;

  Printf.bprintf oc "<h2>Statistics</h2>\n";

  Printf.bprintf oc "<table>\n";
  Printf.bprintf oc "<tr><td>Total packages</td><td>%d</td></tr>\n" st.total_npackages;
  Printf.bprintf oc "<tr><td>Total installable packages</td><td>%d</td></tr>\n"
    (StringMap.cardinal st.packages_correct +
       StringMap.cardinal st.packages_partial);
  Printf.bprintf oc "<tr><td>Total correct packages</td><td>%d</td></tr>\n"
    (StringMap.cardinal st.packages_correct);
  Printf.bprintf oc "<tr><td>Total <a href=\"#partial\">partial packages</a></td><td>%d</td></tr>\n"
    (StringMap.cardinal st.packages_partial);
  Printf.bprintf oc "<tr><td>Total incorrect packages</td><td>%d</td></tr>\n"
    (StringMap.cardinal st.packages_partial +
       StringMap.cardinal st.packages_unavailable);
  Printf.bprintf oc "<tr><td>Total <a href=\"#unavailable\">unavailable packages </a></td><td>%d</td></tr>\n"
    (StringMap.cardinal st.packages_unavailable);

  Printf.bprintf oc "<tr><td>Total versions</td><td>%d</td></tr>\n" st.total_nversions;
  Printf.bprintf oc "<tr><td>Total installable versions</td><td>%d</td></tr>\n" st.total_nversions_correct;
  Printf.bprintf oc "<tr><td>Total broken versions</td><td>%d</td></tr>\n" st.total_nversions_incorrect;
  Printf.bprintf oc "<tr><td>Total <a href=\"#missing\">missing packages</a></td><td>%d</td></tr>\n" (List.length !missing_packages);
  Printf.bprintf oc "<tr><td>With <a href=\"#failures\">failed constraints</a> </td><td>%d</td></tr>\n"
  (List.length !with_missing_constraints);
  Printf.bprintf oc "</table>\n";

    Printf.bprintf oc "<h2>Unavailable Packages (%d)</h2>\n"
      (StringMap.cardinal st.packages_unavailable);
  Printf.bprintf oc "<a name=\"%s\"> </a>\n" "unavailable";

    Printf.bprintf oc "<p><ul>\n";
    StringMap.iter (fun ti_name pkg ->
      Printf.bprintf oc "<li><a href=\"%s-%s.html\">%s</a></li>\n"
        st.ocaml_version ti_name ti_name;
    ) st.packages_unavailable;
    Printf.bprintf oc "</ul></p>\n";


    Printf.bprintf oc "<h2>Partial Packages (%d)</h2>\n"
      (StringMap.cardinal st.packages_partial);

  Printf.bprintf oc "<a name=\"%s\"> </a>\n" "partial";

    Printf.bprintf oc "<p><ul>\n";
    StringMap.iter (fun ti_name pkg ->
      Printf.bprintf oc "<li><a href=\"%s-%s.html\">%s</a></li>\n" st.ocaml_version ti_name ti_name;
    ) st.packages_partial;
    Printf.bprintf oc "</ul></p>\n";

    Printf.bprintf oc "<h2>Correct Packages (%d)</h2>\n"
      (StringMap.cardinal st.packages_correct);

  Printf.bprintf oc "<a name=\"%s\"> </a>\n" "correct";

    Printf.bprintf oc "<p><ul>\n";
    StringMap.iter (fun ti_name pkg ->
      Printf.bprintf oc "<li><a href=\"%s-%s.html\">%s</a></li>\n" st.ocaml_version ti_name ti_name;
    ) st.packages_correct;
    Printf.bprintf oc "</ul></p>\n";


    Printf.bprintf oc "<h2>Missing Packages (%d)</h2>\n"
      (List.length !missing_packages);
  Printf.bprintf oc "<a name=\"%s\"> </a>\n" "missing";


    Printf.bprintf oc "<p>These packages appear in constraints, but are either not compatible with this version, or are completely unknown.</p>\n";

    Printf.bprintf oc "<p><ul>\n";
    List.iter (fun ti_name ->
      Printf.bprintf oc "<li><a href=\"%s-%s.html\">%s</a></li>\n" st.ocaml_version ti_name ti_name;
    ) !missing_packages;
    Printf.bprintf oc "</ul></p>\n";

    Printf.bprintf oc "<h2>Packages with failed constraints  (%d)</h2>\n"
      (List.length !with_missing_constraints);

    Printf.bprintf oc "<a name=\"%s\"> </a>\n" "failures";

    Printf.bprintf oc "<p>These packages already appeared before. They appear here again because they are causing broken constraints on other packages.</p>\n";

    Printf.bprintf oc "<p><ul>\n";
    List.iter (fun ti_name ->
      Printf.bprintf oc "<li><a href=\"%s-%s.html\">%s</a></li>\n" st.ocaml_version ti_name ti_name;
    ) !with_missing_constraints;
    Printf.bprintf oc "</ul></p>\n";

  ()


let make_version_report version results =

  let st = {
    ocaml_version = version;
    total_nversions_incorrect = 0;
    total_npackages = 0;
    packages_correct = StringMap.empty;
    packages_partial = StringMap.empty;
    packages_unavailable = StringMap.empty;
    total_nversions = 0;
    total_nversions_correct = 0;
    total_packages = StringMap.empty;

    version_unsat_constraints = StringMap.empty;
  } in

  let get_pkg_stats ti_name =
    try
      StringMap.find ti_name st.total_packages
    with Not_found ->
      let pkg = {
        pkg_name = ti_name;
        pkg_correct = StringMap.empty;
        pkg_incorrect = StringMap.empty;
      } in
      st.total_packages <- StringMap.add ti_name pkg st.total_packages;
      st.total_npackages <- st.total_npackages + 1;
      pkg
  in
  List.iter (fun p ->
    let ti = p.package in
    let pkg = get_pkg_stats ti.ti_name in
    st.total_nversions <- st.total_nversions + 1;
    match p.broken with
    | [] ->
      pkg.pkg_correct <- StringMap.add ti.ti_version p pkg.pkg_correct;
      st.total_nversions_correct <- st.total_nversions_correct + 1
    | _ ->
      pkg.pkg_incorrect <- StringMap.add ti.ti_version p pkg.pkg_incorrect;
      st.total_nversions_incorrect <- st.total_nversions_incorrect + 1
  ) results.packages;

  StringMap.iter (fun ti_name pkg ->
    if pkg.pkg_incorrect = StringMap.empty then begin
      st.packages_correct <-
        StringMap.add ti_name pkg st.packages_correct
    end else
    if pkg.pkg_correct = StringMap.empty then begin
      st.packages_unavailable <- StringMap.add ti_name pkg
          st.packages_unavailable
    end else begin
      st.packages_partial <- StringMap.add ti_name pkg
          st.packages_partial
    end
  ) st.total_packages;


  let reporthtml = Filename.concat reporthtmldir
      (Printf.sprintf "report-%s.html" version) in
  make_report reporthtml results st;

  st

let _ =
  (try Unix.mkdir html_dir 0o755 with _ -> ());
  (try Unix.mkdir reporthtmldir 0o755 with _ -> ());

  let oc = open_out (Filename.concat reporthtmldir "report.txt") in
  Printf.fprintf oc "repo:%s\n" repo_version;
  Printf.fprintf oc "date:%.f\n" time;
  Printf.fprintf oc "dir:%s\n" reportdir;
  Array.iter (fun st ->
    let results = load_bin_report (Filename.concat reportdir
        (Printf.sprintf "%s-all-1.bin" st)) in

    let _st = make_version_report st results in
    List.iter (fun p ->
      Printf.fprintf oc "%s:%s:%s:%s\n"
        (match p.broken with
         [] -> "ok"
         | _ -> "bad")
        st
        p.package.ti_name
        p.package.ti_version
    ) results.packages;
  ) versions;
  close_out oc;

  generate_version_pages ();

  Html.end_pages ();

  let index_html = Filename.concat html_dir "index.html" in
  let new_html = index_html ^ ".new" in
  let oc = open_out new_html in
  let begin_file = string_of_file (Filename.concat templates_dir "index.header") in
  let end_file = string_of_file (Filename.concat templates_dir "index.trailer") in

  output_string oc begin_file;
  let tm = Unix.gmtime time in
  Printf.fprintf oc "<h3>Latest report generated on %s</h3>" (string_of_date tm);
  Printf.fprintf oc "<ul>\n";
  Array.iter (fun version ->
    Printf.fprintf oc "<li><a href=\"%s/report-%s.html\">For version %s</a></li>\n" reportdir version version;
  ) versions;
  Printf.fprintf oc "<li><a href=\"table.html\">Summary table</a></li>\n";
  Printf.fprintf oc "</ul>\n";

  output_string oc end_file;

  close_out oc;

  (try Sys.remove index_html with _ -> ());
  Sys.rename new_html index_html;
 *)
