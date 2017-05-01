(*******************************************************************)
(*                                                                 *)
(*   Copyright (C) 2014, OCamlPro SAS & INRIA                      *)
(*   Fabrice Le Fessant                                            *)
(*                                                                 *)
(*******************************************************************)

open WeatherTypes
open WeatherMisc


(*
let declare_package pkg =
  let version = Cudf.lookup_typed_package_property pkg "opam-version" in
  let version = Cudf_types_pp.string_of_value version in
  Hashtbl.add declared_versions (pkg.Cudf.package, pkg.Cudf.version) version;
  ()
*)

let declared_version s p v =
  (* Printf.eprintf "declared_version %S %d\n%!" p v; *)
  try
    let p = Cudf.lookup_package s.universe (p,v) in
    Cudf.lookup_package_property p "opam-version"
  with e ->
    Printf.eprintf "Version (%s,%d) not found\n%!" p v;
    raise e

(* decode using x-www-form-urlencoded form *)

let digit_hexa x =
  match x with
  | 'a' .. 'f' -> (Char.code x) + 10 - (Char.code 'a')
  | 'A' .. 'F' -> (Char.code x) + 10 - (Char.code 'A')
  | '0' .. '9' -> (Char.code x) - (Char.code '0')
  | _ -> failwith "Not an hexa number (encode.ml)"

let decode s =
  let len = String.length s in
  let r = Buffer.create len in
  let rec iter i =
    if i < len then
      match s.[i] with
      | '+' -> Buffer.add_char r  ' '; iter (i+1)
      | '%' ->
          let n =
            try
              let fst = digit_hexa s.[i+1] in
              let snd = digit_hexa s.[i+2] in
              Buffer.add_char r (char_of_int (fst*16 + snd));
              3
            with _ ->
                Buffer.add_char r '%';
                1
          in
          iter (i+n)

      | c -> Buffer.add_char r c; iter (i+1)
  in
  iter 0;
  Buffer.contents r

let encode s =
  let pos = ref 0 in
  let len = String.length s in
  let res = String.create (3*len) in
  let hexa_digit x =
    if x >= 10 then Char.chr (Char.code 'a' + x - 10)
    else Char.chr (Char.code '0' + x) in
  for i=0 to len-1 do
    match s.[i] with
    | 'a'..'z' | 'A'..'Z' | '0'..'9' | '-' | '+'(* | '.' | '-' | '*' | '_' *) ->
        res.[!pos] <- s.[i]; incr pos
(*    | ' ' -> res.[!pos] <- '+'; incr pos *)
    | c ->
        res.[!pos] <- '%';
        res.[!pos+1] <- hexa_digit (Char.code c / 16);
        res.[!pos+2] <- hexa_digit (Char.code c mod 16);
        pos := !pos + 3
  done;
  String.sub res 0 !pos

let load_cudf_universe cudf_file =
  let _preamble, universe, _req = Cudf_parser.load_from_file cudf_file in
  universe

(* Reads the OPAM cudf-file #v2v annotations and re-adds dummy version
   of the corresponding packages with the *)
let add_unav_packages universe filename =
  Printf.eprintf "load_cudf_version_map %s\n%!" filename;
  let ic = open_in filename in
  try
    while true do
      let line = input_line ic in
      try
        Scanf.sscanf line "#v2v:%s@:%d=%s"
          (fun opam_name cudf_version opam_version ->
             let cudf_name = Common.CudfAdd.encode opam_name in
             if not (Cudf.mem_package universe (cudf_name, cudf_version)) then
               Cudf.add_package universe {
                 Cudf.default_package with
                 Cudf.package = cudf_name;
                 Cudf.version = cudf_version;
                 Cudf.pkg_extra = [
                   "opam-name", `String opam_name;
                   "opam-version", `String opam_version;
                 ]})
      with Scanf.Scan_failure _ | End_of_file -> ()
    done
  with End_of_file ->
    close_in ic

let string_of_relop = function
    `Eq -> "="
  | `Neq -> "!="
  | `Geq -> ">="
  | `Gt -> ">"
  | `Leq -> "<="
  | `Lt -> "<"

let string_of_constr g pkg = function
  | None -> "(*)"
  | Some (relop, vnum) ->
    Printf.sprintf "(%s %s)"
      (string_of_relop relop)
      (declared_version g pkg vnum)

let merge_constraints g vpkgs =
  match vpkgs with
  [] -> assert false
  | (pkg, constr) :: others ->
    pkg, constr :: List.map (fun (pkg2, constr2) ->
        if pkg2 <> pkg then begin
          Printf.eprintf "Unmergable_constraints %s %s and %s %s\n%!"
            pkg (string_of_constr g pkg constr)
            pkg2 (string_of_constr g pkg constr2);
          assert false
        end;
        constr2
      ) others



let initial_summary cudf =
  { packages = [];
    time = Unix.gmtime (Unix.gettimeofday ());
    cudf = Digest.file cudf;
    universe = Cudf.load_universe [];
  }
