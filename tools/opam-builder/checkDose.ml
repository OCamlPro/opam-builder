(**************************************************************************)
(*                                                                        *)
(*              OCamlPro-Inria-Irill Attribution AGPL                     *)
(*                                                                        *)
(*   Copyright OCamlPro-Inria-Irill 2011-2016. All rights reserved.       *)
(*   This file is distributed under the terms of the AGPL v3.0            *)
(*   (GNU Affero General Public Licence version 3.0) with                 *)
(*   a special OCamlPro-Inria-Irill attribution exception.                *)
(*                                                                        *)
(*     Contact: <typerex@ocamlpro.com> (http://www.ocamlpro.com/)         *)
(*                                                                        *)
(*  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,       *)
(*  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES       *)
(*  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND              *)
(*  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS   *)
(*  BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN    *)
(*  ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN     *)
(*  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE      *)
(*  SOFTWARE.                                                             *)
(**************************************************************************)






open StringCompat
open Algo.Diagnostic

let cudf2opam cpkg =
  Cudf.lookup_package_property cpkg "opam-name",
  Cudf.lookup_package_property cpkg "opam-version"


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


let vpkg2opam cudf2opam cudf_universe vpkg =
  match vpkg2atom cudf2opam cudf_universe vpkg with
  | p, None -> p, None
  | p, Some (relop,v) -> p, Some (relop, v)





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

  let root_pkg, new_reasons = convert_error cudf2opam pkg univ reasons in

  let not_available =
    List.for_all
      (function
       | UnsatConstraint
         { ts_needed_by =
             [ { ti_name = "ocaml";
                 ti_needed_by = [
                     { ts_needed_by = [ti]} ] } ]
         } when ti == root_pkg
         -> true
       |  _ -> false
      ) new_reasons
  in

  let b = Buffer.create 1000 in
  List.iter (fun r -> buffer_reason b root_pkg r) new_reasons;
  let s1 = Buffer.contents b in

  let s2 = string_of_reasons namelink univ reasons in


  not_available, s1 ^ s2

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
