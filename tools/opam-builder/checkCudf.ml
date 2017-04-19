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



(*

  Naming conventions for short variables:
  * p : the package
  * v : the version of the package p (often p.package_version)
  * c : the commit
  * st : the state
  * dirs : the directories (often st.dirs)
*)

open StringCompat

open CheckTypes
open CheckTypes.OP
open CopamInstall.TYPES

let solution_prefix version_dir version_name switch =
  version_dir //
    (Printf.sprintf "%s-%s-solution" version_name switch)

let deps_of_status status =
  try
    match status with
    | NotInstallable -> "no-solution\n"
    | NotAvailable -> "not-available\n"
    | ExternalError -> "external-error\n"
    | Installable packages ->
      let b = Buffer.create 1024 in
      List.iter (fun (p,v) ->
        Printf.bprintf b "%s.%s\n" p v
      ) packages;
      Buffer.contents b
  with _ ->
    "external-error\n"

exception Deps of status
let status_of_deps deps_file =
  try
    let packages = ref [] in
    FileString.iter_lines (fun line ->
      match line with
      | "no-solution" -> raise (Deps NotInstallable)
      | "not-available" -> raise (Deps NotAvailable)
      | "un-parsable" -> raise (Deps ExternalError)
      | s ->
        let package, version = OcpString.cut_at s '.' in
        if version = "" || package = "" then
          raise (Deps ExternalError);
        packages := (package, version) :: !packages;
    ) deps_file;
    Installable (List.sort compare !packages)

  with Deps s -> s

let solution_deps version_dir version_name sw_name =
  let solution_prefix = solution_prefix version_dir version_name sw_name in
  status_of_deps (solution_prefix ^ ".deps")

let check_installability state checksum version_dir version_name =

  let sw = state.sw in

  let solution_prefix = solution_prefix version_dir version_name sw.sw_name in
  let deps_file = solution_prefix ^ ".deps" in
  let log_file = solution_prefix ^ ".log" in

  if Sys.file_exists deps_file then begin
      let ic = open_in deps_file in
      let line = input_line ic in
      close_in ic;
      if line = "external-error" then Sys.remove deps_file
    end;


  CheckUpdate.checksum_rule
    [deps_file; log_file]
    checksum (fun () ->
      Printf.eprintf "  checking installability of %s on %s\n%!"
                     version_name sw.sw_name;

      try
        let universe =
          match sw.sw_cudf.known_universe with
          | Some universe -> universe
          | None ->
             match ! (sw.sw_cudf.cudf_backup) with
             | None -> raise Not_found
             | Some cudf ->
                let open WeatherTypes in
                Printf.eprintf "  checking whole universe with DOSE...\n%!";

                let cudf_file = Filename.temp_file "cudf" ".cudf" in
                CopamCudf.write_file cudf_file cudf;
                let universe = WeatherDiag.load_cudf_universe cudf_file in
                sw.sw_cudf.known_universe <- Some universe;
                let callback diag =
                  let open Algo.Diagnostic in
                  let package = match diag.request with
                    | [p] -> p
                    | _ -> assert false
                  in
                  let name,version =
                    CopamCudf.cudf2opam cudf
                                        package.Cudf.package
                                        package.Cudf.version
                  in
                  let version_name = name ^ "." ^ version in
                  let _package_info = match diag.result with
                    | Success _ -> ()
                    | Failure f ->
                       let reasons = f () in
                       Hashtbl.add sw.sw_cudf.solver_cache version_name
                                   (package,reasons)
                  in
                  ()
                in

                let _nfail =
                  Algo.Depsolver.univcheck ~global_constraints:[] ~callback universe
                in
                (* The unavailable packages must be absent to perform
     the checks, but we re-add dummy versions now for easier lookups
     during reports generation. *)
                WeatherDiag.add_unav_packages universe cudf_file;
                universe
        in
        let (package,reasons) = Hashtbl.find sw.sw_cudf.solver_cache version_name in
        Printf.eprintf "  %s is in cache, not installable...\n%!"
                       version_name;
        let reasons = WeatherReasons.string_of_reasons
                        (fun name -> name)
                        package universe reasons in
        FileString.write_file log_file reasons;
        FileString.write_file deps_file "no-solution";
      with Not_found ->
        Printf.eprintf "  %s not in cache, should be installable...\n%!"
                       version_name;

        let status, log_content =
          CopamInstall.check_install
            state.root sw.sw_cudf.cudf_backup ~switch:sw.sw_name version_name in
        let deps_content = deps_of_status status in
        FileString.write_file log_file log_content;
        FileString.write_file deps_file deps_content;
        Printf.eprintf "     %s: after check, %s\n%!"
                       version_name
                       (match status with
                        | NotAvailable -> "NotAvailable"
                        | Installable _ -> "Installable"
                        | ExternalError -> "ExternalError"
                        | NotInstallable -> "NotInstallable"
                       )
    )


let status_of_files version_dir basename switch =
  let solution_prefix = solution_prefix version_dir basename switch in
  let deps_file = solution_prefix ^ ".deps" in
  let log_file = solution_prefix ^ ".log" in

  let s_status = status_of_deps deps_file in

  let s_log =
    try
      Some (FileString.read_file log_file)
    with _ -> None
  in
  { s_status; s_log }
