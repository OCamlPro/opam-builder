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

open CheckTypes.V
open CheckTypes
open StringCompat
open CopamInstall

let solution_prefix version_dir version_name switch =
  Filename.concat version_dir
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

  Array.iter (fun sw ->

    let solution_prefix = solution_prefix version_dir version_name sw.sw_name in
    let deps_file = solution_prefix ^ ".deps" in
    let log_file = solution_prefix ^ ".log" in

    CheckUpdate.checksum_rule [deps_file; log_file] checksum (fun () ->

      Printf.eprintf "  checking installability of %s on %s\n%!" version_name sw.sw_name;

      let status, log_content =
        CopamInstall.check_install
          state.root sw.sw_cudf ~switch:sw.sw_name version_name in
      let deps_content = deps_of_status status in
      FileString.write_file log_file log_content;
      FileString.write_file deps_file deps_content;
    )
  ) state.sws;
  ()

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
