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

(* This command :
   * checks that it is called in the switch directory
   * checks that the environement is clean (no ocaml in PATH, no env vars)
   * restores the opam switch (or backup it, the first time)
   * scan the packages and versions
   *)

open CheckTypes
open CheckTypes.OP

let arg_commit = ref None
let arg_repo_upgrade = ref true

let args = [
    "--commit", Arg.String (fun s -> arg_commit := Some s),
    "COMMIT Set commit name";

    "--no-repo-upgrade", Arg.Clear arg_repo_upgrade,
    " Do not upgrade opam repo to 2.0 (supposed already done)";
  ]


let last_commit_file = "last-commit.txt"
let last_commit_cmd =
  Printf.sprintf "git rev-parse --short HEAD > %s" last_commit_file

let command cmd =
  let exit = Sys.command cmd in
  if exit <> 0 then begin
    Printf.eprintf "Error: command failed with exit status %d:\n   %s\n%!"
      exit cmd;
    false
  end else true

let get_last_commit () =
  if command last_commit_cmd then begin
      let ic = open_in last_commit_file in
      let commit = input_line ic in
      close_in ic;
      Sys.remove last_commit_file;
      commit

    end else "unknown"


let dirs =
  let repo_subdir = "2.0" in
  let current_dir = CheckTree.current_dir in

  let cache_dir = CheckTree.cache_dir_fullname in
  let repo_dir = "." in
  let opam_dir = current_dir // ".opam" in
  let report_dir = CheckTree.reports_dir_fullname in

  {
    repo_dir; cache_dir; opam_dir; current_dir; report_dir; repo_subdir;
  }

let check_env () =
  List.iter (fun var ->
      try
        let s = Sys.getenv var in
        CheckTree.fatal "env variable %s is set to %S" var s
      with Not_found -> ()
    ) [ "OCAMLPARAM"; "OCAMLRUNPARAM"; "OCAMLLIB"; "CAML_LD_LIBRARY_PATH";
        "OPAMROOT" ];
  Unix.putenv "OCAMLRUNPARAM" "b=1"

let init_commit st commit =

  let dirs = st.dirs in

  let sw = st.sw in
  let switch = sw.sw_name in

  (* universe must have changed for every switch *)
  sw.sw_cudf.cudf_backup := None;
  Hashtbl.clear sw.sw_cudf.solver_cache;
  sw.sw_cudf.known_universe <- None;

  let c = CheckUpdate.check_commit ~lint:false ~commit dirs ~switch in
  c

let init_switch switch =
  (* Check that opam is locally installed in .opam with the
         current switch *)
  let st = CheckBuild.init dirs switch in
  st


let init_repo () =

  let switch = CheckTree.read_switch () in

  CopamInstall.opam_command := "opam.dev";

  if !arg_repo_upgrade then CheckBuild.upgrade_opam2 "2.0";

  let st = init_switch switch in
  let commit = match !arg_commit with
    | None -> get_last_commit ()
    | Some commit -> commit in
  let c = init_commit st commit in
  st, c

let action args =
  CheckTree.check_in_tree ();
  check_env ();
  ignore (init_repo () : CheckTypes.state * CheckTypes.commit)
