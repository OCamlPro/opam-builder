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

let arg_commit = ref "manual-call"
let arg_repo_upgrade = ref true

let args = [
    "--commit", Arg.String ((:=) arg_commit),
    "COMMIT Set commit name";

    "--no-repo-upgrade", Arg.Clear arg_repo_upgrade,
    " Do not upgrade opam repo to 2.0 (supposed already done)";
  ]

let dirs =
  let repo_subdir = "2.0" in
  let current_dir = CheckTree.current_dir in

  let cache_dir = CheckTree.cache_dir_fullname in
  let repo_dir = "." in
  let opam_dir = Filename.concat current_dir ".opam" in
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
  CheckTree.check_in_tree ();
  check_env ();

  let switch = CheckTree.read_switch () in

  CopamInstall.opam_command := "opam.dev";

  if !arg_repo_upgrade then CheckBuild.upgrade_opam2 "2.0";

  let st = init_switch switch in
  let c = init_commit st !arg_commit in
  st, c

let action args =
  ignore (init_repo () : CheckTypes.state * CheckTypes.commit)
