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
   * performs what "opam-builder scan" would do
   * computes the package dependencies.
   * loads them in the 'commit' record
   * save the 'commit' record in "builder.reports/DATE-COMMIT.weather"
 *)

open StringCompat (* for StringMap *)
open CheckTypes
open CheckTypes.OP

let arg_opam_update = ref true

let args =
  CommandScan.args @
    [
      "--no-opam-update", Arg.Clear arg_opam_update,
      " Do not update the opam switch (supposed already done)";

    ]

let init_opam () =

  let (st, c) = CommandScan.init_repo () in

  (* do "opam update" *)
  if !arg_opam_update then
    if not (CheckBuild.command (CopamInstall.opam_cmd st.root "update")) then
      CheckTree.fatal "opam update failed.";

  (* compute dependencies, calling aspcud or cudf if needed *)

  let _status, _log_content =
    CopamInstall.check_install
      st.root st.sw.sw_cudf.cudf_backup ~switch:st.sw.sw_name "base-unix" in

  StringMap.iter (fun package_name p ->
      match p.package_opam_closure_checksum with
      | None -> assert false
      | Some (checksum, closure) ->

         if not (Sys.file_exists p.package_cache_dir) then
           Unix.mkdir p.package_cache_dir 0o775;

         CheckCudf.check_installability st checksum
                                        p.package_cache_dir package_name;

         StringMap.iter (fun version_name v ->
             if not (Sys.file_exists v.version_cache_dir) then
               Unix.mkdir v.version_cache_dir 0o775;
             CheckCudf.check_installability st checksum
                                            v.version_cache_dir version_name;
           ) p.package_versions;

    ) c.packages;

  (* Load all the dependencies into the commit record *)

  StringMap.iter (fun _ p ->
      p.package_status <-
        Some (CheckCudf.status_of_files p.package_cache_dir
                                        p.package_name c.switch);
      StringMap.iter (fun _ v ->
          v.version_status <-
            Some (CheckCudf.status_of_files
                    v.version_cache_dir v.version_name c.switch)
        ) p.package_versions;
    ) c.packages;

  let commit_file =
    st.dirs.report_dir //
      (Printf.sprintf "%s-%s-%s.weather"
         c.timestamp_date
         c.commit_name c.switch) in

  let stats = CheckStats.compute_stats st c in


  CheckIO.save commit_file (c, stats);

  (st, c, stats)

let action args =
  CheckTree.check_in_tree ();
  CommandScan.check_env ();
  ignore (init_opam () :
            CheckTypes.state * CheckTypes.commit * CheckTypes.stats )
