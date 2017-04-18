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

  StringMap.iter (fun package_name p ->
      match p.package_transitive_checksum with
      | None -> assert false
      | Some (checksum, closure) ->

         let package_dir = Filename.concat st.dirs.cache_dir package_name in
         if not (Sys.file_exists package_dir) then Unix.mkdir package_dir 0o775;

         CheckCudf.check_installability st checksum package_dir package_name;

         StringMap.iter (fun version_name v ->
             let version_dir = Filename.concat package_dir version_name in
             if not (Sys.file_exists version_dir) then Unix.mkdir version_dir 0o775;
             CheckCudf.check_installability st checksum version_dir version_name;
           ) p.package_versions;

    ) c.packages;

  (* Load all the dependencies into the commit record *)

  StringMap.iter (fun _ p ->
      let package_dir = Filename.concat st.dirs.cache_dir p.package_name in
      p.package_status <-
        Some (CheckCudf.status_of_files package_dir p.package_name c.switch);
      StringMap.iter (fun _ v ->
          let version_dir = Filename.concat package_dir v.version_name in
          v.version_status <-
            Some (CheckCudf.status_of_files version_dir v.version_name c.switch)
        ) p.package_versions;
    ) c.packages;

  let commit_file = st.dirs.report_dir //
                      (Printf.sprintf "%s-%s.weather" c.check_date
                                      c.commit_name)
  in

  let stats = CheckStats.compute_stats st c in


  CheckIO.save commit_file (c, stats);

  (st, c, stats)

let action args =
  ignore (init_opam () :
            CheckTypes.state * CheckTypes.commit * CheckTypes.stats )