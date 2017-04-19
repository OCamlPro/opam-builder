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
   * performs what "opam-builder weather" would do
   * try to build the package dependencies.
 *)

open StringCompat (* for StringMap *)
open CheckTypes
open CheckTypes.OP

let arg_build = ref true

let args =
  CommandWeather.args @
    [
      "--no-build", Arg.Clear arg_build,
      " Do not build packages (supposed already done)";
    ]

let build_packages () =

  let (st, c, stats) = CommandWeather.init_opam () in

  if !arg_build then
    CheckBuild.install_popular st c stats;

  (* load results in version_result, version_build and version_log *)

  StringMap.iter (fun _ p ->
      StringMap.iter (fun _ v ->
          let install_prefix =
            v.version_cache_dir //
              (Printf.sprintf "%s-%s-install"
                              v.version_name st.sw.sw_name) in
          let build_file = install_prefix ^ ".build" in
          let log_file = install_prefix ^ ".log" in
          let result_file = install_prefix ^ ".result" in
          v.version_result <- (
            try
              match FileString.read_file result_file with
              | "SUCCESS\n" -> Some true
              | "FAILURE\n" ->
                 v.version_log <-
                   (try Some (FileString.read_file log_file) with _ -> None);
                 Some false
              | _ ->
               let tmp_file = result_file ^ ".tmp" in
               (try Sys.remove tmp_file with _ -> ());
               (try Sys.rename result_file tmp_file with _ -> ());
               None
            with _ -> None);
          v.version_build <-
            (try Some (CheckTree.read_build_file build_file) with _ ->
               let tmp_file = build_file ^ ".tmp" in
               (try Sys.remove tmp_file with _ -> ());
               (try Sys.rename build_file tmp_file with _ -> ());
               None);
        ) p.package_versions;
    ) c.packages;

  CheckReport.report st c stats;

  let dump_file = st.dirs.report_dir //
                    (Printf.sprintf "%s-%s-%s.dump"
                                    c.timestamp_date
                                    c.commit_name c.switch) in

  CheckIO.save dump_file (c, stats);

  (st, c, stats)

let action args =
  ignore (build_packages () :
            CheckTypes.state * CheckTypes.commit * CheckTypes.stats)
