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
open CopamInstall.TYPES
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

  let rec iter may_restart =
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
                | "SUCCESS\n" ->
                   Some true
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

    (* Sometimes, a package will not built for external reasons. In
this case, some other packages will be skipped because of that
failure.  However, opam-builder might later detect it, and restart a
finally successful build for the initial package, but unfortunately,
since the dependencies have not changed, the other packages will not
be rebuilt and keep believing that there is still a problem.

Here, we try to detect this case, and restart the build (only once)
after deleting the wrong result files. *)

    let inconsistencies_detected = ref StringMap.empty in
    StringMap.iter (fun _ p ->
        StringMap.iter (fun _ v ->
            match v.version_build with
            | None -> ()
            | Some steps ->
               List.iter (fun (dep, action) ->
                   match action with
                   | DisabledFailed ->
                      begin
                        try
                          let vdep = StringMap.find dep c.versions in
                          match vdep.version_result with
                          | Some true ->
                             Printf.eprintf
                               "Inconsistency: %S skipped, but dep %S is ok\n%!"
                               v.version_name vdep.version_name;

                             inconsistencies_detected :=
                               StringMap.add v.version_name v
                                             !inconsistencies_detected;

                          | _ -> ()
                        with Not_found ->
                          Printf.eprintf
                            "CommandBuild Error: build dep %S of %S does not exist !!\n%!"
                            dep v.version_name
                      end

                   | _ -> ()
                 ) steps

          ) p.package_versions;
      ) c.packages;

    if not (StringMap.is_empty !inconsistencies_detected) && may_restart
    then begin

        Printf.eprintf "Restarting whole build !!!\n%!";
        StringMap.iter (fun _ v ->
            let install_prefix =
              v.version_cache_dir //
                (Printf.sprintf "%s-%s-install"
                                v.version_name st.sw.sw_name) in
            let result_file = install_prefix ^ ".result" in
            (try Sys.remove result_file with _ ->());
          ) !inconsistencies_detected;
        iter false

      end else begin

        CheckReport.report st c stats;
        let dump_file = st.dirs.report_dir //
                          (Printf.sprintf "%s-%s-%s.dump"
                                          c.timestamp_date
                                          c.commit_name c.switch) in

        CheckIO.save dump_file (c, stats);
        Printf.eprintf "Generated file %S\n%!" dump_file;
        (st, c, stats)

      end

  in
  iter true

let action args =
  CheckTree.check_in_tree ();
  CommandScan.check_env ();
  ignore (build_packages () :
            CheckTypes.state * CheckTypes.commit * CheckTypes.stats)
