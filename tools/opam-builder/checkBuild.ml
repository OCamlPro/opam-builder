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
open CheckTypes
open CheckTypes.OP
open CopamInstall.TYPES

let switch_archive cache_dir switch =
  cache_dir // (Printf.sprintf "switch-%s.tar.gz" switch)

let switch_snapshot cache_dir switch =
  cache_dir // (Printf.sprintf "switch-%s.snap" switch)

let chdir dir =
  Printf.eprintf "cd %s\n%!" dir;
  Unix.chdir dir

let command cmd =
  let exit = Sys.command cmd in
  if exit <> 0 then begin
    Printf.eprintf "Error: command failed with exit status %d:\n   %s\n%!"
      exit cmd;
    false
  end else true

let ignore_bool (b: bool) = ()

let restore_switch_from_archive dirs switch =
  Printf.eprintf "Restoring switch %s from archive...\n%!" switch;

  chdir dirs.opam_dir;
  let switch_archive = switch_archive dirs.cache_dir switch in
  ignore_bool (Printf.kprintf command "rm -rf %s" switch);
  ignore_bool (Printf.kprintf command "tar zxf %s" switch_archive);
  chdir dirs.current_dir;

  Printf.eprintf "Restoring switch %s done\n%!" switch;
  ()

let opam_files_to_backup = ["environment"; "switch-state"]
let ignore_opam_files = StringSet.empty

let restore_switch st sw =
  try
    CheckSnapshot.clean_files sw.sw_dir
      ignore_opam_files sw.sw_snapshot
  with
  | CheckSnapshot.FileRemoved file ->
    Printf.eprintf "FileRemoved: %s\n%!" file;
    restore_switch_from_archive st sw.sw_name
  | CheckSnapshot.FileChanged file ->
    Printf.eprintf "FileChanged: %s\n%!" file;
    restore_switch_from_archive st sw.sw_name

let build_and_install st ~switch version =
  Printf.eprintf "build_and_install %s on %s\n%!" version switch;
  let dirs = st.dirs in
  let log_file = "check-install.log" in
  let builder_file = dirs.cache_dir // "builder.txt" in
  (try Sys.remove log_file with _ -> ());
  (try Sys.remove builder_file with _ -> ());
  let exit_code =
    let cmd = Printf.sprintf
      "OPAM_BUILD=%s %s --switch %s --yes --quiet %s > %s 2>&1"
      dirs.cache_dir
      (CopamInstall.opam_cmd st.root "install") switch version log_file in
    Printf.eprintf "cmd=%s\n%!" cmd;
    Sys.command cmd
  in
  let log_content = FileString.read_file log_file in
  let builder_content =
    try
      FileString.read_file builder_file with _ ->
        Printf.eprintf "Error: %s has no %s\n%!" version builder_file;
        "" in
  (try Sys.remove log_file with _ -> ());
  (try Sys.remove builder_file with _ -> ());
  (exit_code, log_content, builder_content)


let compute_solution_checksum st v solution_deps =
  let sw = st.sw in
  let dirs = st.dirs in
  let failures = ref [] in
  let b = Buffer.create 1024 in
  Buffer.add_string b sw.sw_name;
  List.iter (fun (package_name, version) ->
      let version_name = package_name ^ "." ^ version in
      let package_dir = dirs.cache_dir // package_name in
      let version_dir = package_dir // version_name in

      if version_name <> v.version_name then begin
          let install_prefix =
            version_dir //
              (Printf.sprintf "%s-%s-install" version_name sw.sw_name) in
          let result_file = install_prefix ^ ".result" in
          try
            match FileString.read_file result_file with
            | "SUCCESS\n" -> ()
            | "FAILURE\n" ->
               Printf.eprintf
                 "Compilation disabled because dependency %s failed before\n%!"
                 version_name;
               failures := version_name :: !failures
            | _ ->
               Printf.eprintf
                 "Warning: weird, dependency %s has a non parsable result file\n%!"
                 version_name
          with _ ->
            ()
        end;

      let checksum_file = version_dir //
                            (version_name ^ ".lint.checksum") in
      let checksum = CheckDigest.digest_of_file checksum_file in
      Buffer.add_string b version_name;
      CheckDigest.add_digest b checksum;
    ) solution_deps;
  CheckDigest.buffer b, !failures

let really_check_install failures st v only_to_clean
                         result_file log_file build_file
  =
  let sw = st.sw in
  let dirs = st.dirs in
  (* if [only_to_clean] is set, we want to completely remove
           files that have to be rebuilt. We do that because otherwise
           [failures] would be false, since it tries to re-use results
           of preceeding compilations.*)

  if only_to_clean then
    List.iter (fun file ->
        let backup_file = file ^ ".bak" in
        if Sys.file_exists backup_file &&
             Sys.file_exists file then
          (try Sys.remove backup_file with _ -> ());
        try Sys.rename file backup_file with _ -> ()
      ) [ result_file; log_file; build_file]
  else
    match failures with
    | [] ->
       let exit_code, log_content, build_content =
         build_and_install st ~switch:sw.sw_name v.version_name
       in
       (* TODO: we should check that the consistency of the dependencies
               found before and used here. *)
       FileString.write_file log_file log_content;
       FileString.write_file build_file build_content;
       let result = if exit_code = 0 then "SUCCESS\n" else "FAILURE\n" in
       FileString.write_file result_file result;
       Printf.eprintf "Compilation of %s on %s was a %s%!"
                      v.version_name sw.sw_name result;
       MemoryBackup.restore sw.sw_backup;
       restore_switch dirs sw;
       ()
    | failures ->
       let log_oc = open_out log_file in
       let build_oc = open_out build_file in
       List.iter (fun version ->
           Printf.fprintf build_oc "disabled:failed:%s\n" version;
           Printf.fprintf log_oc "Dependency %s could not be compiled in previous runs.\n" version;
         ) failures;
       Printf.fprintf log_oc "Disabling compilation of %s\n%!" v.version_name;
       Printf.fprintf build_oc "disabled:skip:%s\n" v.version_name;
       close_out build_oc;
       close_out log_oc;
       FileString.write_file result_file "FAILURE\n";
       ()

let check_installable_solution
      st
      v
      only_to_clean solution_deps =
  let sw = st.sw in
  let install_prefix =
    v.version_cache_dir //
                    (Printf.sprintf "%s-%s-install" v.version_name sw.sw_name)
  in
  let build_file = install_prefix ^ ".build" in
  let log_file = install_prefix ^ ".log" in
  let result_file = install_prefix ^ ".result" in
  let checksum, failures = compute_solution_checksum st v solution_deps in
  Printf.eprintf "Checksum to build would be %s\n%!"
                 (CheckDigest.to_printable_string checksum);

  (* Note: a build_file might be empty if:
     * opam failed to download the sources (server down, bad CRC)
     * there are no build/install instructions (base-* packages)
     In both cases, it doesn't really matter, no ?
   *)
  if CheckIO.getsize build_file = 0 then Sys.remove build_file;
  if CheckIO.getsize result_file = 0 then Sys.remove result_file;
  CheckUpdate.checksum_rule [result_file; log_file; build_file] checksum
                            (fun () ->
                              really_check_install failures st v only_to_clean
                                                   result_file log_file
                                                   build_file
                            );
  ()

let check_build_and_install_version only_to_clean st c sw v =
  Printf.eprintf "check_build_and_install_version %s on %s\n%!"
                 v.version_name sw.sw_name;

  let solution_deps =
    CheckCudf.solution_deps v.version_cache_dir v.version_name sw.sw_name in

  match solution_deps with
  | NotAvailable | NotInstallable ->
     Printf.eprintf "Warning: package %s is not installable\n%!" v.version_name
  | ExternalError ->
     Printf.eprintf "Warning: package %s : ExternalError\n%!" v.version_name
  | Installable solution_deps ->
     check_installable_solution
       st
      v
      only_to_clean solution_deps

let save_stats st c stats =
  let dirs = st.dirs in
  let stats_file = dirs.report_dir //
                                   (Printf.sprintf "%s.stats" c.commit_name) in
  let oc = open_out stats_file in
  let { stats_switch;  stats_version; stats_version2;
        stats_installable_versions; stats_installable_packages;
        stats_uninstallable_versions; stats_uninstallable_packages;
        stats_unavailable_versions; stats_unavailable_packages;
        stats_error_versions; stats_error_packages;
      } = stats in
  Printf.fprintf oc "Begin switch %s\n" stats_switch.sw_name;
  Array.iteri (fun i s ->
      Printf.fprintf oc "[%5d] %5d %5d %s\n%!"
                     i s.s_used_last s.s_used s.s_version.version_name
    ) stats_version2;

  if stats_installable_versions > 0 then begin
      let n1 = ref 0 in
      let n2 = ref 0 in
      Array.iteri (fun _ s ->
          n1 := !n1 + s.s_used;
          n2 := !n2 + s.s_used_last;
        ) stats_version2;
      Printf.fprintf oc "versions: need to build %d to check %d (%.2f mean)\n%!"
                     !n1 stats_installable_versions
                     (float !n1 /. float stats_installable_versions);
      Printf.fprintf oc "packages: need to build %d to check %d (%.2f mean)\n%!"
                     !n2 stats_installable_packages
                     (float !n2 /. float stats_installable_packages);

      Printf.fprintf oc "stats_installable_packages: %d\n"
                     stats_installable_packages;
      Printf.fprintf oc "stats_unavailable_packages: %d\n"
                     stats_unavailable_packages;
      Printf.fprintf oc "stats_uninstallable_packages: %d\n"
                     stats_uninstallable_packages;
      Printf.fprintf oc "stats_error_packages: %d\n"
                     stats_error_packages;
      Printf.fprintf oc "stats_installable_versions: %d\n"
                     stats_installable_versions;
      Printf.fprintf oc "stats_unavailable_versions: %d\n"
                     stats_unavailable_versions;
      Printf.fprintf oc "stats_uninstallable_versions: %d\n"
                     stats_uninstallable_versions;
      Printf.fprintf oc "stats_error_versions: %d\n"
                     stats_error_versions;
    end;
  Printf.fprintf oc "End switch %s\n%!" stats_switch.sw_name;

  close_out oc;
  Printf.eprintf "Stats saved\n%!";
  ()

let install_popular st c stats =

  List.iter (fun only_to_clean ->

      let
        { stats_switch;  stats_version; stats_version2;
          stats_installable_versions;
          stats_installable_packages } = stats in

      let len = Array.length stats_version in
      for i = 1 to len do
        let s = stats_version.(len-i) in
        Printf.eprintf "checking %s %d\n%!"
                       s.s_version.version_name s.s_used;
        check_build_and_install_version only_to_clean st c stats_switch s.s_version
      done;

      (*
      let len = Array.length stats_version2 in
      for i = 1 to len do
        let s = stats_version2.(len-i) in
        Printf.eprintf "checking %s %d\n%!"
                       s.s_version.version_name s.s_used;
        if s.s_used > 1 then
          check_build_and_install_version only_to_clean st c stats_switch s.s_version
      done;


      let len = Array.length stats_version2 in
      for i = 1 to len do
        let s = stats_version2.(len-i) in
        Printf.eprintf "checking %s %d\n%!"
                       s.s_version.version_name s.s_used;
        if s.s_used = 1 then
          check_build_and_install_version only_to_clean st c stats_switch s.s_version
      done;
       *)
    ) [true; false]

let init dirs switch =

  CopamIssues.init ();

  let root = CopamInstall.init
               ~repo_subdir: dirs.repo_subdir
               dirs.opam_dir switch in

  let switch_archive = switch_archive dirs.cache_dir switch in

  if not (Sys.file_exists switch_archive) then begin
      Printf.eprintf "Creating switch archive %s\n%!" switch_archive;
      chdir dirs.opam_dir;
      if Printf.kprintf Sys.command
                        "tar zcf %s %s" switch_archive switch <> 0 then begin
          Printf.eprintf "Error: could not archive switch %s\n%!" switch;
          exit 2
        end;
      chdir dirs.current_dir;
    end;

  restore_switch_from_archive dirs switch;

  let sw_name = switch in
  let snapshot_file = switch_snapshot dirs.cache_dir sw_name in
  let sw_dir = dirs.opam_dir // sw_name in
  let sw_snapshot =
    if not (Sys.file_exists snapshot_file) then begin
        Printf.eprintf "Creating switch snapshot %s\n%!" snapshot_file;
        restore_switch_from_archive dirs sw_name;
        let sn = CheckSnapshot.make sw_dir ignore_opam_files in
        CheckSnapshot.save snapshot_file sn;
        sn
      end else
      CheckSnapshot.load snapshot_file
  in
  let sw_state_dir = sw_dir // ".opam-switch" in
  let sw_backup = MemoryBackup.save sw_state_dir opam_files_to_backup in


  let sw_cudf = {
      cudf_backup = ref None;
      known_universe = None;
      solver_cache = Hashtbl.create 1111;
    } in
  let sw =
    { sw_snapshot; sw_name; sw_dir; sw_cudf; sw_backup }
  in
  let st = { root; sw; dirs; } in
  st

let upgrade_opam2 subdir =
  Printf.eprintf "Upgrading repository to 2.0...\n%!";
  ignore_bool
    (Printf.kprintf command "%s admin upgrade -m %s"
                    !CopamInstall.opam_command subdir);
  Printf.eprintf "Upgrading repository to 2.0...done\n%!"
