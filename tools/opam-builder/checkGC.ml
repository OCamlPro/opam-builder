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

type archive = {
  filename : string;
  mutable used : int;
  mutable occur : int;
}

let clean cache_dir switches =

  List.iter (fun switch ->
    Printf.eprintf "switch: %s\n%!" switch;
    let archives = Hashtbl.create 111111 in
    let register_archive version_dir file_base =
      let filename = Filename.concat version_dir file_base in
      try
        let a = Hashtbl.find archives filename in
        a.occur <- a.occur + 1
      with Not_found ->
        Hashtbl.add archives filename
          { filename; used = 0; occur = 1 }
    in

    let need_archive version_name hash =
      let package_name, _ = OcpString.cut_at version_name '.' in
      let file_base = Printf.sprintf "%s-%s" package_name switch in
      let filename = Filename.concat package_name
        (Filename.concat version_name file_base) in
      try
        let a = Hashtbl.find archives filename in
        a.used <- a.used + 1
      with Not_found ->
        Hashtbl.add archives filename
          { filename; used = 1; occur = 0 }
    in
    let read_build_file version_dir file =
      let filename = Filename.concat version_dir file in
      Printf.eprintf "  processing %s\n%!" filename;
      let version_name_ref = ref None in
      let hash_ref = ref None in
      FileString.iter_lines (fun line ->
        let segments = OcpString.split line ':' in
        match segments with
        | [ "begin" ; ("build" | "install") ; _time ; version_name ] ->
          version_name_ref := Some version_name
        | [ "hash" ; hash ] ->
          hash_ref := Some hash
        | [ "archive" ; "reused" ]
        | [ "archive" ; "created" ]
          ->
          begin
            match !version_name_ref, !hash_ref with
            | Some version_name, Some hash ->
              need_archive version_name hash
            | _ ->
              Printf.eprintf "  Error: version_name or hash missing\n%!"
          end
        | [ "end"; ( "build"|"install") ; _delay; version_name ] ->
          version_name_ref := None;
          hash_ref := None
        | _ -> ()
      ) filename
    in


    let clean_version package_name version_name version_dir =
      let files = Sys.readdir version_dir in

      Array.iter (fun file ->
        if Filename.check_suffix file ".build" then
          read_build_file version_dir file
        else
          if Filename.check_suffix file "-build.tar.gz" then
            register_archive version_dir (Filename.chop_suffix file ".tar.gz")
      ) files
    in

    let packages = Sys.readdir cache_dir in
    Array.iter (fun package_name ->
      let package_dir = Filename.concat cache_dir package_name in
      let versions = Sys.readdir package_dir in
      Array.iter (fun version_name ->
        let version_dir = Filename.concat package_dir version_name in
        if Sys.is_directory version_dir then
          clean_version package_name version_name version_dir
      ) versions;
      Printf.eprintf "%s done\n%!" package_name;
    ) packages;

    let day_secs = 3600 * 24 in
    let current_time = Unix.gettimeofday () in
    let nused = ref 0 in
    let nunused = ref 0 in
    let nremoved = ref 0 in
    Hashtbl.iter (fun _ a ->
      let base_archive = a.filename in
      let archive = base_archive ^ ".tar.gz" in
      let mtime =
        try
          (Unix.stat archive).Unix.st_mtime
        with _ -> current_time +. 100_000.
      in
      let ndays = int_of_float (current_time -. mtime) /  day_secs in
      Printf.eprintf "%s: %d uses %d occurs (%d days old).\n%!"
        a.filename
        a.used
        a.occur
        ndays;
      if a.used>0 then begin
        incr nused;
      end else begin
        incr nunused;
        if ndays > 7 then incr nremoved;
      end
    ) archives;
    Printf.eprintf "%d used archives\n%!" !nused;
    Printf.eprintf "%d unused archives\n%!" !nunused;
    Printf.eprintf "%d removed archives\n%!" !nremoved;

  ) switches;
  ()

(*

open CheckTypes.V
open CheckTypes
open StringCompat
open CopamInstall


let switch_archive cache_dir switch =
  Filename.concat cache_dir
    (Printf.sprintf "switch-%s.tar.gz" switch)

let switch_snapshot cache_dir switch =
  Filename.concat cache_dir
    (Printf.sprintf "switch-%s.snap" switch)

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

let opam_files_to_backup = ["environment"; "reinstall"; "state"]
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
  let builder_file = Filename.concat dirs.cache_dir "builder.txt" in
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

let check_build_and_install_version only_to_clean st c sw v =
  Printf.eprintf "check_build_and_install_version %s on %s\n%!"
    v.version_name sw.sw_name;
  let dirs = st.dirs in
  let p = v.version_package in
  let package_dir = Filename.concat dirs.cache_dir p.package_name in
  let version_dir = Filename.concat package_dir v.version_name in
  let install_prefix =
    Filename.concat version_dir
      (Printf.sprintf "%s-%s-install" v.version_name sw.sw_name) in
  let build_file = install_prefix ^ ".build" in
  let log_file = install_prefix ^ ".log" in
  let result_file = install_prefix ^ ".result" in

  let solution_deps =
    CheckCudf.solution_deps version_dir v.version_name sw.sw_name in

  match solution_deps with
  | NotAvailable | NotInstallable ->
    Printf.eprintf "Warning: package %s is not installable\n%!" v.version_name
  | ExternalError ->
    Printf.eprintf "Warning: package %s : ExternalError\n%!" v.version_name
  | Installable solution_deps ->
    let failures = ref [] in
    let b = Buffer.create 1024 in
    Buffer.add_string b sw.sw_name;
    List.iter (fun (package_name, version) ->
        let version_name = package_name ^ "." ^ version in
        let package_dir = Filename.concat dirs.cache_dir package_name in
        let version_dir = Filename.concat package_dir version_name in

        begin
          let install_prefix =
            Filename.concat version_dir
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
        let checksum_file = Filename.concat version_dir
            (version_name ^ ".lint.checksum") in
        let ic = open_in checksum_file in
        let checksum = input_line ic in
        close_in ic;
        Buffer.add_string b version_name;
        Buffer.add_string b checksum;
      ) solution_deps;
    let checksum = CheckDigest.buffer b in
    Printf.eprintf "Checksum to build would be %s\n%!"
      (CheckDigest.to_printable_string checksum);
    CheckUpdate.checksum_rule [result_file; log_file; build_file] checksum (fun () ->
        (* if [only_to_clean] is set, we want to completely remove
           files that have to be rebuilt. We do that because otherwise
           [failures] would be false, since it tries to re-use results
           of preceeding compilations.*)

        if only_to_clean then
          List.iter (fun file ->
              try Sys.remove file with _ -> ()
            ) [ result_file; log_file; build_file]
        else
          match !failures with
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
      );
    ()

let install_popular st c stats =
  let open CheckGraph in
  let dirs = st.dirs in
  let stats_file = Filename.concat dirs.report_dir
      (Printf.sprintf "%s.stats" c.commit_name) in
  let oc = open_out stats_file in
  Array.iter (fun { stats_switch;  stats_version; stats_version2;
                    stats_installable_versions; stats_installable_packages;
                    stats_uninstallable_versions; stats_uninstallable_packages;
                    stats_unavailable_versions; stats_unavailable_packages;
                    stats_error_versions; stats_error_packages;
                  } ->
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

             ) stats;
  close_out oc;
  Printf.eprintf "Stats saved\n%!";

  List.iter (fun only_to_clean ->

      Array.iter
        (fun
          { stats_switch;  stats_version; stats_version2;
            stats_installable_versions;
            stats_installable_packages } ->
          let len = Array.length stats_version2 in
          for i = 1 to len do
            let s = stats_version2.(len-i) in
            Printf.eprintf "checking %s %d\n%!"
              s.s_version.version_name s.s_used;
            if s.s_used > 1 then
              check_build_and_install_version only_to_clean st c stats_switch s.s_version
          done;
        ) stats;
      Array.iter
        (fun { stats_switch;  stats_version; stats_version2;
               stats_installable_versions;
               stats_installable_packages } ->
          let len = Array.length stats_version2 in
          for i = 1 to len do
            let s = stats_version2.(len-i) in
            Printf.eprintf "checking %s %d\n%!"
              s.s_version.version_name s.s_used;
            if s.s_used = 1 then
              check_build_and_install_version only_to_clean st c stats_switch s.s_version
          done;
        ) stats
    ) [true; false]

let init dirs switches =

    CopamIssues.init ();

    let switches =
      match switches with
      | [] -> [
        "3.12.1";
        "4.00.0";
        "4.01.0";
        "4.02.1";
        "4.02.3"
      ]
      | _ -> List.sort compare switches
    in

    let root = CopamInstall.init dirs.opam_dir switches in

    List.iter (fun switch ->
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
      end
    ) switches;
    let switches = Array.of_list switches in
    Array.iter (restore_switch_from_archive dirs) switches;
    let switches =
      Array.map (fun sw_name ->
        let snapshot_file = switch_snapshot dirs.cache_dir sw_name in
        let sw_dir = Filename.concat dirs.opam_dir sw_name in
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
        let sw_backup = MemoryBackup.save sw_dir opam_files_to_backup in


        let sw_cudf = ref None in
        { sw_snapshot; sw_name; sw_dir; sw_cudf; sw_backup }
      ) switches
    in
    let st = {
      root; sws = switches; dirs;
    } in
    st

let export_switch st c sw =
  let dirs = st.dirs in
  let date = CheckDate.TIMESTAMP.current () in
  let export_file = Filename.concat dirs.report_dir
      (Printf.sprintf "%s-%s-%s.export"
         date
         c.commit_name sw.sw_name) in

  let export_file_tmp = export_file ^ ".tmp" in
  let oc = open_out export_file_tmp in

  Printf.fprintf oc "commit:%s\n" c.commit_name;
  Printf.fprintf oc "switch:%s\n" sw.sw_name;
  Printf.fprintf oc "date:%f\n" (Unix.time());

  StringMap.iter (fun package_name p ->
      let package_dir = Filename.concat dirs.cache_dir p.package_name in
      Printf.fprintf oc "package:%s\n" package_name;
      let status = CheckCudf.solution_deps
          package_dir package_name sw.sw_name in
      begin
        match status with
        | NotInstallable ->
          Printf.fprintf oc "status:non-installable\n"
        | NotAvailable ->
          Printf.fprintf oc "status:non-available\n"
        | ExternalError ->
          Printf.fprintf oc "status:external-error\n"
        | Installable deps ->
          Printf.fprintf oc "status:installable\n";
          Printf.fprintf oc "deps:%s\n"
            (String.concat ","
               (List.map (fun (n,v) -> Printf.sprintf "%s.%s" n v) deps))
      end;
      StringMap.iter (fun version_name v ->
          Printf.fprintf oc "version:%s\n" version_name;

          let version_dir = Filename.concat package_dir v.version_name in
          let status = CheckCudf.solution_deps
              version_dir version_name sw.sw_name in
          begin
            match status with
            | NotInstallable ->
              Printf.fprintf oc "status:non-installable\n"
            | NotAvailable ->
              Printf.fprintf oc "status:non-available\n"
            | ExternalError ->
              Printf.fprintf oc "status:builder-error\n"
            | Installable deps ->
              Printf.fprintf oc "status:installable\n";
              Printf.fprintf oc "deps:%s\n"
                (String.concat ","
                   (List.map (fun (n,v) -> Printf.sprintf "%s.%s" n v) deps));
              let install_prefix =
                Filename.concat version_dir
                  (Printf.sprintf "%s-%s-install" v.version_name sw.sw_name) in
              let build_file = install_prefix ^ ".build" in
              let log_file = install_prefix ^ ".log" in
              let result_file = install_prefix ^ ".result" in
              try
                match FileString.read_file result_file with
                | "SUCCESS\n" ->
                  Printf.fprintf oc "status:success\n";
                  let build_content = FileString.read_file build_file in
                  Printf.fprintf oc "begin-build:true\n%s\nbegin-build:false\n"
                    build_content;

                | "FAILURE\n" ->
                  let log_content = FileString.read_file log_file in
                  let build_content = FileString.read_file build_file in
                  Printf.fprintf oc "status:failure\n";
                  Printf.fprintf oc "begin-build:true\n%s\nbegin-build:false\n"
                    build_content;
                  Printf.fprintf oc "begin-log:true\n%s\nbegin-log:false\n"
                    log_content;
                | _ -> raise Exit
              with _ ->
                Printf.fprintf oc "status:builder-error\n";
          end;

        ) p.package_versions;
    ) c.packages;
  Printf.fprintf oc "export:end\n";
  close_out oc;
  Sys.rename export_file_tmp export_file

let report_switch st c sw =
  let dirs = st.dirs in
  let report_file = Filename.concat dirs.report_dir
      (Printf.sprintf "%s-build-%s.html"
         c.commit_name sw.sw_name) in




  let npackages = ref 0 in
  let nversions = ref 0 in
  let nunavailable_versions = ref 0 in
  let nbroken_versions = ref 0 in
  let nfailed_versions = ref 0 in
  let nsuccess_versions = ref 0 in
  let nerror_versions = ref 0 in

  StringMap.iter (fun package_name p ->
      incr npackages;
      let package_dir = Filename.concat dirs.cache_dir p.package_name in
      StringMap.iter (fun version_name v ->
          incr nversions;
          let version_dir = Filename.concat package_dir v.version_name in
          let status = CheckCudf.solution_deps version_dir version_name sw.sw_name in
          begin
            match status with
            | NotInstallable -> incr nbroken_versions
            | NotAvailable -> incr nunavailable_versions
            | ExternalError -> incr nerror_versions
            | Installable deps ->
              let install_prefix =
                Filename.concat version_dir
                  (Printf.sprintf "%s-%s-install" v.version_name sw.sw_name) in
              let _build_file = install_prefix ^ ".build" in
              let _log_file = install_prefix ^ ".log" in
              let result_file = install_prefix ^ ".result" in
              try
                match FileString.read_file result_file with
                | "SUCCESS\n" -> incr nsuccess_versions
                | "FAILURE\n" -> incr nfailed_versions
                | _ -> raise Exit
              with _ ->
                incr nerror_versions
          end;
        ) p.package_versions;
    ) c.packages;







  let oc = open_out report_file in
  Printf.fprintf oc "<h1>Commit %s, Switch %s</h1>\n" c.commit_name sw.sw_name;

  Printf.fprintf oc "<pre>\n";
  Printf.fprintf oc "Num packages: %d\n" !npackages;
  Printf.fprintf oc "Num versions: %d\n" !nversions;
  Printf.fprintf oc "Num unavailable versions: %6d (version not available for this OCaml version)\n" !nunavailable_versions;
  Printf.fprintf oc "Num broken versions:      %6d (broken dependencies)\n" !nbroken_versions;
  Printf.fprintf oc "Num failed versions:      %6d (compilation failed)\n" !nfailed_versions;
  Printf.fprintf oc "Num success versions:     %6d (installation succeeded)\n" !nsuccess_versions;
  Printf.fprintf oc "Num error versions:       %6d (builder internal error)\n" !nerror_versions;
  Printf.fprintf oc "</pre>\n";

  Printf.fprintf oc "<table>\n";
  Printf.fprintf oc "<tr>\n";
  Printf.fprintf oc "  <td>Package</td>\n";
  Printf.fprintf oc "  <td>OCaml %s</td>\n" sw.sw_name;
  Printf.fprintf oc "</tr>\n";

  StringMap.iter (fun package_name p ->
      let package_dir = Filename.concat dirs.cache_dir p.package_name in

      Printf.fprintf oc "<tr>\n";
      Printf.fprintf oc "  <td>%s</td>\n" package_name;
      Printf.fprintf oc "  <td></td>\n";
      Printf.fprintf oc "</tr>\n";

      StringMap.iter (fun version_name v ->
          Printf.fprintf oc "<tr>\n";
          Printf.fprintf oc "  <td><a href=\"http://github.com/ocaml/opam-repository/tree/master/packages/%s/%s/opam\">%s</a></td>\n"
            package_name version_name version_name;

          let version_dir = Filename.concat package_dir v.version_name in
          let status = CheckCudf.solution_deps version_dir version_name sw.sw_name in
          begin
            match status with
            | NotInstallable ->
              Printf.fprintf oc "  <td style=\"background-color: red;\">BROKEN</td>\n"
            | NotAvailable ->
              Printf.fprintf oc "  <td style=\"background-color: white;\"></td>\n"
            | ExternalError ->
              Printf.fprintf oc "  <td style=\"background-color: yellow;\"></td>\n"
            | Installable deps ->
              let install_prefix =
                Filename.concat version_dir
                  (Printf.sprintf "%s-%s-install" v.version_name sw.sw_name) in
              let _build_file = install_prefix ^ ".build" in
              let _log_file = install_prefix ^ ".log" in
              let result_file = install_prefix ^ ".result" in
              try
                match FileString.read_file result_file with
                | "SUCCESS\n" ->
                  Printf.fprintf oc "  <td style=\"background-color: green;\"></td>\n";
                | "FAILURE\n" ->
                  Printf.fprintf oc "  <td style=\"background-color: orange;\"><a href=\"#%s\">FAILURE</a></td>\n" v.version_name;
                | _ -> raise Exit
              with _ ->
                Printf.fprintf oc "  <td style=\"background-color: orange;\">???</td>\n";
          end;

          Printf.fprintf oc "</tr>\n";

        ) p.package_versions;
    ) c.packages;

  Printf.fprintf oc "</table>\n";

  let version_header p v =
    Printf.fprintf oc "<a name=\"%s\">\n" v.version_name;
    Printf.fprintf oc "<h2><a href=\"http://github.com/ocaml/opam-repository/tree/master/packages/%s/%s/opam\">%s</a></h2>\n"
      p.package_name v.version_name v.version_name;
    Printf.fprintf oc "</a>\n";
  in

  StringMap.iter (fun package_name p ->
      let package_dir = Filename.concat dirs.cache_dir p.package_name in

      StringMap.iter (fun version_name v ->

          let version_dir = Filename.concat package_dir v.version_name in
          let status = CheckCudf.solution_deps version_dir version_name sw.sw_name in
          begin
            match status with
            | NotAvailable -> ()
            | ExternalError -> ()
            | NotInstallable ->
              version_header p v
            | Installable deps ->
              let install_prefix =
                Filename.concat version_dir
                  (Printf.sprintf "%s-%s-install" v.version_name sw.sw_name) in
              let build_file = install_prefix ^ ".build" in
              let log_file = install_prefix ^ ".log" in
              let result_file = install_prefix ^ ".result" in
              try
                match FileString.read_file result_file with
                | "SUCCESS\n" -> ()
                | "FAILURE\n" ->
                  version_header p v;
                  Printf.fprintf oc "<pre>%s</pre>"
                    (FileString.read_file build_file);
                  Printf.fprintf oc "<pre>%s</pre>"
                    (FileString.read_file log_file);
                | _ -> raise Exit
              with _ ->
                ()
          end;

          Printf.fprintf oc "</tr>\n";

        ) p.package_versions;
    ) c.packages;



  ()

let report st c stats =
  Array.iter (fun sw ->
    report_switch st c sw
  ) st.sws

let export st c stats =
  Array.iter (fun sw ->
    export_switch st c sw
  ) st.sws

*)
