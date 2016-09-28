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



open CheckTypes.V
open CheckTypes
open StringCompat
open CopamInstall

module IntSet = Set.Make(struct type t = int let compare = compare end)

let filtered_errors = List.fold_left (fun set error ->
  IntSet.add error set) IntSet.empty [
  37; (* Missing field 'dev-repo' *)
  42; (* The 'dev-repo' field doesn't use version control. You may use URLs of the form "git+https://" or a ".hg" or ".git" suffix *)
]

(* Import the .export files from a list of directories. *)

type state = {
  dirs : string list;
  mutable files : StringSet.t;
}

let init dirs = { files = StringSet.empty; dirs = dirs }


type status =
  | StatusNonInstallable
  | StatusNonAvailable
  | StatusBuilderError
  | StatusFailure of string list * string list * string list
  | StatusSuccess of string list * string list
  | StatusLint of (int * string) list * (int * string) list
  | StatusUnknown

type mode =
  | Full
  | Diff
  | Summary

type package_display = {
  mutable pd_display : bool;
  pd_name : string;
  pd_dir : string;
  mutable pd_versions : version_display list;
}

and version_display = {
  mutable vd_display : bool;
  vd_name : string;
  vd_dir : string;
  mutable vd_statuses : status_display list;
  vd_pd : package_display;
}

and status_display = {
  mutable sd_digest : CheckDigest.t option ref;
  sd_status : status;
  sd_vd : version_display;
  sd_cd : compiler_display;
}

and compiler_display = {
  cd_name : string;
  cd_commit : string;
  cd_date : string;
}

and display = {
  pds : package_display list;
  cds : compiler_display list;
}

(* For now, we raise this exception, as it is probably the case
   that the file was read before it was fully generated. Ideally,
   we should fix opam-builder to rename the file only once it is
   fully generated. *)
exception NotComplete

let import_lint lint_file =
  let errors = ref [] in
  let warnings = ref [] in
  let package = ref None in
  let version = ref None in
  let commit_name = ref None in
  let check_date = ref None in
  let versions = Hashtbl.create 1111 in
  let packages = Hashtbl.create 1111 in
  let package_versions = ref [] in
  let commit_package () =
    match !package with
    | None -> ()
    | Some package_name ->
      Hashtbl.add packages package_name !package_versions;
      package_versions := [];
      package := None
    in
  FileString.iter_lines (fun line ->
      let header, line = OcpString.cut_at line ':' in
      match header with
      | "commit" -> commit_name := Some line
      | "switch" -> assert (line = "lint")
      | "date" -> check_date := Some line
      | "package" ->
        commit_package ();
        package := Some line
      | "version" ->
        version := Some line;
        package_versions := line :: !package_versions
      | "warning" ->
        let num, msg = OcpString.cut_at line ':' in
        let error = int_of_string num in
        if not (IntSet.mem error filtered_errors) then
          warnings := (error, msg) :: !warnings
      | "error" ->
        let num, msg = OcpString.cut_at line ':' in
        let error = int_of_string num in
        if not (IntSet.mem error filtered_errors) then
          errors := (error, msg) :: !errors
      | "lint" ->
        begin
          assert (line = "end");
          match !package with
          | None -> assert false
          | Some package_name ->
            match !version with
            | None -> assert false
            | Some version_name ->
              Hashtbl.add versions version_name
                (StatusLint (!errors, !warnings), ref None);
              version := None;
              warnings := [];
              errors := []
        end
      | "export" -> assert (line = "end")
      | _ ->
        Printf.eprintf "%s: unknown line %S:%S\n%!" lint_file header line;
    ) lint_file;
  commit_package ();
  match !commit_name, !check_date with
  | Some commit_name, Some check_date ->
    let cd = {
      cd_commit = commit_name;
      cd_date = check_date;
      cd_name = "Lint";
    } in
    (cd, packages, versions)
  | _ -> raise NotComplete


let import_switch switch_file =
  let package = ref None in
  let version = ref None in
  let commit_name = ref None in
  let switch_name = ref None in
  let check_date = ref None in
  let status = ref None in
  let deps = ref None in
  let log_file = ref None in
  let build_file = ref None in
  let versions = Hashtbl.create 1111 in
  let packages = Hashtbl.create 1111 in
  let package_versions = ref [] in
  let commit_version () =
    match !version with
    | None -> ()
    | Some version_name ->
      begin
        match !status with
        | None -> assert false
        | Some status ->
          try
            let status = match status with
              | "non-installable" -> StatusNonInstallable
              | "non-available" -> StatusNonAvailable
              | "builder-error" -> StatusBuilderError
              | "failure" ->
                begin
                  match !deps, !log_file, !build_file with
                  | Some deps, Some log_file, Some build_file ->
                    StatusFailure (deps, log_file, build_file)

              (* workaround a bug in opam-builder... *)
                  | Some deps, None, Some build_file ->
                    StatusSuccess (deps, build_file)
                  | _ -> assert false
                end
              | "success" ->
                begin
                  match !deps, !build_file with
                  | Some deps, None -> StatusSuccess (deps, [])
                  | Some deps, Some build_file -> StatusSuccess (deps, build_file)
                  | _ -> assert false
                end
              | _ -> assert false
            in
            Hashtbl.add versions version_name (status, ref None)
          with exn ->
            Printf.eprintf "Error with status = %S\n%!" status;
            raise exn
      end;
      version := None;
      status := None;
      deps := None;
      log_file := None;
      build_file := None;
  in
  let commit_package () =
    commit_version ();
    match !package with
    | None -> ()
    | Some package_name ->
      Hashtbl.add packages package_name !package_versions;
      package_versions := [];
      package := None
  in
  let in_file = ref false in
  let file_lines = ref [] in
  FileString.iter_lines (fun line ->
    if !in_file then begin
      match line with
      | "begin-build:false" | "builder:end-build" ->
        in_file := false;
        build_file := Some (List.rev !file_lines);
        file_lines := []
      | "begin-log:false" | "builder:end-log" ->
        in_file := false;
        log_file := Some (List.rev !file_lines);
        file_lines := []
      | _ -> file_lines := line :: !file_lines;
    end else
      let header, line = OcpString.cut_at line ':' in
      match header with
      | "commit" -> commit_name := Some line
      | "switch" -> switch_name := Some line
      | "date" -> check_date := Some line
      | "package" ->
        commit_package ();
        package := Some line;
      | "status" ->
        status := Some line
      | "deps" ->
        deps := Some (OcpString.split line ',')
      | "version" ->
        commit_version ();
        version := Some line;
        package_versions := line :: !package_versions
      | "begin-build" ->
        assert (line = "true");
        in_file := true;
        file_lines := [];
      | "builder" when line = "begin-build" ->
        in_file := true;
        file_lines := [];
      | "begin-log" ->
        assert (line = "true");
        in_file := true;
        file_lines := [];
      | "builder" when line = "begin-log" ->
        in_file := true;
        file_lines := [];
      | "end-export" -> assert (line = "true")
      | "export" -> assert (line = "end")
      | _ ->
        Printf.eprintf "%s: unknown line %S:%S\n%!" switch_file header line;
  ) switch_file;
  commit_package ();
  match !commit_name, !check_date, !switch_name with
  | Some commit_name, Some check_date, Some switch_name ->
    let cd = {
      cd_commit = commit_name;
      cd_date = check_date;
      cd_name = switch_name;
    } in
    (cd, packages, versions)
  | _ ->
    raise NotComplete

let html_dir = "html"

let need_dir dir =
  if not (Sys.file_exists dir) then Unix.mkdir dir 0o755

let print_status_report vd cd status =
  let pd = vd.vd_pd in

  need_dir html_dir;
  need_dir pd.pd_dir;
  need_dir vd.vd_dir;

  let temp_file = Filename.concat vd.vd_dir "tmp" in
  let oc = open_out temp_file in
  CheckHtml.begin_html oc vd.vd_name;
  Printf.fprintf oc "<h1>Version: %s</h1>\n%!" vd.vd_name;
  Printf.fprintf oc "<p>Return to <a href=\"../../report-last.html#%s\">last report</a></p>\n" vd.vd_name;
  begin match status with
    | StatusLint (lint_errors, lint_warnings) ->
      begin
        match lint_errors with
        | [] -> ()
        | _ ->
          Printf.fprintf oc "<h3>Errors</h3>\n";
          Printf.fprintf oc "<ul>\n";
          List.iter (fun (num, msg) ->
              Printf.fprintf oc "  <li> %d: %s\n" num msg
            ) lint_errors;
          Printf.fprintf oc "</ul>\n";
      end;

      begin
        match lint_warnings with
        | [] -> ()
        | _ ->
          Printf.fprintf oc "<h3>Warnings</h3>\n";
          Printf.fprintf oc "<ul>\n";
          List.iter (fun (num, msg) ->
              Printf.fprintf oc "  <li> %d: %s\n" num msg
            ) lint_warnings;
          Printf.fprintf oc "</ul>\n";
      end;


    | StatusNonAvailable ->
      Printf.fprintf oc
        "<h3>This version is not available for this version of OCaml</h3>\n";

    | StatusNonInstallable ->
      Printf.fprintf oc
        "<h3>This version has broken dependencies for this version of OCaml</h3>\n";

    | StatusBuilderError ->
      Printf.fprintf oc
        "<h3>Builder error</h3>\n"
    | StatusSuccess (deps, build_content) ->
      Printf.fprintf oc
        "<h3>Build and install successful</h3>\n";
      Printf.fprintf oc "<h3>Dependencies:</h3>\n";
      Printf.fprintf oc "<ul>\n";
      List.iter (fun dep ->
          Printf.fprintf oc "  <li><p>%s</p></li>\n" dep) deps;
      Printf.fprintf oc "</ul>\n";
      Printf.fprintf oc "<h3>Build file:</h3>\n";
      Printf.fprintf oc "<pre>";
      List.iter (fun line -> Printf.fprintf oc "%s\n" line)
        build_content;
      Printf.fprintf oc "</pre>";
    | StatusFailure (deps, log_content, build_content) ->
      Printf.fprintf oc
        "<h3>Build failed</h3>\n";
      Printf.fprintf oc "<h3>Dependencies:</h3>\n";
      Printf.fprintf oc "<ul>\n";
      List.iter (fun dep ->
          Printf.fprintf oc "  <li><p>%s</p></li>\n" dep) deps;
      Printf.fprintf oc "</ul>\n";
      Printf.fprintf oc "<h3>Build file:</h3>\n";
      Printf.fprintf oc "<pre>";
      List.iter (fun line -> Printf.fprintf oc "%s\n" line)
        build_content;
      Printf.fprintf oc "</pre>";
      Printf.fprintf oc "<h3>Log file:</h3>\n";
      Printf.fprintf oc "<pre>";
      List.iter (fun line -> Printf.fprintf oc "%s\n" line)
        log_content;
      Printf.fprintf oc "</pre>";
    | StatusUnknown -> assert false
  end;
  CheckHtml.end_html oc;
  close_out oc;
  let digest = CheckDigest.file temp_file in
  let final_file = Filename.concat vd.vd_dir
      (CheckDigest.to_printable_string digest) in
  if not (Sys.file_exists final_file) then
    Sys.rename temp_file final_file;
  digest


let display_link oc sd msg =
  let vd = sd.sd_vd in
  let pd = vd.vd_pd in
  let cd = sd.sd_cd in
  let digest =
    match !(sd.sd_digest) with
    | Some digest -> digest
    | None ->
      let digest = print_status_report vd cd sd.sd_status in
      sd.sd_digest := Some digest;
      digest
  in
  Printf.fprintf oc "<a href=\"%s/%s/%s\">"
    pd.pd_name vd.vd_name
    (CheckDigest.to_printable_string digest);
  msg oc;
  Printf.fprintf oc "</a>";
  ()

let display_package oc pd =
    if pd.pd_display then begin
      Printf.fprintf oc "<tr>\n";
      Printf.fprintf oc "  <td><a name=\"%s\">%s</a></td>\n"
        pd.pd_name pd.pd_name;
      Printf.fprintf oc "  <td></td>\n";
      Printf.fprintf oc "</tr>\n";

      List.iter (fun vd ->
          if vd.vd_display then begin
            Printf.fprintf oc "<tr>\n";
            Printf.fprintf oc
              "  <td><a name=\"%s\"> </a><a href=\"http://github.com/ocaml/opam-repository/tree/master/packages/%s/%s/opam\">%s</a></td>\n"
              vd.vd_name pd.pd_name vd.vd_name  vd.vd_name;

            List.iter (fun sd ->
                match sd.sd_status with
                | StatusLint ([], []) ->
                  Printf.fprintf oc "<td class=\"lint-ok\"/>\n";

                | StatusLint ([], lint_warnings) ->
                  Printf.fprintf oc "<td class=\"lint-warnings\"\">";
                  display_link oc sd (fun oc ->
                      List.iter (fun (num, _) ->
                          Printf.fprintf oc "%d " num;
                        ) lint_warnings);
                  Printf.fprintf oc "</td>\n"

                | StatusLint (lint_errors, lint_warnings) ->
                  Printf.fprintf oc "<td class=\"lint-errors\"\">";
                  display_link oc sd (fun oc ->
                      List.iter (fun (num, _) ->
                          Printf.fprintf oc "%d " num;
                        ) lint_errors;
                      Printf.fprintf oc ": ";
                      List.iter (fun (num, _) ->
                          Printf.fprintf oc "%d " num;
                        ) lint_warnings
                    );
                  Printf.fprintf oc "</td>\n"

                | StatusBuilderError ->
                  Printf.fprintf oc "<td class=\"builder-error\">\n";
                  display_link oc sd (fun oc ->
                      Printf.fprintf oc "Builder Error");
                  Printf.fprintf oc "</td>\n";
                | StatusNonInstallable ->
                  Printf.fprintf oc "<td class=\"non-installable\">\n";
                  Printf.fprintf oc "<a href=\"http://ows.irill.org/latest/today/packages/%s-page.html#%s\">" pd.pd_name vd.vd_name;
                  Printf.fprintf oc "Uninstallable";
                  Printf.fprintf oc "</a>";
                  Printf.fprintf oc "</td>\n"
                | StatusNonAvailable ->
                  Printf.fprintf oc "  <td class=\"non-available\">\n";
                  Printf.fprintf oc "</td>\n"
                | StatusSuccess (deps, build_content) ->
                  Printf.fprintf oc "<td class=\"success\">\n";
                  display_link oc sd (fun oc -> Printf.fprintf oc "Ok");
                  Printf.fprintf oc "</td>\n"
                | StatusFailure (deps, build_content, log_content) ->
                  Printf.fprintf oc "<td class=\"failure\">\n";
                  display_link oc sd (fun oc -> Printf.fprintf oc "Fail");
                  Printf.fprintf oc "</td>\n"
                | StatusUnknown ->
                  Printf.fprintf oc "<td class=\"unknown\">Unknown</td>\n"

              ) vd.vd_statuses;

            Printf.fprintf oc "</tr>\n";
          end
        ) pd.pd_versions
    end

let get_display switches =
  let packages = ref StringMap.empty in
  List.iter (fun (_cd,ps, _versions) ->

      Hashtbl.iter (fun package_name package_versions ->
          let p =
            try
              StringMap.find package_name !packages
            with Not_found ->
              let p = ref StringSet.empty in
              packages := StringMap.add package_name p !packages;
              p
          in
          List.iter (fun version_name ->
              if not (StringSet.mem version_name !p) then
                p := StringSet.add version_name !p
            ) package_versions
        ) ps
    ) switches;

  let pds = ref [] in

  StringMap.iter (fun package_name package_versions ->
      let package_dir = Filename.concat html_dir package_name in
      need_dir package_dir;

      let pd = {
        pd_name = package_name;
        pd_display = true;
        pd_dir = package_dir;
        pd_versions = [];
      } in
      pds := pd :: !pds;

      StringSet.iter (fun version_name ->
          let version_dir = Filename.concat package_dir version_name in
          let vd = {
            vd_name = version_name;
            vd_dir = version_dir;
            vd_display = true;
            vd_statuses = [];
            vd_pd = pd;
          } in
          pd.pd_versions <- vd :: pd.pd_versions;

          List.iter (fun (cd,_,versions) ->
              let status, ref =
                try
                  Hashtbl.find versions version_name
                with Not_found -> StatusUnknown, ref None
              in
              let sd = {
                sd_status = status;
                sd_digest = ref;
                sd_vd = vd;
                sd_cd = cd;
              } in
              vd.vd_statuses <- sd :: vd.vd_statuses
            ) switches;
          vd.vd_statuses <- List.rev vd.vd_statuses
        ) !package_versions;
      pd.pd_versions <- List.rev pd.pd_versions

    ) !packages;

  let cds = List.map (fun (cd,_,_) -> cd) switches in
  let pds = List.rev !pds in
  { pds; cds }

let diff_mode display =
  List.iter (fun pd ->
      pd.pd_display <- false;

      let display_package () =
        pd.pd_display <- true
      in
      List.iter (fun vd ->
          vd.vd_display <- false;
          let display_version () =
            display_package ();
            vd.vd_display <- true;
          in

          let should_display =
            let statuses = ref [] in
            List.iter (fun sd ->
                let status =
                  match sd.sd_status with
                  | StatusNonAvailable -> ""
                  | StatusNonInstallable -> "BrokenDeps"
                  | StatusBuilderError -> "BuilderFailed"
                  | StatusFailure _ -> "Failed"
                  | StatusSuccess _ -> "OK"
                  | StatusLint _ -> "Lint"
                  | StatusUnknown -> "Unknown"
                in
                match !statuses with
                | [] | [_] -> statuses := status :: !statuses
                | s :: _ ->
                  if s <> status then
                    statuses := status :: !statuses
              ) vd.vd_statuses;

            match !statuses with
            | [] | [_] -> true
            | [s1;s2] -> s1 <> s2
            | _ :: _ :: _ :: _ -> true
          in

          if should_display then display_version ();
        ) pd.pd_versions
    ) display.pds

let all_mode display =
  List.iter (fun pd ->
      if not pd.pd_display then pd.pd_display <- true;
      List.iter (fun vd ->
          if not vd.vd_display then vd.vd_display <- true;
        ) pd.pd_versions
    ) display.pds

type pd_cd_status =
  | PackageNotAvailable   (* disabled for this switch *)
  | PackageNotInstallable (* deps or build errors *)
  | PackageHasCandidates  (* some errors *)
  | PackageInstallable    (* all candidates installable *)

let display_summary oc display =
  let ncds = List.length display.cds in
  List.iter (fun pd ->
      let nversions = List.length pd.pd_versions in
      let ncandidates = Array.make ncds 0 in
      let ninstallable = Array.make ncds 0 in
      let lint_errors = ref 0 in
      let lint_warnings = ref 0 in
      let lint_cd = ref 0 in
      List.iter (fun vd ->
          OcpList.iteri (fun i sd ->
              match sd.sd_status with
              | StatusLint ([], []) ->
                ninstallable.(i) <- ninstallable.(i) + 1;
                ncandidates.(i) <- ncandidates.(i) + 1;
                lint_cd := 1;
              | StatusLint ([], _) ->
                ninstallable.(i) <- ninstallable.(i) + 1;
                incr lint_warnings;
                lint_cd := 1;
              | StatusLint _ ->
                incr lint_errors;
                ninstallable.(i) <- ninstallable.(i) + 1;
                lint_cd := 1;
              | StatusNonInstallable ->
                ncandidates.(i) <- ncandidates.(i) + 1
              | StatusFailure _ ->
                ncandidates.(i) <- ncandidates.(i) + 1
              | StatusSuccess _ ->
                ncandidates.(i) <- ncandidates.(i) + 1;
                ninstallable.(i) <- ninstallable.(i) + 1
              | StatusBuilderError -> ()
              | StatusUnknown -> ()
              | StatusNonAvailable -> ()
            ) vd.vd_statuses
        ) pd.pd_versions;

      let nins = ref 0 in
      let no_avail = ref 0 in
      for i = 0 to ncds - 1 do
        if ninstallable.(i) > 0 then incr nins else
        if ncandidates.(i) = 0 then incr no_avail
      done;

      Printf.fprintf oc "<tr>\n";
      Printf.fprintf oc "<td class=\"global-summary-%s\"><a href='report-full.html#%s'>%s</a> (%d versions)</td>\n"
        ((* lint results are counted as a compiler when available;
            if the number of installable versions is
            the number of lint results, then it means that no
            actual compiler version supports the package *)
         if !nins = !lint_cd then "non-installable" else
         if !nins + !no_avail = ncds then "full" else
           "partial")
        pd.pd_name
        pd.pd_name
        nversions;
      if !lint_cd > 0 then begin
        if !lint_errors > 0 then
          Printf.fprintf oc "<td class=\"lint-summary-errors\">%s</td>\n"
            (if !lint_warnings > 0 then
               Printf.sprintf "%d + %d" !lint_errors !lint_warnings
             else
               string_of_int !lint_errors)
        else
        if !lint_warnings > 0 then
          Printf.fprintf oc "<td class=\"lint-summary-warnings\">%d</td>\n" !lint_warnings
        else
          Printf.fprintf oc "<td class=\"lint-summary-full\"></td>\n"
      end;
      for i = !lint_cd to ncds -1 do
        let (summary_class, content) =
          if ninstallable.(i) = nversions then
            ("full", "")
          else
          if ninstallable.(i) = ncandidates.(i) &&
             ncandidates.(i) > 0 then
            ("full", string_of_int ncandidates.(i))
          else
          if ncandidates.(i) = 0 then
            ("non-available", "")
          else
          if ninstallable.(i) = 0 then
            ("non-installable",
             Printf.sprintf "%d/%d" ninstallable.(i) ncandidates.(i))
          else
            ("partial",
             Printf.sprintf "%d/%d" ninstallable.(i) ncandidates.(i))
        in
        Printf.fprintf oc "<td class=\"compiler-summary-%s\">%s</td>\n" summary_class content
      done;
      Printf.fprintf oc "</tr>\n";

    ) display.pds


let print_report ~mode display switch_name =
  need_dir html_dir;

  begin match mode with
    | Full | Summary -> all_mode display
    | Diff -> diff_mode display
  end;

  let name, alter =
    if switch_name = "" then
      match mode with
      | Summary -> "last", "full"
      | Full -> "full", "last"
      | Diff -> assert false
    else
      match mode with
      | Summary -> assert false
      | Full -> switch_name ^ "-full", switch_name
      | Diff -> switch_name, switch_name ^ "-full"
  in
  let oc = open_out (Filename.concat html_dir
                       (Printf.sprintf "report-%s.html" name)) in
  CheckHtml.begin_html oc (Printf.sprintf "Report: %s" name);
  Printf.fprintf oc "<h1>opam-repository Status on Debian 8</h1>\n";

  Printf.fprintf oc "
<script src=\"jquery-2.2.4.min.js\"></script>
<style type=\"text/css\" media=\"screen\">
.floatTable td, .floatHolder td { padding: 5px;width: 100%%;}
.floatTr { background-color: #bbb; }
.floatTr2 { background-color: #bbb; }
.floatHolder { position: fixed; top: 0; left: 0; width: 100; display: none; }

/* lint-summary-* classes are used for the lint information on the summary page
   compiler-summary-* classes summarize the information of all versions of a package
      for a fixed compiler version (the version columns of the summary page)
   global-summary-* classes summarize the information of all versions of a package
      for all compilers (the package name case on the summary page)
*/

.lint-ok, .success, .lint-summary-full, .global-summary-full, .compiler-summary-full
  { background-color: green; }
.lint-summary-warnings
  { background-color: #4f4; }
.lint-warnings, .lint-summary-errors, .global-summary-partial, .compiler-summary-partial
  { background-color: orange; }

/* global non-installability means that no version of the package is
   installable for any compiler version; we treat this as a hard
   error. On the contrary, non-installability of a single version or
   all versions of a package under a fixed compiler version merely
   means that its dependencies are unavailable (or in conflict), so we
   do not treat it as a hard error but as an indirect form of
   unaivalability. */

.lint-errors, .failure, .global-summary-non-installable
  { background-color: red; }
.non-available, .non-installable, .compiler-summary-non-available, .compiler-summary-non-installable
  { background-color: white; }
</style>
";
  Printf.fprintf oc "<input id=\"search\" class=\"search-query\" type=\"text\" placeholder=\"Search packages\"/>
";


  begin match mode with
    | Diff ->
      Printf.fprintf oc
        "<h2>Diff Mode: only status changes are displayed (<a href='report-%s.html'>Full</a>)</h2>\n" alter
    | Summary ->
      Printf.fprintf oc
        "<h2>Summary Mode: only packages are displayed (<a href='report-%s.html'>Full</a>)</h2>\n" alter
    | Full ->
      Printf.fprintf oc
        "<h2>Full Mode: all packages are displayed (<a href='report-%s.html'>Summary</a>)</h2>\n" alter
  end;

  Printf.fprintf oc "<table class=\"floatHolder\">\n";
  Printf.fprintf oc "<tr class=\"floatTr2\">\n";
  Printf.fprintf oc "  <td><a href=\"report-last.html\">Package</a></td>\n";
  List.iter (fun cd ->

      if Sys.file_exists (Printf.sprintf "%s/report-%s.html" html_dir cd.cd_name)
      then
        Printf.fprintf oc "  <td><a href=\"report-%s.html\">%s</a></td>\n"
          cd.cd_name cd.cd_name
      else
        Printf.fprintf oc "  <td>%s</td>\n" cd.cd_name;
    ) display.cds;
  Printf.fprintf oc "</tr>\n";
  Printf.fprintf oc "</table>\n";

  Printf.fprintf oc "<table id=\"packages\" class=\"floatTable\">\n";


  (****** Table line with compiler names ******)

  Printf.fprintf oc "<tr class=\"floatTr\">\n";
  Printf.fprintf oc "  <td><a href=\"report-last.html\">Package</a></td>\n";
  List.iter (fun cd ->


      if Sys.file_exists (Printf.sprintf "%s/report-%s.html" html_dir cd.cd_name)
      then
        Printf.fprintf oc "  <td><a href=\"report-%s.html\">%s</a></td>\n"
          cd.cd_name cd.cd_name
      else
        Printf.fprintf oc "  <td>%s</td>\n" cd.cd_name
    ) display.cds;
  Printf.fprintf oc "</tr>\n";

  begin match mode with
    | Diff | Full ->

      (****** Table line with commits   ******)

      Printf.fprintf oc "<tr class=\"floatTr\">\n";
      Printf.fprintf oc "  <td>Commit: </td>\n";
      List.iter (fun cd ->
          Printf.fprintf oc "  <td><a href=\"https://github.com/ocaml/opam-repository/commit/%s\">%s</a></td>\n" cd.cd_commit cd.cd_commit;
        ) display.cds;
      Printf.fprintf oc "</tr>\n";


      (****** Table line with dates   ******)


      Printf.fprintf oc "<tr>\n";
      Printf.fprintf oc "  <td>Date:</td>\n";
      List.iter (fun cd ->
          Printf.fprintf oc "  <td>%s</td>\n"
            (CheckDate.ISO8601.of_float (float_of_string cd.cd_date))
        ) display.cds;
      Printf.fprintf oc "</tr>\n";


      (****** Table lines with packages   ******)
      List.iter (display_package oc) display.pds;

    | Summary ->

      display_summary oc display
  end;

  Printf.fprintf oc "</table>\n";
  Printf.fprintf oc
    "
<script>
var fhHeight = 0;
var ft = $('.floatTable');
var fhold = $('.floatHolder');
var win = $(window);
var ftr = $('.floatTr');
var ftrs = ftr.children();

$(window).scroll(function() {

$(fhold).width(ft.width())

if(fhHeight==0 && (ft.offset().top - win.scrollTop()) <= 0 &&
       !ftr.parent().hasClass('floatHolder')) {
    fhHeight = ftr.height();
//    ftr.appendTo(fhold);

var i;
var fholds = fhold.children()[0].children[0].children;
for(i=0;i<ftrs.length;i++){
   var w = $(ftrs[i]).width();
   $(fholds[i]).width(w);
}


    fhold.show();
}
else if(fhHeight!=0 && fhHeight!=-1 &&
        (ft.offset().top - win.scrollTop() > fhHeight ||
         ft.offset().top + ft.outerHeight(true) - win.scrollTop() < 0))  {
    if(ft.offset().top + ft.outerHeight(true) - win.scrollTop() < 0) {
        fhHeight = -1;
    }
    else {
        fhHeight = 0;
    }
    ftr.prependTo(ft);
    fhold.hide();
}
else if(fhHeight==-1 && ft.offset().top + ft.outerHeight(true) - win.scrollTop() > 0) {
    fhHeight = 0;
  }






});
 </script>
<script src=\"search.js\"> </script>
";

  CheckHtml.end_html oc;
  close_out oc;
  Printf.eprintf "Generated\n%!";
  ()

let import t =
  Printf.eprintf "CheckImport.import\n%!";
  let export_files = ref [] in
  List.iter (fun dir ->
      let files = Sys.readdir dir in
      Array.iter (fun file ->
          if Filename.check_suffix file ".export" then
            let base = Filename.chop_suffix file ".export" in
            match OcpString.split base '-' with
            | date :: commit :: switch :: []
              when String.length date = 14
              ->
              let filename =  Filename.concat dir file in
              export_files := (switch, date, filename) :: !export_files;
            | _ -> ()
        ) files
    ) t.dirs;
  let export_files = List.sort compare !export_files in
  let switches = ref StringMap.empty in
  List.iter (fun (switch, date, filename) ->
      let ref = try
          StringMap.find switch !switches
        with Not_found ->
          let ref = ref [] in
          switches := StringMap.add switch ref !switches;
          ref
      in
      ref := filename :: !ref
    ) export_files;

  let to_add = ref [] in
  let new_file = ref false in
  StringMap.iter (fun switch filenames ->
      match !filenames with
      | filename :: _ ->
        if not (StringSet.mem filename t.files) then begin
          to_add := filename :: !to_add;
          new_file := true
        end
      | _ -> ()
    ) !switches;

  if !new_file then begin
    try
      let lint = ref None in
      let files = ref [] in
      StringMap.iter (fun switch filenames ->
          match !filenames with
          | [] -> assert false
          | filename :: _ ->

            Printf.eprintf "Loading %S\n%!" filename;
            match switch with
            | "lint" ->
              lint := Some (import_lint filename)
            | switch ->
              files := (switch, import_switch filename) :: !files;

              begin
                let switches = match !filenames with
                    f1::f2::f3::f4::f5::f6:: _ -> [f1;f2;f3;f4;f5;f6]
                  | files -> files in
                let switches = List.map (fun filename ->
                    import_switch filename
                  ) switches in
                let display = get_display switches in
                print_report ~mode:Full display switch;
                print_report ~mode:Diff display switch
              end;
        ) !switches;

      let files = List.map snd (List.sort compare !files) in
      let switches = match !lint with
        | None -> files
        | Some lint -> lint :: files in

      let display = get_display switches in
      print_report ~mode:Summary display "";
      print_report ~mode:Full display "";
      Printf.eprintf "All reports generated\n%!";

      List.iter (fun filename ->
          t.files <- StringSet.add filename t.files
        ) !to_add
    with NotComplete ->
      (* One of the files was not finished, we should restart in one minute.*)
      ()
  end

let import t =
  try
    import t
  with exn ->
    Printf.eprintf
      "Error %s while importing status, 1st time, waiting for 5s and restart.\n%!" (Printexc.to_string exn);
    Unix.sleep 5;
    try
      import t
    with exn ->
      Printf.eprintf
        "Error %s while importing status, 2nd time, waiting for 5s and restart.\n%!"
        (Printexc.to_string exn);
      Unix.sleep 5;
      import t
