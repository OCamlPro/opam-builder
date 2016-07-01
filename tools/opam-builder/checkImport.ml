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
  File.iter_lines (fun line ->
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
    (commit_name, check_date, "Lint", packages, versions)
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
  File.iter_lines (fun line ->
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
    (commit_name, check_date, switch_name, packages, versions)
  | _ ->
    raise NotComplete

let html_dir = "html"

let print_report ~diff switches name =

  if not (Sys.file_exists html_dir) then
    Unix.mkdir html_dir 0o755;

  (* Find all packages, and all versions per package *)
  let packages = ref StringMap.empty in
  List.iter (fun (_,_,_,ps, _) ->

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

  (* Create one report per package/version *)
  StringMap.iter (fun package_name package_versions ->
      let package_dir = Filename.concat html_dir package_name in
      if not (Sys.file_exists package_dir) then
        Unix.mkdir package_dir 0o755;

      StringSet.iter (fun version_name ->
          let version_dir = Filename.concat package_dir version_name in
          if not (Sys.file_exists version_dir) then
            Unix.mkdir version_dir 0o755;

          List.iter (fun (commit, date, switch, _, versions) ->

              try
                let status, ref = Hashtbl.find versions version_name in
                let temp_file = Filename.concat version_dir "tmp" in
                let oc = open_out temp_file in
                CheckHtml.begin_html oc version_name;
                Printf.fprintf oc "<h1>Version: %s</h1>\n%!" version_name;
                Printf.fprintf oc "<p>Return to <a href=\"../../report-last.html#%s\">last report</a></p>\n" version_name;
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
                end;
                CheckHtml.end_html oc;
                close_out oc;
                let digest = CheckDigest.file temp_file in
                let final_file = Filename.concat version_dir
                    (CheckDigest.to_printable_string digest) in
                if not (Sys.file_exists final_file) then
                  Sys.rename temp_file final_file;
                ref := Some digest
              with Not_found -> ()

            ) switches;


        ) !package_versions

    ) !packages;


  let oc = open_out (Filename.concat html_dir
                       (Printf.sprintf "report-%s.html" name)) in
  CheckHtml.begin_html oc (Printf.sprintf "Report: %s" name);
  Printf.fprintf oc "<h1>opam-repository Status on Debian 8</h1>\n";

  Printf.fprintf oc "
<script src=\"jquery-2.2.4.min.js\"></script>
<style type=\"text/css\" media=\"screen\">
     .floatTable td, .floatHolder td {
                 padding: 5px;
                 width: 100%%;
      }
     .floatTr {
        background-color: #bbb;
      }
     .floatTr2 {
        background-color: #bbb;
      }
      .floatHolder {
        position: fixed;
        top: 0;
        left: 0;
        width: 100;
        display: none;
       }
</style>
<input id=\"search\" class=\"search-query\" type=\"text\" placeholder=\"Search packages\"/>
";


  if diff then begin
    Printf.fprintf oc "<h2>Diff Mode: only status changes are displayed</h2>\n";
  end;

  Printf.fprintf oc "<table class=\"floatHolder\">\n";
  Printf.fprintf oc "<tr class=\"floatTr2\">\n";
  Printf.fprintf oc "  <td><a href=\"report-last.html\">Package</a></td>\n";
  List.iter (fun (commit, date, switch, _, _) ->

      if Sys.file_exists (Printf.sprintf "%s/report-%s.html" html_dir switch)
      then
        Printf.fprintf oc "  <td><a href=\"report-%s.html\">%s</a></td>\n"
          switch switch
      else
        Printf.fprintf oc "  <td>%s</td>\n" switch;
    ) switches;
  Printf.fprintf oc "</tr>\n";
  Printf.fprintf oc "</table>\n";

  Printf.fprintf oc "<table id=\"packages\" class=\"floatTable\">\n";

  Printf.fprintf oc "<tr class=\"floatTr\">\n";
  Printf.fprintf oc "  <td><a href=\"report-last.html\">Package</a></td>\n";
  List.iter (fun (commit, date, switch, _, _) ->

      if Sys.file_exists (Printf.sprintf "%s/report-%s.html" html_dir switch)
      then
        Printf.fprintf oc "  <td><a href=\"report-%s.html\">%s</a></td>\n"
          switch switch
      else
        Printf.fprintf oc "  <td>%s</td>\n" switch;
    ) switches;
  Printf.fprintf oc "</tr>\n";


  Printf.fprintf oc "<tr class=\"floatTr\">\n";
  Printf.fprintf oc "  <td>Commit: </td>\n";
  List.iter (fun (commit, date, switch, _, _) ->
      Printf.fprintf oc "  <td><a href=\"https://github.com/ocaml/opam-repository/commit/%s\">%s</a></td>\n" commit commit;
    ) switches;
  Printf.fprintf oc "</tr>\n";

  Printf.fprintf oc "<tr>\n";
  Printf.fprintf oc "  <td>Date:</td>\n";
  List.iter (fun (commit, date, switch, _, _) ->
      Printf.fprintf oc "  <td>%s</td>\n"
        (CheckDate.ISO8601.of_float
           (float_of_string date))
    ) switches;
  Printf.fprintf oc "</tr>\n";

  StringMap.iter (fun package_name package_versions ->

      let display_package =
        let display = ref false in
        function () ->
          if not !display then begin
            display := true;
            Printf.fprintf oc "<tr>\n";
            Printf.fprintf oc "  <td><a name=\"%s\">%s</a></td>\n"
              package_name package_name;
            Printf.fprintf oc "  <td></td>\n";
            Printf.fprintf oc "</tr>\n";
          end
      in
      StringSet.iter (fun version_name ->

          let display_version =
            let display = ref false in
            function () ->
              if not !display then begin
                display_package ();
                Printf.fprintf oc "<tr>\n";
                Printf.fprintf oc "  <td><a name=\"%s\"> </a><a href=\"http://github.com/ocaml/opam-repository/tree/master/packages/%s/%s/opam\">%s</a></td>\n"
                  version_name package_name version_name  version_name;
              end
          in

          let should_display =
            not diff ||
            (let statuses = ref [] in
             List.iter (fun (_,_,_,_,versions) ->
                 let status =
                   try
                     let status, _ref = Hashtbl.find versions version_name in
                     status
                   with Not_found ->
                     StatusNonAvailable
                 in
                 let status =
                   match status with
                   | StatusNonAvailable -> ""
                   | StatusNonInstallable -> "BrokenDeps"
                   | StatusBuilderError -> "BuilderFailed"
                   | StatusFailure _ -> "Failed"
                   | StatusSuccess _ -> "OK"
                   | StatusLint _ -> "Lint"
                 in
                 match !statuses with
                 | [] | [_] -> statuses := status :: !statuses
                 | s :: _ ->
                   if s <> status then
                     statuses := status :: !statuses
               ) switches;

             match !statuses with
             | [] | [_] -> true
             | [s1;s2] -> s1 <> s2
             | _ :: _ :: _ :: _ -> true
            )
          in

          if should_display then begin
            display_version ();
            List.iter (fun (_,_,_,_,versions) ->

                try
                  let status, ref = Hashtbl.find versions version_name in
                  let with_link msg =
                    Printf.fprintf oc "  <a href=\"%s/%s/%s\">"
                      package_name version_name
                      (match !ref with
                       | None -> assert false
                       | Some digest ->
                         CheckDigest.to_printable_string digest);
                    msg oc;
                    Printf.fprintf oc "  </a>\n";
                  in
                  match status with
                  | StatusLint (lint_errors, lint_warnings) ->
                    begin
                      match lint_errors with
                      | _ :: _ ->
                        Printf.fprintf oc "  <td style=\"background-color: red;\">";
                        with_link (fun oc ->
                            List.iter (fun (num, _) ->
                                Printf.fprintf oc "%d " num;
                              ) lint_errors;
                            Printf.fprintf oc ": ";
                            List.iter (fun (num, _) ->
                                Printf.fprintf oc "%d " num;
                              ) lint_warnings
                          );
                        Printf.fprintf oc "</td>\n"
                      | [] ->
                        match lint_warnings with
                        | _ :: _ ->
                          Printf.fprintf oc "  <td style=\"background-color: orange;\">";
                          with_link (fun oc ->
                              List.iter (fun (num, _) ->
                                  Printf.fprintf oc "%d " num;
                                ) lint_warnings;
                              Printf.fprintf oc "</td>\n")
                        | [] ->
                          Printf.fprintf oc "  <td style=\"background-color: green;\">";
                          Printf.fprintf oc "</td>\n"
                    end
                  | StatusBuilderError ->
                    Printf.fprintf oc "<td>\n";
                    with_link (fun oc ->
                        Printf.fprintf oc "Builder Error");
                    Printf.fprintf oc "</td>\n";
                  | StatusNonInstallable ->
                    Printf.fprintf oc "<td style=\"background-color: red;\">\n";
                    Printf.fprintf oc "<a href=\"http://ows.irill.org/latest/today/packages/%s-page.html#%s\">" package_name version_name;
                    Printf.fprintf oc "Broken Deps";
                    Printf.fprintf oc "</a>";
                    Printf.fprintf oc "</td>\n"
                  | StatusNonAvailable ->
                    Printf.fprintf oc "  <td>\n";
                    Printf.fprintf oc "</td>\n"
                  | StatusSuccess (deps, build_content) ->
                    Printf.fprintf oc "<td style=\"background-color: green;\">\n";
                    with_link (fun oc -> Printf.fprintf oc "Ok");
                    Printf.fprintf oc "</td>\n"
                  | StatusFailure (deps, build_content, log_content) ->
                    Printf.fprintf oc "<td style=\"background-color: orange;\">\n";
                    with_link (fun oc -> Printf.fprintf oc "Fail");
                    Printf.fprintf oc "</td>\n"
                with Not_found ->
                  Printf.fprintf oc "<td>Unknown</td>\n"
              ) switches;
            Printf.fprintf oc "</tr>\n";
          end
        ) !package_versions;

    ) !packages;

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
                print_report ~diff:true switches switch
              end;
        ) !switches;

      let files = List.map snd (List.sort compare !files) in
      let switches = match !lint with
        | None -> files
        | Some lint -> lint :: files in

      print_report ~diff:false switches "last";

      List.iter (fun filename ->
          t.files <- StringSet.add filename t.files
        ) !to_add
    with NotComplete ->
      (* One of the files was not finished, we should restart in one minute.*)
      ()
  end
