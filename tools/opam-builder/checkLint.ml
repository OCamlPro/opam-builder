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



(* TODO:
  * We should replace contact@ocamlpro.com by oss-devs@ocamlpro.com for
     packages belonging to OCamlPro ?
*)


open StringCompat
open CheckTypes.V
open CheckTypes

let max_errors = 50 (* TODO: auto-compute *)
let colon_pos = 20 (* Monitor opam... *)

let load_lint lint_version filename =
  try
    let lines = File.lines_of_file filename in
    let lint_warnings, lint_errors =
      match lines with
      |[ "exit-code:0" ] -> [], []
      | ("exit-code:1" | "exit-code:0") :: _ :: lines ->
        let warnings = ref [] in
        let errors = ref [] in
        List.iter (fun line ->
          let len = String.length line in
          let header = String.sub line 0 (colon_pos - 3) in
          let num = String.sub line (colon_pos-2) 2 in
          let msg = String.sub line (colon_pos+2) (len-colon_pos-2) in
          (*          Printf.eprintf "[%s][%s][%s]\n%!" header num msg; *)
          let kind =
            match header with
            | "          warning" -> warnings
            | "            error" -> errors
            | _ -> failwith "Bad line header"
          in
          let num =
            try int_of_string num with
            | _ ->
              try int_of_string (String.sub num 1 1) with exn ->
                Printf.eprintf "Error: incorrect lint file %s\n%!" filename;
                Printf.eprintf "  exception: %s\n%!" (Printexc.to_string exn);
                0 in
          kind := (num, msg)  :: !kind
        ) lines;
        !warnings, !errors
      | _ -> failwith "Bad file content"
    in
    Some { lint_version; lint_warnings; lint_errors }
  with exn ->
    Printf.eprintf "Error: incorrect lint file %s\n%!" filename;
    Printf.eprintf "  exception: %s\n%!" (Printexc.to_string exn);
    None

let load_all_lint dirs c =
  StringMap.iter (fun _ p ->
    let package_dir = Filename.concat dirs.cache_dir p.package_name in
    StringMap.iter (fun _ v ->
      let version_dir = Filename.concat package_dir v.version_name in
      let lint_file = Filename.concat version_dir
        (Printf.sprintf "%s.lint" v.version_name) in
      v.version_lint <- load_lint v lint_file;
    ) p.package_versions;
  ) c.packages

let analyze dirs c =

  load_all_lint dirs c;

  let errors = Array.make max_errors [] in
  let error_messages = Array.make max_errors "" in
  let warnings = Array.make max_errors [] in
  let warning_messages = Array.make max_errors "" in

  StringMap.iter (fun _ p ->
  StringMap.iter (fun _ v ->
    match v.version_lint with
    | None -> ()
    | Some { lint_errors; lint_warnings } ->
      List.iter (fun (num, msg) ->
        errors.(num) <- v :: errors.(num);
        error_messages.(num) <- msg
      ) lint_errors;
      List.iter (fun (num, msg) ->
        warnings.(num) <- v :: warnings.(num);
        warning_messages.(num) <- msg;
      ) lint_warnings;
    ) p.package_versions;
  ) c.packages;

  let report_file = Filename.concat dirs.report_dir
    (Printf.sprintf "%s-lint.html" c.commit_name) in
  let oc = open_out report_file in
  Printf.fprintf oc "<h1>Commit %s</h1>\n" c.commit_name;
  Printf.fprintf oc "<p><a href=\"#by-error\">Sorted by error</a></p>\n";
  Printf.fprintf oc "<table>\n";
  Printf.fprintf oc "<tr>\n";
  Printf.fprintf oc "  <td>Package</td>\n";
  Printf.fprintf oc "  <td>Lint</td>\n";
  Printf.fprintf oc "</tr>\n";

  StringMap.iter (fun package_name p ->
    Printf.fprintf oc "<tr>\n";
    Printf.fprintf oc "  <td>%s</td>\n" package_name;
    Printf.fprintf oc "  <td></td>\n";
    Printf.fprintf oc "</tr>\n";

    StringMap.iter (fun version_name v ->
      Printf.fprintf oc "<tr>\n";
      Printf.fprintf oc "  <td><a href=\"http://github.com/ocaml/opam-repository/tree/master/packages/%s/%s/opam\">%s</a></td>\n"
        package_name version_name version_name;
      begin match v.version_lint with
      | None ->
         Printf.fprintf oc "  <td style=\"background-color: white;\"></td>\n"
      | Some { lint_warnings; lint_errors } ->
        match lint_errors with
        | _ :: _ ->
          Printf.fprintf oc "  <td style=\"background-color: red;\">";
          Printf.fprintf oc "  <a href=\"#%s\">" version_name;
          List.iter (fun (num, _) ->
            Printf.fprintf oc "%d " num;
          ) lint_errors;
          Printf.fprintf oc ": ";
          List.iter (fun (num, _) ->
            Printf.fprintf oc "%d " num;
          ) lint_warnings;
          Printf.fprintf oc "  </a>\n";
          Printf.fprintf oc "</td>\n"
        | [] ->
          match lint_warnings with
          | _ :: _ ->
            Printf.fprintf oc "  <td style=\"background-color: orange;\">";
            Printf.fprintf oc "  <a href=\"#%s\">" version_name;
            List.iter (fun (num, _) ->
              Printf.fprintf oc "%d " num;
            ) lint_warnings;
            Printf.fprintf oc "  </a>\n";
            Printf.fprintf oc "</td>\n"
          | [] ->
            Printf.fprintf oc "  <td style=\"background-color: green;\">";
            Printf.fprintf oc "</td>\n"
      end;
      Printf.fprintf oc "</tr>\n";

    ) p.package_versions;
  ) c.packages;

  Printf.fprintf oc "</table>\n";

  Printf.fprintf oc "<a name=\"by-error\"><h1>Packages by Errors</h1></a>\n";

  Printf.fprintf oc "<table>\n";

  Array.iteri (fun i versions ->
    match versions with
    | [] -> ()
    | _ ->
      Printf.fprintf oc "<tr><td><a href=\"#error-%d\">Error %d</a></td><td>%d</td><td>%s</td></tr>\n"
        i i
        (List.length versions)
        error_messages.(i);
  ) errors;

  Array.iteri (fun i versions ->
    match versions with
    | [] -> ()
    | _ ->
      Printf.fprintf oc "<tr><td><a href=\"#warning-%d\">Warning %d</a></td><td>%d</td><td>%s</td></tr>\n"
        i i
        (List.length versions)
        warning_messages.(i);
  ) warnings;

  Printf.fprintf oc "</table>\n";

  Array.iteri (fun i versions ->
    match versions with
    | [] -> ()
    | _ ->
      Printf.fprintf oc "<a name=\"error-%d\"><h2>Error %d: %s</h2></a>\n" i i error_messages.(i);
      Printf.fprintf oc "<p>";
      List.iter (fun v ->
        Printf.fprintf oc "<a href=\"#%s\">%s</a> "
          v.version_name v.version_name
      ) versions
  ) errors;

  Array.iteri (fun i versions ->
    match versions with
    | [] -> ()
    | _ ->
      Printf.fprintf oc "<a name=\"warning-%d\"><h2>Warning %d: %s</h2></a>\n" i i warning_messages.(i);
      Printf.fprintf oc "<p>";
      List.iter (fun v ->
        Printf.fprintf oc "<a href=\"#%s\">%s</a> "
          v.version_name v.version_name
      ) versions
  ) warnings;

  Printf.fprintf oc "<a name=\"by-error\"><h1>Errors by Packages</h1></a>\n";

  let fprintf_version oc p v lint_errors lint_warnings =
    Printf.fprintf oc "<a name=\"%s\">\n" v.version_name;
    Printf.fprintf oc "<h2><a href=\"http://github.com/ocaml/opam-repository/tree/master/packages/%s/%s/opam\">%s</a></h2>\n"
    p.package_name v.version_name v.version_name;
    Printf.fprintf oc "</a>\n";

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

  in

  StringMap.iter (fun package_name p ->

    StringMap.iter (fun version_name v ->
      match v.version_lint with
      | None -> ()
      | Some { lint_warnings; lint_errors } ->
        match lint_errors with
        | _ :: _ -> fprintf_version oc p v lint_errors lint_warnings
        | [] ->
          match lint_warnings with
          | _ :: _ ->
            fprintf_version oc p v lint_errors lint_warnings
          | [] -> ()
    ) p.package_versions;
  ) c.packages;


  close_out oc;

  ()

let autofix_packages dirs c =
  load_all_lint dirs c;

  let can_auto_fix =
    [
      24; (* Error 24	307	Field 'maintainer' has the old default value *)
      42; (* Error 42	1687	The 'dev-repo' field doesn't use version control. You may use URLs of the form "git+https://" or a ".hg" or ".git" suffix *)


      (*  Error 21	530	Field 'opam-version' doesn't match the current version, validation may not be accurate: "1" *)
      (*  Error 22	2	Some fields are present but empty; remove or fill them: "bug_reports" *)
      23; (* Error 23	12	Missing field 'maintainer' *)
      25; (* Error 25	1927	Missing field 'authors' *)
      (* Error 30	1	Field 'depopts' contains formulas or version constraints *)
      (* Error 31	7	Fields 'depends' and 'depopts' refer to the same package names: "pcre" *)
      (* Error 32	1106	Field 'ocaml-version' is deprecated, use 'available' and the 'ocaml-version' variable instead *)
      (* Error 33	84	Field 'os' is deprecated, use 'available' and the 'os' variable instead *)
      35; (* Error 35	1701	Missing field 'homepage' *)
      (* Error 39	35	Command 'make' called directly, use the built-in variable instead *)
      (* Warning 20	2	Field 'opam-version' refers to the patch version of opam, it should be of the form MAJOR.MINOR: "1.2.0" *)
      (* Warning 26	2077	No field 'install', but a field 'remove': install instructions probably part of 'build'. Use the 'install' field or a .install file *)
      (* Warning 27	7	No field 'remove' while a field 'install' is present, uncomplete uninstallation suspected *)
      36; (* Warning 36	2446	Missing field 'bug-reports' *)
      37; (* Warning 37	1336	Missing field 'dev-repo' *)
    ] in


  let find_field_to_fix v0 field_name f =
    let opam_file = CheckUpdate.opam_file dirs v0 in
    let opam = CopamOpamFile.parse opam_file in
    let rec iter items =
      match items with
      | CopamTypes.Variable (_, field, v) :: items2
          when field = field_name ->
        let item_pos default items =
          let open CopamTypes in
          match items with
          | [] -> ("", default, 0)
          | item :: _ ->
            match item with
            | Variable (pos, _, _) -> pos
            | Section (pos, _) -> pos
        in

        let (file1, line1, col1) = item_pos max_int items in
        let (file2, line2, col2) = item_pos (line1+1) items2 in
        begin match v with
            CopamTypes.String (_,s)
          | CopamTypes.List (_,
                             [ CopamTypes.String (_,s)]) ->
            if line1 +1 = line2 && col1 = 0 then begin
              Printf.eprintf "Ok, can replace field %s of %s\n%!"
                field_name opam_file;
              f opam_file field_name line1 s
            end else begin
              Printf.eprintf "Warning: cannot replace field %s in %s\n%!"
                field_name opam_file
            end
          | _ -> ()
        end
      | _ :: items  -> iter items
      | [] -> ()
    in
    iter opam.CopamTypes.file_contents
  in


  let replace_line filename pos1 s =
    let lines = File.lines_of_file filename in
    let rec iter pos2 lines =
      match lines with
      | [] ->
        Printf.eprintf "Warning: line %d not found in %s\n%!"
          pos1 filename;
        []
      | line :: lines ->
        if pos1 = pos2 then
          s @ lines
        else
          line :: (iter (pos2+1) lines)
    in
    File.file_of_lines filename (iter 1 lines)
  in


  StringMap.iter (fun _ p ->

      (* 1/ find versions that have the required fields *)
      let solvers = Array.make max_errors [] in
      let missing = Array.make max_errors [] in
      StringMap.iter (fun _ v ->
          match v.version_lint with
          | None ->
            Printf.eprintf "%s has no lint !\n%!" v.version_name
          | Some { lint_errors; lint_warnings } ->
            let errors = Array.make max_errors false in
            List.iter (fun (num, msg) ->
                Printf.eprintf "%s has error %d\n%!" v.version_name num;
                errors.(num) <- true;
                missing.(num) <- v :: missing.(num);
              ) (lint_errors @ lint_warnings);
            List.iter (fun num ->
                if not errors.(num) then
                  solvers.(num) <- v :: solvers.(num)
              ) can_auto_fix;
        ) p.package_versions;

      List.iter (fun num ->
          match missing.(num) with
          | [] -> ()
          |  _ ->
            match num with
            | 24 ->
          (*
          if
            not (OcpString.starts_with p.package_name "ocp-") &&
            not (OcpString.starts_with p.package_name "ocplib-") &&
            not (OcpString.starts_with p.package_name "opam") &&
            not (List.mem p.package_name
                   [ "ocaml-top"; "tryocaml" ]) then
          *)
              List.iter (fun v0 ->
                  find_field_to_fix v0 "maintainer"
                    (fun opam_file field_name line v ->
                       let comment =
                         "  (* auto-fix removed wrong maintainer field *)" in
                       replace_line opam_file line [comment]
                    )
                ) missing.(num)

            | 42 ->
              List.iter (fun v0 ->
                  find_field_to_fix v0 "dev-repo"
                    (fun opam_file field_name line v ->
                       let len = String.length v in
                       let prefix = "https://github.com/" in
                       let prefix_len = String.length prefix in
                       if len > prefix_len &&
                          String.sub v 0 prefix_len = prefix &&
                          String.sub v (len-4) 4 = ".git" then
                         let new_v = "git+" ^ v in
                         let new_v = Printf.sprintf "%s: %S" field_name new_v in
                         Printf.eprintf "Replacing\n  %s by\n  %s\nin %s\n%!"
                           v new_v opam_file;
                         let comment = "  (* auto-fix replace field *)" in
                         replace_line opam_file line [comment; new_v]
                       else
                         let prefix = "http://github.com/" in
                         let prefix_len = String.length prefix in
                         if len > prefix_len &&
                            String.sub v 0 prefix_len = prefix &&
                            String.sub v (len-4) 4 = ".git" then
                           let new_v = "git+https" ^
                                       (String.sub v 4 (len-4)) in
                           let new_v = Printf.sprintf "%s: %S" field_name new_v in
                           Printf.eprintf "Replacing\n  %s by\n  %s\nin %s\n%!"
                             v new_v opam_file;
                           let comment = "  (* auto-fix replace field *)" in
                           replace_line opam_file line [comment; new_v]
                         else begin
                           Printf.eprintf
                             "Warning: don't know how to fix %s in %s:\n  %s\n%!"
                             field_name opam_file v
                         end
                    )
                ) missing.(num)

            | _ ->
              let add_field field_name field_value =
                List.iter (fun v ->
                    let opam_file = CheckUpdate.opam_file dirs v in
                    let old_content = File.string_of_file opam_file in
                    File.file_of_string (opam_file ^ ".orig") old_content;
                    let new_content =
                      Printf.sprintf
                        "%s\n  (* auto-fix field %s for conf-* packages *)\n%s: %S\n"
                        old_content field_name field_name field_value
                    in
                    File.file_of_string opam_file new_content

                  ) missing.(num)
              in
              match solvers.(num) with
              | [] ->
                if OcpString.starts_with p.package_name "conf-" then
                  begin
                    match num with
                    | 36 -> add_field
                              "bug-reports"
                              "https://github.com/ocaml/opam-repository/issues"
                    | 37 -> add_field
                              "dev-repo"
                              "git+https://github.com/ocaml/opam-repository.git"
                    | _ -> ()
                  end

              |  _ ->
                let add_content = ref None in

                let find_field field_name =
                  List.iter (fun v0 ->
                      match !add_content with
                      | Some _ -> ()
                      | None ->
                        let opam_file = CheckUpdate.opam_file dirs v0 in
                        let opam = CopamOpamFile.parse opam_file in
                        List.iter (fun item ->
                            match item with
                            | CopamTypes.Variable
                                (_, field, v) when field = field_name ->
                              begin match v with
                                  CopamTypes.String (_,s)
                                | CopamTypes.List (_,
                                                   [ CopamTypes.String (_,s)]) ->
                                  add_content := Some
                                      (Printf.sprintf "%s: %S" field_name s, v0);
                                | CopamTypes.List (_, list) ->
                                  if List.for_all (function
                                      | CopamTypes.String _ -> true
                                      | _ -> false )list then begin
                                    let b = Buffer.create 1000 in
                                    Printf.bprintf b "%s: [\n" field_name;
                                    List.iter  (function
                                        | CopamTypes.String (_, s) ->
                                          Printf.bprintf b "%S\n" s
                                        | _ -> assert false) list;
                                    Printf.bprintf b "    ]";
                                    add_content := Some (Buffer.contents b, v0)
                                  end
                                | _ -> ()
                              end
                            | _ -> ()
                          ) opam.CopamTypes.file_contents;
                    ) solvers.(num)
                in

                let field_name =
                  match num with
                  | 23 -> "maintainer"
                  | 25 -> "authors"
                  | 35 -> "homepage"
                  | 36 -> "bug-reports"
                  | 37 -> "dev-repo"
                  | _ ->
                    Printf.eprintf "auto-fix error %d not implemented\n%!" num;
                    assert false
                in
                find_field field_name;
                begin
                  match !add_content with
                  | None ->
                    Printf.eprintf "Warning: could not use %s field from %s\n%!" field_name p.package_name
                  | Some _ -> ()
                end;
                match !add_content with
                | None -> ()
                | Some (content, v0) ->
                  List.iter (fun v ->
                      let opam_file = CheckUpdate.opam_file dirs v in
                      let old_content = File.string_of_file opam_file in
                      File.file_of_string (opam_file ^ ".orig") old_content;
                      let new_content =
                        Printf.sprintf "%s\n  (* auto-fix field from %s *)\n%s\n"
                          old_content v0.version_name content
                      in
                      File.file_of_string opam_file new_content
                    ) missing.(num)
        ) can_auto_fix;
    ) c.packages;
  ()


let export dirs c =

  load_all_lint dirs c;

  let date = CheckDate.TIMESTAMP.current () in
  let export_file = Filename.concat dirs.report_dir
      (Printf.sprintf "%s-%s-lint.export"
         date
         c.commit_name) in

  let oc = open_out export_file in

  Printf.fprintf oc "commit:%s\n" c.commit_name;
  Printf.fprintf oc "switch:lint\n";
  Printf.fprintf oc "date:%f\n" (Unix.time());

  StringMap.iter (fun package_name p ->
      Printf.fprintf oc "package:%s\n" package_name;
      StringMap.iter (fun version_name v ->
          match v.version_lint with
          | None -> ()
          | Some { lint_warnings; lint_errors } ->
            Printf.fprintf oc "version:%s\n" version_name;
            List.iter (fun (num, msg) ->
                Printf.fprintf oc "warning:%d:%s\n" num msg) lint_warnings;
            List.iter (fun (num, msg) ->
                Printf.fprintf oc "error:%d:%s\n" num msg) lint_errors;
            Printf.fprintf oc "lint:end\n";
        ) p.package_versions;
    ) c.packages;
  Printf.fprintf oc "export:end\n";

  close_out oc;
