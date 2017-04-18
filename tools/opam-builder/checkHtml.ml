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

open CheckTypes
open StringCompat
open CopamInstall

let style_url = "http://opam.ocamlpro.com/builder/html/style.css"

(* Check the LICENSE file before modifying these two lines: *)
let link_url = "http://www.ocamlpro.com/"
let logo_url = "http://opam.ocamlpro.com/images/opam-builder-ocamlpro-inria-irill.png"

let begin_html oc title =
  Printf.fprintf oc "<!doctype html>\n\
<html xml:lang=\"en\" xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"en\">\n\
<head>\
<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"/>\
<link rel=\"stylesheet\" href=\"%s\"/>\
<title>%s</title></head>\
<body>\n\
<a href=\"%s\"><img class=\"attribution\" id=\"attribution\" src=\"%s\" /></a>\
                    "
    style_url
    title
    link_url
    logo_url

let end_html oc =
  Printf.fprintf oc "\n</body></html>"

let print_commit_report st c =
  (* Let's print a simple html document, for now, showing the commit
       information. *)

  let oc = open_out (Filename.concat st.dirs.report_dir
                                     (c.commit_name ^ ".html")) in

  Printf.fprintf oc "<h1>Commit %s</h1>\n" c.commit_name;
  (*  Printf.fprintf oc "<p>Checked Packages: %d</p>\n" !checked_packages; *)
  (*  Printf.fprintf oc "<p>Checked Versions: %d</p>\n" !checked_versions; *)
  Printf.fprintf oc "<table>\n";

  Printf.fprintf oc "<tr>\n";
  Printf.fprintf oc "  <td>Package</td>\n";
  Printf.fprintf oc "    <td>Lint</td>\n";
  Printf.fprintf oc "  <td>%s</td>\n" st.sw.sw_name;
  Printf.fprintf oc "</tr>\n";

  StringMap.iter (fun package_name p ->
      Printf.fprintf oc "<tr>\n";
      Printf.fprintf oc "  <td>%s</td>\n" package_name;
      Printf.fprintf oc "  <td></td>\n";
      begin
        match p.package_status with
        | None -> ()
        | Some s ->
           match s.s_status with
           | NotInstallable ->
              Printf.fprintf oc
                             "  <td style=\"background-color: red;\">BAD</td>\n"
           | NotAvailable ->
              Printf.fprintf oc "  <td></td>\n"
           | ExternalError ->
              Printf.fprintf oc
                             "  <td style=\"background-color: orange;\">ERR</td>\n"
           | Installable packages ->
              Printf.fprintf oc
                             "  <td style=\"background-color: green;\">%d</td>\n"
                             (List.length packages)
      end;
      Printf.fprintf oc "</tr>\n";

      StringMap.iter (fun version_name v ->
          Printf.fprintf oc "<tr>\n";
          Printf.fprintf oc "  <td>%s</td>\n" version_name;

          begin
            match v.version_lint with
            | None ->
               Printf.fprintf oc
                              "  <td style=\"background-color: white;\"></td>\n"
            | Some { lint_warnings; lint_errors } ->
               match lint_warnings, lint_errors with
               | [], [] ->
                  Printf.fprintf oc
                                 "  <td style=\"background-color: green;\"></td>\n"
               | _ ->
                  Printf.fprintf oc
                                 "  <td style=\"background-color: red;\">%d</td>\n"
                                 (List.length lint_errors)
          end;


          begin
            match v.version_status with
            | None -> ()
            | Some s ->
               match s.s_status with
               | NotInstallable ->
                  Printf.fprintf oc
                                 "  <td style=\"background-color: red;\">BAD</td>\n"
               | NotAvailable ->
                  Printf.fprintf oc "  <td></td>\n"
               | ExternalError ->
                  Printf.fprintf oc
                                 "  <td style=\"background-color: orange;\">ERR</td>\n"
               | Installable packages ->
                  Printf.fprintf oc
                                 "  <td style=\"background-color: green;\">%d</td>\n" (List.length packages)

          end;
          Printf.fprintf oc "</tr>\n";

        ) p.package_versions;
    ) c.packages;

  Printf.fprintf oc "</table>\n";
  close_out oc;

  ()
