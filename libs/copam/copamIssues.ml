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



open CopamMisc

let issue package version title lines =
  let filename = Printf.sprintf "issues/new/issue-%s-%s.html" title version in
  let oc = open_out filename in
  Printf.fprintf oc "<h1>Issue %s with %s:</h1>\n" title version;
  Printf.fprintf oc "<a href=\"https://github.com/ocaml/opam-repository/tree/master/packages/%s/%s/\">%s</a>\n" package version version;
  Printf.fprintf oc "<pre>\n";
  List.iter (fun line ->
    Printf.fprintf oc "  %s\n" line) lines;
  Printf.fprintf oc "</pre>\n";
  close_out oc;
  Printf.eprintf "New issue %s\n%!" filename;
  raise Exit

let init () =
  List.iter (fun dirname ->
    if not (Sys.file_exists dirname && Sys.is_directory dirname) then begin
      Unix.mkdir dirname 0o775;
    end;
  ) [ "issues"; "issues/new"; "issues/old" ]

let rotate () =

  (* move old issues to issues/old/ *)
  let issues = Sys.readdir "issues" in
  Array.iter (fun file ->
    match file with
      "old" | "new" -> ()
    | _ ->
      ignore_bool (
        Printf.kprintf command
          "mv issues/%s issues/old/" file)
  ) issues;

          (* move new issues to issues/ *)
  let issues = Sys.readdir "issues/new" in
  Array.iter (fun file ->
    ignore_bool (
      Printf.kprintf command
        "mv issues/new/%s issues/" file)
  ) issues;
