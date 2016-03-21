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



let opam_lint = "COLUMNS=10000 opam.dev lint"

type t = int * string list

let lint opam_file =
  let log_file = "check-lint.log" in
  (try Sys.remove log_file with _ -> ());
  let cmd = Printf.sprintf
    "%s %s > %s 2>&1" opam_lint opam_file log_file in
  Printf.eprintf "cmd=%s\n%!" cmd;
  let exitcode = Sys.command cmd in
  let lines = try File.lines_of_file log_file with _ -> [] in
  (try Sys.remove log_file with _ -> ());
  (exitcode, lines)

let save lint_file (exitcode, lines) =
  let oc = open_out lint_file in
  Printf.fprintf oc "exit-code:%d\n" exitcode;
  List.iter (fun line ->
    Printf.fprintf oc "%s\n" line) lines;
  close_out oc

let load lint_file =
  let lines = File.lines_of_file lint_file in
  match lines with
  | [] -> 99, []
  | line :: lines ->
    let before, after = OcpString.cut_at line ':' in
    if before = "exit-code" then
      try
        int_of_string after, lines
      with _ ->
        99, lines
    else
      99, lines
