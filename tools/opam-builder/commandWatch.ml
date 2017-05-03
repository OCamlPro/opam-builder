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

let arg_on_commit = ref []
let arg_delay = ref 60
let arg_branch = ref "master"
let arg_remote = ref "ocaml"

let generic_args = [
    "--delay", Arg.Int (fun s -> arg_delay := s),
    "DELAY Check every DELAY seconds";
    "--branch", Arg.String (fun s -> arg_branch := s),
    Printf.sprintf "BRANCH Watch branch BRANCH (default is %S)" !arg_branch;
    "--remote", Arg.String (fun s -> arg_remote := s),
    Printf.sprintf "REMOTE Watch remote REMOTE (default is %S)" !arg_remote;
  ]

let args = [
    "--on-commit", Arg.Rest (fun s -> arg_on_commit := s :: !arg_on_commit),
    "REST Command called on each new commit, with";
    "", Arg.Unit (fun () -> ()), "    commit passed as last argument";
  ] @ generic_args

let watch f =

  if not (Sys.file_exists ".git") then
    CheckTree.fatal "this command must be run in a .git repo";

  let last_commit = ref "reboot" in
  while true do
    if
      Printf.kprintf CommandScan.command "git checkout %s" !arg_branch &&
        Printf.kprintf CommandScan.command "git pull %s %s" !arg_remote !arg_branch then
      let commit = CommandScan.get_last_commit () in
      if !last_commit <> commit then begin

          f commit;

          last_commit := commit;
        end;

    Unix.sleep !arg_delay
  done

let action args =
  let args =
    match !arg_on_commit with
      [] -> [ "echo"; "NEW"; "COMMIT" ]
    | args -> args
  in
  watch (fun commit ->
      let args = List.rev (commit :: args) in
      let args = List.map (function
                             "SELF" -> Sys.argv.(0)
                           | s -> s) args in
      ignore ( CommandScan.command (String.concat " " args) : bool );
    )
