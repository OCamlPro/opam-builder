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
open CheckTypes.OP

let cache_dir_basename = "builder.cache"
let reports_dir_basename = "builder.reports"
let switch_file_basename = "builder.switch"

let current_dir = Sys.getcwd ()

let reports_dir_fullname = current_dir // reports_dir_basename
let cache_dir_fullname = current_dir // cache_dir_basename
let switch_file_fullname = current_dir // switch_file_basename

let fatal fmt =
  Printf.kprintf (fun msg ->
      Printf.eprintf "opam-builder error: %s\n%!" msg;
      exit 2
    ) fmt

let read_switch () =
  match FileString.read_lines switch_file_basename with
  | [| switch |] -> switch
  | _ ->
     fatal "File %S does not contain the current switch"
           switch_file_basename


let write_switch switch =
  FileString.write_lines switch_file_basename [| switch |]

let check_in_tree () =
  List.iter (fun (kind,file) ->

      if not (Sys.file_exists file) then
        fatal "this action must be performed inside a switch\n\
               (where the %s %S is present)" kind file
    )
            [
              "file", switch_file_basename;
              "dir", cache_dir_basename;
              "dir", reports_dir_basename;
              "dir", ".opam";
              "dir", ".git";
            ]

let parse_build_lines msg lines =
  let lines = Array.map (fun line ->
                  OcpString.split line ':') lines in
  let rec iter actions lines =
    match lines with
    | [] -> List.rev actions
    | [ "disabled"; "failed"; version_name ] :: lines ->
       let actions = (version_name, DisabledFailed) :: actions in
       iter actions lines
    | [ "disabled"; "skip"; version_name ] :: lines ->
       let actions = (version_name, DisabledSkip) :: actions in
       iter actions lines
    | [ "begin"; ("build"|"install" as kind);
        build_report_begin_time; version_name ] ::
        [ "hash"; build_report_hash ] ::
          [ "depends"; depends ] ::
            [ "depopts"; depopts ] ::
              lines ->
       let build_report_depends = OcpString.split depends ',' in
       let build_report_depopts = OcpString.split depopts ',' in
       let action = {
           build_report_begin_time;
           build_report_hash;
           build_report_end_time = build_report_begin_time;
           build_report_depends;
           build_report_depopts;
           build_report_snap_errors = [];
           build_report_result = ActionUnknown;
         } in
       let actions = (version_name,
                      match kind with
                      | "build" -> Build action
                      | "install" -> Install action
                      | _ -> assert false) :: actions in
       iter_action action actions lines

    | line :: lines ->
       Printf.eprintf "%s, cannot parse line: \n" msg;
       Printf.eprintf "%s\n%!" (String.concat ":" line);
       raise InvalidFile

  and iter_action action actions lines =
    match lines with

    | [] ->
       Printf.eprintf "%s, unexpected end of file \n" msg;
       raise InvalidFile

    | [ "archive" ; "reused" ] :: lines ->
       action.build_report_result <- ActionReused;
       iter_action action actions lines

    | [ "action-failed"; duration ] :: lines ->
       action.build_report_result <- ActionFailed duration;
       iter_action action actions lines

    | [ "end"; ("build"|"install" as _kind);
        build_report_end_time; version_name ] :: lines ->
       action.build_report_end_time <- build_report_end_time;
       iter actions lines

    | [ "archive"; "created"; duration ] :: lines ->
       action.build_report_result <- ActionInstalled duration;
       iter_action action actions lines

    | [ "snap"; "error"; "modified-file"; file ] :: lines ->
       action.build_report_snap_errors <-
         ModifiedFile file :: action.build_report_snap_errors;
       iter_action action actions lines

    | [ "snap"; "error"; "removed-file"; file ] :: lines ->
       action.build_report_snap_errors <-
         RemovedFile file :: action.build_report_snap_errors;
       iter_action action actions lines

    | line :: lines ->
       Printf.eprintf "%s, cannot parse action line: \n" msg;
       Printf.eprintf "%s\n%!" (String.concat ":" line);
       raise InvalidFile

  in
  iter [] (Array.to_list lines)

let read_build_file file =
  let lines = FileString.read_lines file in
  parse_build_lines (Printf.sprintf "File %S" file) lines
