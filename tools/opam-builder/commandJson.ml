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
open CheckTypes.OP

let args = []

let rec head_n n list =
  if n <= 0 then []
  else
    match list with
    | [] -> []
    | x :: tail -> x :: (head_n (n-1) tail)

let check_num_prefix file nchars =
  try
    for i = 0 to nchars - 1 do
      match file.[i] with
      | '0'..'9' -> ()
      | _ -> raise Exit
    done;
    true
  with _ -> false

let find_most_recent_commits dir extension =
  let found = ref [] in
  let files = Sys.readdir dir in
  Array.iter (fun file ->
      if String.length file > 12 && Filename.check_suffix file extension &&
           check_num_prefix file 12
      then
        found := file :: !found
    ) files;
  head_n 6 (List.sort compare !found)

let action dirs =

  let extension = ".dump" in
  let switches = ref StringMap.empty in
  let get_switch switch =
    try
      StringMap.find switch !switches
    with Not_found ->
      let x = (ref StringMap.empty, ref []) in
      switches := StringMap.add switch x !switches;
      x
  in

  (* scan dirs, and find matching .dump files *)
  List.iter (fun dir ->
      let files = Sys.readdir dir in
      Array.iter (fun file ->
          if String.length file > 12 && Filename.check_suffix file extension &&
               check_num_prefix file 12
          then
            let date, rem = OcpString.cut_at file '-' in
            let commit, rem = OcpString.cut_at rem '-' in
            let switch = Filename.chop_suffix rem extension in

            let commits, list = get_switch switch in

            try
              let list = StringMap.find commit !commits in
              list := (date, dir // file) :: !list
            with Not_found ->
              let x = ref [ date, dir // file ] in
              commits := StringMap.add commit x !commits;
              list := x :: !list
        ) files
    ) dirs;

  (* keep only the last 6 commits for each switch *)
  let switches =
    StringMap.map (fun (commits, list) ->
        StringMap.iter (fun _ list ->
            (* sort each list of files in decreasing times, for a commit *)
            list := List.rev (List.sort compare !list)
          ) !commits;
      (* sort each list of commits in decreasing times, for a switch.
       it requires the files to be sorted by date. *)
        let commits = head_n 6 (List.rev (List.sort compare !list)) in

        (* preload them *)
        List.map (fun list ->
            match !list with
              [] -> assert false
            | (_, file) :: _ ->
               let (c, _stats) = CheckIO.load file in
            c) commits
      ) !switches
  in

  let main_cs = (* main page: last commit per switch *)
    let cs = ref [] in
    StringMap.iter (fun switch switch_cs ->
        match switch_cs with
          [] -> assert false
        | c :: _ -> cs := c :: !cs
      ) switches;
    List.rev !cs
  in

  let generate_json file cs =
    CheckJson.of_commits file cs;
    Printf.eprintf "File %S generated\n%!" file
  in
  generate_json "opam-builder.json" main_cs;

  StringMap.iter (fun switch switch_cs ->
      generate_json (Printf.sprintf "%s.json" switch) switch_cs
    ) switches;

  List.iter (fun file ->
      FileString.write_file file
                            (List.assoc ("files" // file) CheckFiles.files);
      Printf.eprintf "File %S generated\n%!" file;
    ) [
              "opam-builder.html";
              "opam-builder.css";
              "opam-builder.js";
              "opam-builder-ocamlpro-inria-irill.png";
            ];
  ()