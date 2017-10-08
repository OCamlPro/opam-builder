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

let arg_replace_commit_tree = ref false
let arg_watch = ref false
let arg_static = ref true
let arg_dynamic = ref true

let args = [
    "--replace-commit-tree", Arg.Set arg_replace_commit_tree,
    " Replace already existing commit tree";
    "--watch", Arg.Set arg_watch,
    " Keep running";
    "--no-static", Arg.Clear arg_static,
    " Do not replace static files (HTML/CSS/JS)";
    "--no-dynamic", Arg.Clear arg_dynamic,
    " Do not replace dynamic files (JSON files)";
  ]

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

let generate_json dirs =

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


  let replace_commit_tree = !arg_replace_commit_tree in
  let generate_json file title cs =
    CheckJson.of_commits  ~replace_commit_tree file title cs;
    Printf.eprintf "File %S generated\n%!" file
  in

  (* keep only the last 6 commits for each switch *)
  let main_cs = ref [] in

  StringMap.iter (fun switch (commits, list) ->
      StringMap.iter (fun _ list ->
          (* sort each list of files in decreasing times, for a commit *)
          list := List.rev (List.sort compare !list)
        ) !commits;
      (* sort each list of commits in decreasing times, for a switch.
       it requires the files to be sorted by date. *)
      let commits = head_n 6 (List.rev (List.sort compare !list)) in

      Printf.eprintf "For switch %S,\n%!" switch;
      (* preload them *)
      let switch_cs =
        List.map (fun list ->
            match !list with
              [] -> assert false
            | (_, file) :: _ ->
               Printf.eprintf "   using %S\n%!" file;
               let (c, _stats) = CheckIO.load file in
               c) commits
      in
      generate_json (Printf.sprintf "%s.json" switch)
                    (Printf.sprintf "Switch %s" switch)
                    switch_cs;
      match switch_cs with
        [] -> assert false
      | c :: _ -> main_cs := c :: !main_cs
    ) !switches;

  (* switches are iterated in alphabetic order,
     but main_cs is appended-to so it has to be reversed
     to show switches in order *)
  main_cs := List.rev !main_cs;

  generate_json "opam-builder.json" "All Switches" !main_cs;

  Gc.major();
  Gc.compact();

  ()

let readdir dir = try Sys.readdir dir with _ -> [||]

let action dirs =


  if !arg_static then begin
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
    end;

  if !arg_dynamic then begin
      if !arg_watch then CommandSwitch.save_pid ();
      let rec iter prev_files =

        let report_dirs = ref [] in
        List.iter (fun dir ->
            let files = readdir dir in
            Array.iter (fun file ->
                let report_dir = dir // file //
                                   "builder.reports" in
                if Sys.file_exists report_dir then
                  report_dirs := report_dir :: !report_dirs
              ) files
          ) dirs;

        let new_files = List.map readdir !report_dirs in
        if prev_files <> new_files then
          generate_json !report_dirs;

        if !arg_watch then begin
            Unix.sleep 5;
            iter new_files
          end
      in iter []
    end
