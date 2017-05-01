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

(* Tested: works, but it will fail to safely keep files that are
   referenced from broken .build files.
   The 7 days threshold makes only sense if we want to keep older
   versions, which is not required.
 *)

open CheckTypes.OP

type archive = {
  filename : string;
  mutable used : int;
  mutable occur : int;
}

let clean ?(fake=false) ?(ndays_threshold=7) ~cache_dir ~switch =

  Printf.eprintf "GC on switch: %s\n%!" switch;
  let archives = Hashtbl.create 111111 in
  let register_archive version_dir file_base =
    let filename = version_dir // file_base in
    try
      let a = Hashtbl.find archives filename in
      a.occur <- a.occur + 1
    with Not_found ->
      Hashtbl.add archives filename
                  { filename; used = 0; occur = 1 }
  in

  let need_archive version_name hash =
    let package_name, _ = OcpString.cut_at version_name '.' in
    let file_base = Printf.sprintf "%s-%s" hash switch in
    let filename =
      cache_dir // package_name // version_name // file_base in
    Printf.eprintf "need_archive %s\n%!" filename;
    try
      let a = Hashtbl.find archives filename in
      a.used <- a.used + 1
    with Not_found ->
      Hashtbl.add archives filename
                  { filename; used = 1; occur = 0 }
  in
  let read_build_file version_dir file =
    let filename = version_dir // file in
    (* Printf.eprintf "  processing %s\n%!" filename; *)
    let version_name_ref = ref None in
    let hash_ref = ref None in
    (* Printf.eprintf "reading %s\n%!" filename; *)
    FileString.iter_lines (fun line ->
        (* Printf.eprintf "   LINE %s\n%!" line; *)
        let segments = OcpString.split line ':' in
        match segments with
        | [ "begin" ; ("build" | "install") ; _time ; version_name ] ->
           version_name_ref := Some version_name
        | [ "hash" ; hash ] ->
           hash_ref := Some hash
        | "archive" :: "reused" :: _
          | "archive" :: "created" :: _
          ->
           begin
             match !version_name_ref, !hash_ref with
             | Some version_name, Some hash ->
                need_archive version_name hash
             | _ ->
                Printf.eprintf "  Error: version_name or hash missing\n%!"
           end
        | [ "end"; ( "build"|"install") ; _delay; version_name ] ->
           version_name_ref := None;
           hash_ref := None
        | _ -> ()
      ) filename
  in

  let clean_version package_name version_name version_dir =
    let files = Sys.readdir version_dir in

    Array.iter (fun file ->
        if Filename.check_suffix file ".build" then
          read_build_file version_dir file
        else
          if Filename.check_suffix file "-build.tar.gz" then
            register_archive version_dir (Filename.chop_suffix file "-build.tar.gz")
      ) files
  in

  let packages = Sys.readdir cache_dir in
  Array.iter (fun package_name ->
      let package_dir = cache_dir // package_name in
      if Sys.is_directory package_dir then
        let versions = Sys.readdir package_dir in
        Array.iter (fun version_name ->
            let version_dir = package_dir // version_name in
            if Sys.is_directory version_dir then
              clean_version package_name version_name version_dir
          ) versions;
        Printf.eprintf "%s done\n%!" package_name;
    ) packages;

  let day_secs = 3600 * 24 in
  let current_time = Unix.gettimeofday () in
  let nused = ref 0 in
  let nunused = ref 0 in
  let nremoved = ref 0 in
  Hashtbl.iter (fun _ a ->
      let base_archive = a.filename in
      let archive = base_archive ^ "-build.tar.gz" in
      let mtime =
        try
          (Unix.stat archive).Unix.st_mtime
        with _ -> current_time +. 100_000.
      in
      let ndays = int_of_float (current_time -. mtime) /  day_secs in
      Printf.eprintf "%s: %d uses %d occurs (%d days old).\n%!"
                     a.filename
                     a.used
                     a.occur
                     ndays;
      if a.used>0 then begin
          incr nused;
        end else begin
          incr nunused;
          if ndays >= ndays_threshold then begin
              incr nremoved;
              List.iter (fun ext ->
                  let filename = a.filename ^ ext in
                  Printf.eprintf " Removing %s\n%!" filename;
                  if not fake then
                    try Sys.remove filename with _ -> ()
                ) [
                          "-build.depends";
                          "-build.snap";
                          "-build.tar.gz";
                          "-install.depends";
                          "-install.snap";
                          "-install.tar.gz";
                        ]
            end
        end
    ) archives;
  Printf.eprintf "%d used archives\n%!" !nused;
  Printf.eprintf "%d unused archives\n%!" !nunused;
  Printf.eprintf "%d removed archives\n%!" !nremoved;

  ()
