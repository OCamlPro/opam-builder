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



(* Todo:  This system could become a quality control for opam-repository
   if we add:
   * when failing anywhere in the process of backuping an archive, we should
   generate a file issue-$version.txt, containing the problem found with
   version.
   * if the problem disappears, we should remove that file.

   We could use these files to monitor problems in the repository.
   * We should try to download all files once a week.
*)

open StringCompat
open CopamMisc (* command, ignore_bool *)

let last_commit_cmd = "git rev-parse --short HEAD > last-commit.txt"

let branch_missing_checksum = false

let parse_url_file package version dirname =
  let url_file = Filename.concat dirname "url" in
      if not (Sys.file_exists url_file) then begin
      (* CopamIssues.issue package version "no-url" [ "No 'url' file" ] *)
        raise Exit;
      end;
      let idents =
        try
          CopamUrlFile.load url_file
        with _ ->
          CopamIssues.issue package version "bad-format" [ "Cannot parse 'url' file" ]
      in

      let url = try
                  List.assoc "archive" idents
        with Not_found ->
          try
            List.assoc "http" idents
          with Not_found ->
          CopamIssues.issue package version "no-archive" [ "File 'url' has no 'archive' header" ]
      in
      (url_file, idents, url)

let iter_download_archives () =
  Printf.eprintf "iter_download_archives...\n%!";
  CopamRepo.iter_packages "." (fun package version dirname ->
      (*      Printf.eprintf "    %s:\n%!" version; *)

    let url_file, idents, url = parse_url_file package version dirname in


      let checksum = String.lowercase (
        try
          List.assoc "checksum" idents
        with Not_found ->
          CopamIssues.issue package version "no-checksum" [ "File 'url' has no 'checksum' header" ]
      ) in

      if not (
        String.length checksum = 32 &&
          (let all_good = ref true in
           for i = 0 to 31 do
             match checksum.[i] with
               'a'..'f' | '0' .. '9' -> ()
             | _ -> all_good := false
           done;
           !all_good)
      ) then
        CopamIssues.issue package version "bad-checksum" [ "Checksum is not correct" ];

    let backup_file = Printf.sprintf "backup/%c/%c/%c/%s"
        checksum.[0]
        checksum.[1]
        checksum.[2]
        checksum in

      if Sys.file_exists backup_file then raise Exit; (* we are done *)

      if not (
        Printf.kprintf command "mkdir -p %s" (Filename.dirname backup_file)
      )
      then begin
        Printf.eprintf "Error: could not create dir %s\n%!" backup_file;
        raise Exit
      end;

      if not (
        Printf.kprintf command
          "wget -o log.%s -O archive.%s --tries=1 --timeout=5 %s" version version url
      ) then begin
        let lines = try FileLines.read_file ("log." ^ version) with
            _ -> [ "???" ] in
        CopamIssues.issue package version "download-failed" (
          Printf.sprintf "Could not download %s:" url
                                                ::
            lines)
        end;

      Printf.eprintf "Archive of %s downloaded\n%!" version;
      if not ( Printf.kprintf command
                 "md5sum archive.%s > checksum" version) then begin
        Printf.eprintf "Error: could not md5sum archive.%s\n%!" version;
        raise Exit
      end;

      let md5sum = FileString.read_file "checksum" in
      let md5sum = String.sub md5sum 0 32 in
      if md5sum <> checksum then begin
        CopamIssues.issue package version "wrong-checksums" [
          Printf.sprintf "Checksums differ for %s:" version;
          Printf.sprintf "   %S (expected)" checksum;
          Printf.sprintf "   %S (computed)" md5sum;
        ]
      end;

      if not (Printf.kprintf command "mv archive.%s %s"
                version backup_file) then begin
        Printf.eprintf "Error: could not copy archive.%s to %s\n" version backup_file;
        raise Exit
      end;

      let oc = open_out (backup_file ^ ".url") in
      Printf.fprintf oc "url: %s\n" url;
      Printf.fprintf oc "version: %s\n" version;
      close_out oc;

      Printf.eprintf "%s archived\n%!" version;
      (try Sys.remove ("log."^version) with _ -> ());

      (*
      if branch_missing_checksum then
          try
            let _package = Filename.basename url_file in
            let url = List.assoc "archive" idents in
      *)
    );
  Printf.eprintf "iter_download_archives...done\n%!";
  ()


let fix_crcs versions =
  CopamRepo.iter_packages "." (fun package version dirname ->
    if StringSet.mem version !versions then begin

      versions := StringSet.remove version !versions;

      let url_file, idents, url = parse_url_file package version dirname in
      (try Sys.remove "archive" with _ -> ());
      ignore (
        Printf.kprintf command
          "git branch -D fix-checksum-%s" version);
      if
        Printf.kprintf command "git checkout master" &&
          Printf.kprintf command "git checkout ." &&
          Printf.kprintf command "git checkout -b fix-checksum-%s" version &&
          Printf.kprintf command
          "wget -o log.%s --timeout=10 %s -O archive.%s.tar.gz"
          version url version &&
          Printf.kprintf command
          "md5sum archive.%s.tar.gz > checksum" version then begin
            let md5sum = FileString.read_file "checksum" in
            let md5sum = String.sub md5sum 0 32 in
            let oc = open_out url_file in
            List.iter (fun (s,v) ->
              if s <> "checksum" then
                Printf.fprintf oc "%s: %S\n" s v
            ) idents;
            Printf.fprintf oc "checksum: %S\n" md5sum;
            close_out oc;
            if not (
              Printf.kprintf command "git add %s" url_file &&
                Printf.kprintf command
                "git commit %s -m 'fix checksum for %s'" url_file version &&
                Printf.kprintf command
                "git push -f --set-upstream origin fix-checksum-%s" version
            ) then exit 2;
          end
    end
  );
  if !versions <> StringSet.empty then begin
    Printf.eprintf "Error: some versions were not found:\n";
    StringSet.iter (fun s-> Printf.eprintf "  %s\n%!" s) !versions;
    exit 2
  end;
  exit 0

let _ =
  if not (Sys.file_exists "packages") ||
    not (Sys.file_exists ".git")
  then begin
    Printf.eprintf "opam-archive should be run at the root of an opam-repository clone.\n%!";
    exit 2
  end;
  let fix_crc = ref false in
  let versions = ref StringSet.empty in
  let arg_list = Arg.align [
    "-fix-crc", Arg.Set fix_crc, " Fix the checksums of packages given in argument";
  ] in
  let arg_anon s = versions := StringSet.add s !versions in
  let arg_usage = "opam-archive [OPTIONS] : backup all archives of an opam-repository" in
  Arg.parse arg_list arg_anon arg_usage;

  CopamIssues.init ();

  if !fix_crc then begin

    fix_crcs versions;
  end;

  let current_commit = ref "" in
  while true do
    if command "git checkout master" &&
      command "git pull ocaml master" &&
      command last_commit_cmd then begin

        let commit = FileString.read_file "last-commit.txt" in
        Sys.remove "last-commit.txt";

        if commit <> !current_commit then begin
          iter_download_archives ();
          current_commit := commit;

          CopamIssues.rotate();

        end

      end;
    Unix.sleep 60;
  done
