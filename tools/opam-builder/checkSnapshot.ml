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

  type t = {
    files : (string * kind) list;
  }
  and kind =
    | File of file
    | Dir of t
    | Link of string
    | Fifo
    | Other
  and file = {
    file_size : int;
    file_mtime : float;
  }

  exception FileRemoved of string
  exception FileChanged of string

  let rec make dir base ignored =
    let files = Sys.readdir dir in Array.sort compare files;
    let snapshot_files = ref [] in
    Array.iter (fun file ->
      if not (StringSet.mem (Filename.concat base file) ignored) then
        let kind =
          let filename = Filename.concat dir file in
          let st = Unix.lstat filename in
          match st.Unix.st_kind with
          | Unix.S_REG ->
            File {
              file_size = st.Unix.st_size;
              file_mtime = st.Unix.st_mtime;
            }
          | Unix.S_DIR ->
            Dir (make filename (Filename.concat base file) ignored)
          | Unix.S_LNK ->
            let link = Unix.readlink filename in
            Link link
          | Unix.S_FIFO -> Fifo
          | Unix.S_CHR
          | Unix.S_BLK
          | Unix.S_SOCK
            -> Other
        in
        snapshot_files := (file, kind) :: !snapshot_files
      ) files;
    { files = List.rev !snapshot_files }

  let make dir ignored = make dir "." ignored

  let save filename t =
    let oc = open_out filename in
    let rec save t =
      List.iter (fun (file, kind) ->
        match kind with
        | Link link ->
          Printf.fprintf oc "LINK\n%s\n" file;
          Printf.fprintf oc "%s\n" link;
        | Dir t ->
          Printf.fprintf oc "DIR\n%s\n" file;
          save t;
        | File f ->
          Printf.fprintf oc "FILE\n%s\n" file;
          Printf.fprintf oc "%d\n" f.file_size;
          Printf.fprintf oc "%f\n" f.file_mtime;
        | Fifo ->
          Printf.fprintf oc "FIFO\n%s\n" file
        | Other ->
          Printf.fprintf oc "OTHER\n%s\n" file
      ) t.files;
      Printf.fprintf oc "END\n";
    in
    save t;
    close_out oc

  let lines_of_file filename =
    let ic = open_in filename in
    let lines = ref [] in
    try
      while true do
        lines := (input_line ic) :: !lines
      done;
      assert false
    with _ ->
      close_in ic;
      List.rev !lines

  let load filename =
    let lines = lines_of_file filename in
    let rec load lines files =
      match lines with
      | "END" :: rem ->
        { files = List.rev files }, rem
      | "LINK" :: file :: link :: rem ->
        load rem ( (file, Link link) :: files )
      | "DIR" :: file :: rem ->
        let t, rem = load rem [] in
        load rem ( (file, Dir t) :: files )
      | "FILE" :: file :: file_size :: file_mtime :: rem ->
        let f = {
          file_size = int_of_string file_size;
          file_mtime = float_of_string file_mtime;
        } in
        load rem ( (file, File f) :: files )
      | "FIFO" :: file :: rem ->
        load rem ( (file, Fifo) :: files)
      | "OTHER" :: file :: rem ->
        load rem ( (file, Other) :: files)
      | _ -> assert false
    in
    let t,rem = load lines [] in
    assert (rem = []);
    t

  (* For now, we only support adding files, not removing them ! *)
  let diff after before = (* TODO *)
    let rec diff_files after before files =
      match after, before with
      | _, [] -> List.rev files @ after
      | [], (file,_) :: _ -> raise (FileRemoved file)
      | (file1, kind1) :: after1,
        (file2, kind2) :: before2 ->
        if file1 < file2 then
          diff_files after1 before ( (file1, kind1) :: files )
        else
        if file1 = file2 then
          let files =
            match kind1, kind2 with
            | File f1, File f2 ->
              if f1 <> f2 then
              (* (file1, kind1) :: files *)
                raise (FileChanged file1) (* file content changed *)
              else files
            | Dir { files = files1 }, Dir { files = files2 } ->
              let diff = diff_files files1 files2 [] in
              if diff <> [] then
                (file1, Dir { files = diff } ) :: files
              else files

            | Link link1, Link link2 ->
              (* TODO: check that tar can change links *)
              if link1 <> link2 then (* (file1, kind1) :: files *)
                raise (FileChanged file1) (* link content changed *)
              else files
            | Fifo, Fifo -> files
            | _ -> raise (FileChanged file1) (* type of file changed *)
          in
          diff_files after1 before2 files
        else
          raise (FileRemoved file2)
    in
    { files = diff_files after.files before.files [] }

(* We skip other files *)
  let rec copy_files prefix snap destdir =
    List.iter (fun (file, kind) ->
      let src_file = Filename.concat prefix file in
      let dst_file = Filename.concat destdir file in
      match kind with
      | Link link ->
        if Filename.is_relative link then
          Unix.symlink link dst_file
        else assert false
      | Dir snap ->
        Unix.mkdir dst_file 0o755; (* TODO: add perms in snapshots *)
        copy_files src_file snap dst_file
      | File _ | Fifo ->
        let exit =  (* call cp for now, because it keeps perms *)
          Printf.kprintf Sys.command "cp -R '%s' '%s'" src_file dst_file
        in
        assert (exit = 0)
      | Other -> ()
(*
        let s = File.string_of_file src_file in
        File.file_of_string dst_file s  (* TODO: add perms in snapshots *)
*)
    ) snap.files

  let rec remove_files prefix snap =
    List.iter (fun (file, kind) ->
      let src_file = Filename.concat prefix file in
      match kind with
      | Link _
      | File _
      | Fifo | Other
        ->
        Sys.remove src_file
      | Dir snap ->
        remove_files src_file snap;
        (try Unix.rmdir src_file with _ -> ())
    ) snap.files

  let clean_files dir ignored sn_before =
    let sn_after = make dir ignored in
    let diff = diff sn_after sn_before in
    remove_files dir diff



      (*
end


  let opam_ignored_files =
    let set = ref StringSet.empty in
    List.iter (fun file ->
      set := StringSet.add file !set
    ) [
      "./backup"; "./build"; "./config";
      "./install"; "./overlay"; "./packages.dev";
      "./installed"; "./installed_roots"; "./state"; "./environment";
      "./reinstall"; "./pinned";
    ];
    !set
      *)
