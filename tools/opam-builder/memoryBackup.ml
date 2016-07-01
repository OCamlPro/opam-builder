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



  type t = {
    backup_files : (string * float * string) list;
  }

  let save dir files =
    let backup_files = ref [] in
    List.iter (fun file ->
      let filename = Filename.concat dir file in
      if Sys.file_exists filename then begin
        Printf.eprintf "Saving %s\n%!" filename;
        let st = Unix.lstat filename in
        let mtime = st.Unix.st_mtime in
        backup_files := (filename, mtime, FileString.read_file filename)
        :: !backup_files
      end else begin
        Printf.eprintf "Warning: could not backup %s\n%!"  filename
        end
    ) files;
    { backup_files = !backup_files }

  let restore t =
    List.iter (fun (filename, mtime, content) ->
      Printf.eprintf "Restoring %s\n%!" filename;
      FileString.write_file filename content;
      Unix.utimes filename mtime mtime;
    ) t.backup_files
