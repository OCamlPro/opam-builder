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



open CheckTypes.V
open StringCompat

let rec hash_directory dirname =
  let files = Sys.readdir dirname in
  Array.sort compare files;
  let b = Buffer.create (Array.length files * 16+16) in
  CheckDigest.add_digest b (CheckDigest.string dirname);
  Array.iter (fun file ->
    let filename = Filename.concat dirname file in
    CheckDigest.add_digest b (
      if Sys.is_directory filename then
        hash_directory filename
      else
        CheckDigest.file filename);
  ) files;
  CheckDigest.string (Buffer.contents b)




let hash_package_content p =
  let b = Buffer.create 1111 in
  Buffer.add_string b p.package_name;
  StringMap.iter (fun _ v ->
    Buffer.add_string b v.version_name;
    CheckDigest.add_digest b v.version_checksum;
  ) p.package_versions;
  p.package_local_checksum <- Some (CheckDigest.string (Buffer.contents b))




let hash_package_closure p visit =
  incr visit;
  if !visit = 0 then begin
    (* Instead, we should reinitialize the counters *)
    Printf.eprintf "Error: exceeded visits of packages. Exiting.\n%!";
    exit 2
  end;
  let closure = ref StringMap.empty in
  let rec iter _ p =
    if p.package_visited <> !visit then begin
      p.package_visited <- !visit;
      closure := StringMap.add p.package_name p !closure;
      StringMap.iter iter p.package_deps
    end
  in
  iter p.package_name p;

  let b = Buffer.create 1111 in
  StringMap.iter (fun package_name p ->
    match p.package_local_checksum with
    | None -> assert false
    | Some checksum ->
      Buffer.add_string b package_name;
      CheckDigest.add_digest b checksum;
  ) !closure;
  let checksum = CheckDigest.string (Buffer.contents b) in

  p.package_transitive_checksum <- Some (checksum, !closure)
