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



(* computes all checksums of a commit. If [lint] is true, also call
   `opam lint` on all modified opam files. The following fields are
   not initialized yet: [v.version_lint], [v.version_status],
   [p.package_status].
*)
val check_commit :
  lint:bool ->
  commit:string ->
  switch:string ->
  CheckTypes.directories ->
  CheckTypes.commit

(* [checksum_rule files new_checksum f] checks that all the [files]
   have been generated, calling a function to generate them if
   needed. A checksum is attached to the first file (in a file FILE ^
   ".checksum"), and can cause the regeneration if [new_checksum] is
   different. *)
val checksum_rule :
  string list ->
  CheckDigest.t ->
  (unit -> unit) ->
  unit

val opam_file :
  CheckTypes.directories -> CheckTypes.version -> string
