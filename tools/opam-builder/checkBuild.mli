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



val init : CheckTypes.directories -> string list -> CheckTypes.state

val install_popular :
           CheckTypes.state ->
           CheckTypes.V.commit -> CheckGraph.stats array -> unit

val restore_switch_from_archive :
  CheckTypes.directories -> string -> unit
val switch_archive : string -> string -> string
val switch_snapshot : string -> string -> string
val ignore_opam_files : StringCompat.StringSet.t
val report :
  CheckTypes.state ->
  CheckTypes.V.commit -> CheckGraph.stats array -> unit
val export :
  CheckTypes.state ->
  CheckTypes.V.commit -> CheckGraph.stats array -> unit


(* Misc functions *)
val chdir : string -> unit
val ignore_bool : bool -> unit
