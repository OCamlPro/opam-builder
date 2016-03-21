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

  exception FileRemoved of string
  exception FileChanged of string

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

  val make :
    string -> (* directory to snapshot *)
    StringSet.t -> (* ignored_files *)
    t
  val save : string -> t -> unit
  val load : string -> t
  val diff : (* after *) t -> (* before *) t -> t

  val copy_files : (* src_dir *) string -> t -> (* dst_dir *) string -> unit
  val remove_files : string -> t -> unit

  val clean_files :
    string -> (* directory *)
    StringSet.t -> (* ignore files, starting with ./XXX *)
    t -> (* previous snapshot *)
    unit
