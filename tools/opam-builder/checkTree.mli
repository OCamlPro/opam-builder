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

module TYPES : sig

  type build_file = (string * build_action) list

   and build_action =
     | Build of build_report
     | Install of build_report
   | DisabledFailed
   | DisabledSkip

 and build_report =
   {
     build_report_begin_time : string;
     build_report_hash : string;
     build_report_depends : string list;
     build_report_depopts : string list;
     mutable build_report_result : build_result;
     mutable build_report_snap_errors : build_snap_errors list;
     mutable build_report_end_time : string;
   }

   and build_result =
     | ActionReused
     | ActionFailed of string (* duration before failure *)
     | ActionInstalled of string
     | ActionUnknown

   and build_snap_errors =
     | ModifiedFile of string
     | RemovedFile of string

  exception InvalidFile

  end

val switch_file_basename : string
val cache_dir_basename : string
val reports_dir_basename : string

(* with current_dir before (current_dir at startup) *)
val current_dir : string
val switch_file_fullname : string
val cache_dir_fullname : string
val reports_dir_fullname : string

(* read and write builder.switch *)
val read_switch : unit -> string
val write_switch : string -> unit

(* Check that we are called in the current switch directory, exit with
  fatal error otherwise *)
val check_in_tree: unit -> unit

(* print a fatal error and exit *)
val fatal : ('a, unit, string, 'b) format4 -> 'a


(* File with .build *)

val read_build : string -> TYPES.build_file
