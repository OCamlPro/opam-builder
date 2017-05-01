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

type database = {
    date : string;
    switch : string;
    mutable packages : package StringMap.t;
    mutable intf_by_hash : intf_unit StringMap.t;
    mutable asm_by_hash : asm_unit StringMap.t;
    mutable byte_by_hash : byte_unit StringMap.t;
  }

 and package = {
     package_name : string;
     mutable versions : package_version list;
   }

 and package_version = {
    package : package;
    version : string;
    depends : string list;
    depopts : string list;
    mutable files : directory;
  }

 and directory = (string * file) list

 and file = {
     revpath : string list;
     file_package : package_version;
     mutable file_kind : file_kind;
   }

 and file_kind =
   | Dir of directory
   | File of file_type * int

 and file_type =
   | CMI of cmi_file
   | CMO of cmo_file
   | CMA of cma_file
   | CMX of cmx_file
   | CMXA of cmxa_file
   | EXEC
   | FILE

 and intf_unit = {
     intf_modname : string;
     intf_hash : string;
     mutable intf_cmis : cmi_file list;
     mutable intf_deps : (string * intf_unit option) list; (* depends on these interfaces *)
     mutable intf_asm : asm_unit list; (* implemented by these cmxs *)
     mutable intf_byte : byte_unit list;
     mutable intf_api : sig_api option;
   }

 and byte_unit = {
     byte_modname : string;
     byte_hash : string;
     byte_intf : intf_unit; (* implements this cmi *)
     byte_deps : (string * intf_unit option) list; (* depends on these interfaces *)
     mutable byte_cmos : cmo_file list;
     mutable byte_cmas : cma_file list;
   }

 and asm_unit = {
     asm_modname : string;
     asm_hash : string;
     mutable asm_intf : intf_unit option;
     mutable asm_intf_deps : (string * intf_unit option) list;
     mutable asm_asm_deps : (string * asm_unit option) list;
     mutable asm_cmxs : cmx_file list;
     mutable asm_cmxas : cmxa_file list;
   }

 and cmi_file = {
     cmi_file : file;
     cmi_intf : intf_unit;
   }

 and cma_file = {
     cma_file : file;
     cma_units : byte_unit list; (* contains these cmos *)
   }

 and cmxa_file = {
     cmxa_file : file;
     cmxa_units : asm_unit list; (* contains these cmxs *)
   }

 and cmo_file = {
     cmo_file : file;
     cmo_unit : byte_unit;
   }

 and cmx_file = {
     cmx_file : file;
     cmx_unit : asm_unit;
   }

 and sig_api = string * module_desc

 and module_desc = (string * item_desc) list

 and item_desc =
   | Value of string
   | Type of string (* printed type *)
             * (string * string) list (* list of constructors or labels *)
   | Module of string * module_desc option


(* This file format is used to exchange information between
opam-builder and opam-builder-api. *)
type new_file_INFO = {
    info_nv : string;
    info_depends : string list;
    info_depopts : string list;
  }

module OP = struct
  let (//) = Filename.concat
end
