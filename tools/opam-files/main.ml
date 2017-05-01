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



(*

  Naming conventions for short variables:
  * p : the package
  * v : the version of the package p (often p.package_version)
  * c : the commit
  * st : the state
  * dirs : the directories (often st.dirs)
*)

open CheckTypes
open StringCompat
open CopamInstall

open Types

let api_dir = ref "api"
let check_api_dir = ref true

let string_of_type ty =
  Printtyp.reset_and_mark_loops ty;
  Printtyp.type_expr Format.str_formatter ty;
  Format.flush_str_formatter ()

let string_of_signature sign =
  Printtyp.signature Format.str_formatter sign;
  Format.flush_str_formatter ()

let string_of f sign =
  f Format.str_formatter sign;
  Format.flush_str_formatter ()

let save_item package path content =
  if !check_api_dir then begin
    check_api_dir := false;
    if not (Sys.file_exists !api_dir) then
      Unix.mkdir !api_dir 0o755;
  end;
  let full_path = List.rev path in
  let rec iter api_dir path =
    match path with
    | [] ->
      let api_file = Filename.concat api_dir ("@" ^ package ^ ".txt") in
      let oc = open_out api_file in
      Printf.fprintf oc "%s defines %s as:\n%s\n" package
        (String.concat "." full_path) content;
      close_out oc;
    | id :: path ->
      match id.[0] with
      | 'a'..'z' | 'A'..'Z' ->
        let api_dir = Filename.concat api_dir id in
        if not (Sys.file_exists api_dir) then
          Unix.mkdir api_dir 0o755;
        iter api_dir path
      | _ -> ()
  in
  iter !api_dir full_path

let print_value package path ty =
  let ty = string_of_type ty in
  save_item package path ty

let parse_cmi package cmi_file =
  let modname =
    String.capitalize (Filename.chop_extension (Filename.basename cmi_file)) in
#if OCAML_VERSION >= "4.00"
  let {
    Cmi_format.cmi_name;
    cmi_sign;
    cmi_crcs;
    cmi_flags;
  } = Cmi_format.read_cmi cmi_file in
#else
  let cmi_sign = Env.read_signature modname cmi_file in
#endif
let rec iter_item path = function
#if OCAML_VERSION < "4.00"
    | Tsig_value (id, val_desc) ->
#else
    | Sig_value (id, val_desc) ->
#endif
      print_value package (Ident.name id :: path) val_desc.val_type
#if OCAML_VERSION < "4.00"
    | Tsig_module (mid, mod_decl, rec_status) ->
#else
    | Sig_module (mid, mod_decl, rec_status) ->
#endif
      let mod_ty =
#if OCAML_VERSION < "4.02"
        mod_decl
#else
        mod_decl.md_type
#endif
      in
      save_item package (Ident.name mid :: path)
        (string_of Printtyp.modtype mod_ty);
      begin
        match mod_ty with
#if OCAML_VERSION < "4.00"
      | Tmty_ident _
      | Tmty_functor _
#elif OCAML_VERSION < "4.02"
      | Mty_ident _
      | Mty_functor _
#else
      | Mty_alias _
      | Mty_ident _
      | Mty_functor _

#endif
          -> ()
#if OCAML_VERSION < "4.00"
        | Tmty_signature sig_items ->
#else
        | Mty_signature sig_items ->
#endif
          let path = Ident.name mid :: path in
          List.iter (iter_item path) sig_items
      end

#if OCAML_VERSION < "4.00"
   | Tsig_type (id, type_decl, rec_status) ->
#else
   | Sig_type (id, type_decl, rec_status) ->
#endif
      save_item package (Printf.sprintf "type-%s" (Ident.name id) :: path)
        (string_of (Printtyp.type_declaration id) type_decl)

#if OCAML_VERSION < "4.00"
    | Tsig_class _
#elif OCAML_VERSION < "4.02"
    | Sig_class _
    | Sig_class_type _
    | Sig_exception (_, _)
    | Sig_modtype (_, _)
#else
    | Sig_class _
    | Sig_class_type _
    | Sig_typext _
    | Sig_modtype (_, _)
#endif
      -> ()
  in
  let path = [modname] in
  save_item package path (string_of_signature cmi_sign);
  List.iter (iter_item path) cmi_sign


let current_dir = Sys.getcwd ()
let scan_archive package archive_file =
  let archive_file_done = archive_file ^ ".done" in
  if not (Sys.file_exists archive_file_done) then begin
    Printf.eprintf "Scanning archive %s\n%!" archive_file;
    ignore (Sys.command "rm -rf archive-content");
    Unix.mkdir "archive-content" 0o755;
    Unix.chdir "archive-content";
    ignore (Printf.kprintf Sys.command "tar zxf ../%s" archive_file);
    Unix.chdir current_dir;
    let rec iter dirname =
      let files = Sys.readdir dirname in
      Array.sort compare files;
      Array.iter (fun file ->
          let filename = Filename.concat dirname file in
          if Filename.check_suffix file ".cmi" then
            parse_cmi package filename
          else
            let st = Unix.lstat filename in
            match st.Unix.st_kind with
            | Unix.S_DIR ->
              iter filename
            | _ -> ()
        ) files
    in
    iter "archive-content";
    let oc = open_out archive_file_done in
    Printf.fprintf oc "done\n";
    close_out oc
  end

let scan_cache_dir cache_dir =
  let ocaml_version = Sys.ocaml_version in
  let switch_archive = Filename.concat cache_dir
      (Printf.sprintf "switch-%s.tar.gz" ocaml_version) in
  scan_archive (Printf.sprintf "ocaml.%s" ocaml_version)
    switch_archive;
  let packages = Sys.readdir cache_dir in
  Array.sort compare packages;
  Array.iter (fun package_name ->
      let package_dir =
        Filename.concat cache_dir package_name in

      if Sys.is_directory package_dir then
        let versions = Sys.readdir package_dir in
        Array.sort compare versions;
        Array.iter (fun version_name ->
            let version_dir =
              Filename.concat package_dir version_name in
            if Sys.is_directory version_dir then
              let files = Sys.readdir version_dir in
              Array.iter (fun file ->
                  if Filename.check_suffix file ".tar.gz" then
                    scan_archive version_name
                      (Filename.concat version_dir file)
                ) files
          ) versions
    ) packages;
  ()

let () =
  let package = ref "default.1.0" in
  let arg_list = Arg.align [
      "-package", Arg.String ( (:=) package ),
      "PACKAGE Set package name";
      "-cmi", Arg.String (fun cmi_file ->
          parse_cmi !package cmi_file), "CMI_FILE Parse CMI file";
      "-cache", Arg.String (fun cache_dir ->
          scan_cache_dir cache_dir
        ), "CACHE_DIR Scan cache directory";
    ] in
  let arg_anon s =
    Printf.eprintf "Error: unknown argument %S\n%!" s;
    exit 2
  in
  let arg_usage = "opam-files [OPTIONS]" in
  Arg.parse arg_list arg_anon arg_usage;
  ()
