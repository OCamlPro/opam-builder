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
open ApiTypes
open ApiTypes.OP

open Asttypes
open Types

(* Move this in somewhere generic... *)
let with_ic filename f =
  let ic = open_in_bin filename in
  try
    let res = f ic in
    close_in ic;
    res
  with exn ->
       close_in ic;
       raise exn



let string_of f sign =
  f Format.str_formatter sign;
  Format.flush_str_formatter ()

let string_of_type =
  string_of (fun ppf ty ->
      Printtyp.reset_and_mark_loops ty;
      Printtyp.type_expr ppf ty)

let string_of_variant =
  string_of (fun ppf ty ->
      Printtyp.constructor_arguments ppf ty)

let string_of_signature = string_of Printtyp.signature

let rec module_desc sign =

  let items = ref [] in
  List.iter (
      function
      | Sig_value (id, val_desc) ->
         items := (Ident.name id,
                   Value (string_of_type val_desc.val_type))
                  :: !items

      | Sig_module (mid, mod_decl, rec_status) ->
         let s = string_of Printtyp.modtype mod_decl.md_type in
         let mod_desc = match mod_decl.md_type with
           | Mty_signature sig_items ->
              Some (module_desc sig_items)
           | _ -> None
         in
         items := (Ident.name mid,
                   Module (s, mod_desc))
                  :: !items
      | Sig_type (id, type_decl, rec_status) ->
         let s = string_of (Printtyp.type_declaration id) type_decl in
         let decls = ref [] in
         begin match type_decl.type_kind with
         | Type_record (labels,_repr) ->
            List.iter (fun l ->
                decls :=
                  (Ident.name l.ld_id,
                   (match l.ld_mutable with
                    | Mutable -> "mutable "
                    | Immutable -> ""
                   ) ^ string_of_type l.ld_type) :: !decls
              ) labels
         | Type_variant variants ->
            List.iter (fun cd ->
                decls :=
                  (Ident.name cd.cd_id,
                   let cd_args = string_of_variant cd.cd_args in
                   match cd.cd_res with
                   | None -> "of " ^ cd_args
                   | Some res ->
                      let res = string_of_type res in
                      Printf.sprintf ": %s -> %s"
                                     cd_args res) :: !decls
              ) variants
         | _ -> ()
         end;

         items := (Ident.name id,
                   Type (s, !decls)) :: !items

      | Sig_typext (id, ext, _) ->
         let s = string_of (Printtyp.extension_constructor id) ext in
         let decls = ref [] in
         let cd_args = string_of_variant ext.ext_args in
         decls := (Ident.name id, cd_args) :: !decls;
         items := (Ident.name id,
                   Type (s, !decls)) :: !items


      | Sig_modtype _
        | Sig_class _
        | Sig_class_type _ -> ()
    ) sign;

  List.rev !items


let find_intf d intf_modname intf_hash =
  try
    StringMap.find intf_hash d.intf_by_hash
  with Not_found ->
    let u = {
        intf_modname;
        intf_hash;
        intf_cmis = [];
        intf_deps = [];
        intf_asm = [];
        intf_byte = [];
        intf_api = None;
      } in
    d.intf_by_hash <- StringMap.add intf_hash u d.intf_by_hash;
    u

let modname_of filename =
  String.capitalize (Filename.chop_extension (Filename.basename filename))

let cmi_of_file d file filename =
  let intf_modname = modname_of filename in
#if OCAML_VERSION >= "4.00"
  let {
    Cmi_format.cmi_name;
    cmi_sign;
    cmi_crcs;
    cmi_flags;
  } = Cmi_format.read_cmi filename in
#else
#error "Not compatible with this version of OCaml"
#endif
  let intf_hash, cmi_deps = match cmi_crcs with
    | (_, Some hash) :: deps -> hash, deps
    | _ -> assert false
      in
      let cmi_intf = find_intf d intf_modname intf_hash in
      begin
        match cmi_intf.intf_cmis with
        | [] ->
           cmi_intf.intf_deps <-
             List.map (fun (intf_modname, maybe_crc) ->
                 intf_modname,
                 match maybe_crc with
                 | None -> None
                 | Some intf_hash ->
                    Some (find_intf d intf_modname intf_hash)
               ) cmi_deps;
           cmi_intf.intf_api <- Some (
                                    string_of_signature cmi_sign,
                                    module_desc cmi_sign);
        | _ -> ()
      end;
      let cmi_file = {
          cmi_file = file;
          cmi_intf;
        } in
      cmi_intf.intf_cmis <- cmi_file :: cmi_intf.intf_cmis;
      CMI cmi_file
  ;;

let find_byte d cu =
  let byte_hash = Digest.string (Marshal.to_string cu []) in
  try
    StringMap.find byte_hash d.byte_by_hash
  with Not_found ->
    let byte_modname = cu.Cmo_format.cu_name in
    let byte_intf = ref "" in
    let byte_deps = ref [] in
    List.iter (fun (modname, digesto) ->
        if modname = byte_modname then
          match digesto with
          | None -> assert false
          | Some digest -> byte_intf := digest
        else
          byte_deps := (modname,
                        match digesto with
                        | None -> None
                        | Some digest ->
                           Some (find_intf d modname digest)
                       ) :: !byte_deps;
      ) cu.Cmo_format.cu_imports;
    let byte_intf = find_intf d byte_modname !byte_intf in
    let byte = {
        byte_modname;
        byte_hash;
        byte_deps = !byte_deps;
        byte_intf;
        byte_cmos = [];
        byte_cmas = [];
      } in
    byte_intf.intf_byte <- byte :: byte_intf.intf_byte;
    d.byte_by_hash <- StringMap.add byte_hash byte d.byte_by_hash;
    byte
;;

let find_asm d asm_modname asm_hash =
  try
    StringMap.find asm_hash d.asm_by_hash
  with Not_found ->
       let asm = {
           asm_modname;
           asm_hash;
           asm_intf_deps = [];
           asm_asm_deps = [];
           asm_cmxs = [];
           asm_cmxas = [];
           asm_intf = None;
         } in
       d.asm_by_hash <- StringMap.add asm_hash asm d.asm_by_hash;
       asm

let find_asm d cu asm_hash =
  let asm_modname = cu.Cmx_format.ui_name in
  let asm = find_asm d asm_modname asm_hash in
  begin
    match asm.asm_intf with
    | Some _ -> ()
    | None ->

       List.iter (fun (modname, digesto) ->
           if modname = asm_modname then
             match digesto with
             | None -> assert false
             | Some digest -> assert (asm_hash = digest)
           else
             asm.asm_asm_deps <- (modname,
                              match digesto with
                                None -> None
                              | Some digest ->
                                 Some (find_asm d modname digest)) ::
                                   asm.asm_asm_deps;
         ) cu.Cmx_format.ui_imports_cmx;

       List.iter (fun (modname, digesto) ->
           if modname = asm_modname then
             match digesto with
             | None -> assert false
             | Some digest ->
                let unit = find_intf d asm_modname digest in
                asm.asm_intf <- Some unit;
                unit.intf_asm <- asm :: unit.intf_asm
           else
             asm.asm_intf_deps <- (modname,
                              match digesto with
                                None -> None
                              | Some digest ->
                                 Some (find_intf d modname digest)) ::
                                   asm.asm_intf_deps;
         ) cu.Cmx_format.ui_imports_cmi;

       assert (asm.asm_intf <> None);
  end;
  asm

let cmx_of_file d cmx_file filename =
  let cu, crc =
    with_ic filename (fun ic ->
              let len_magic_number = String.length Config.cmx_magic_number in
              let magic_number = really_input_string ic len_magic_number in
              if magic_number = Config.cmx_magic_number then
                let cu = (input_value ic : Cmx_format.unit_infos) in
                let crc = Digest.input ic in
                (cu, crc)
              else failwith "cmo_of_file: bad magic"
            )
  in
  let cmx_unit = find_asm d cu crc in
  let cmx_file = { cmx_file; cmx_unit } in
  cmx_unit.asm_cmxs <- cmx_file :: cmx_unit.asm_cmxs;
  CMX cmx_file
  ;;

let cmxa_of_file d cmxa_file filename =
  let lib =
    with_ic filename (fun ic ->
              let len_magic_number = String.length Config.cmxa_magic_number in
              let magic_number = really_input_string ic len_magic_number in
              if magic_number = Config.cmxa_magic_number then
                let lib = (input_value ic : Cmx_format.library_infos) in
                lib
              else failwith "cmo_of_file: bad magic")
  in
  let cmxa_units = List.map (fun (cu, crc) ->
                       find_asm d cu crc) lib.Cmx_format.lib_units in
  let cmxa_file = { cmxa_file; cmxa_units } in
  List.iter (fun cmx_unit ->
      cmx_unit.asm_cmxas <- cmxa_file :: cmx_unit.asm_cmxas;
    ) cmxa_units;
  CMXA cmxa_file
  ;;


let cmo_of_file d cmo_file filename =
  let cu =
    with_ic filename (fun ic ->
              let len_magic_number = String.length Config.cmo_magic_number in
              let magic_number = really_input_string ic len_magic_number in
              if magic_number = Config.cmo_magic_number then
                let cu_pos = input_binary_int ic in
                seek_in ic cu_pos;
                let cu = (input_value ic : Cmo_format.compilation_unit) in
                cu
              else failwith "cmo_of_file: bad magic")
  in
  let cmo_unit = find_byte d cu in
  let cmo_file = { cmo_file; cmo_unit } in
  cmo_unit.byte_cmos <- cmo_file :: cmo_unit.byte_cmos;
  CMO cmo_file
  ;;

(* primes.1.3.*: the .cma file is actually an executable !! *)

let cma_of_file d cma_file filename =
  let lib =
    with_ic filename (fun ic ->
              let len_magic_number = String.length Config.cma_magic_number in
              let magic_number = really_input_string ic len_magic_number in
              if magic_number = Config.cma_magic_number then
                let toc_pos = input_binary_int ic in
                seek_in ic toc_pos;
                let lib = (input_value ic : Cmo_format.library) in
                lib
              else failwith "cma_of_file: bad magic")
  in
  let cma_units = List.map (find_byte d) lib.Cmo_format.lib_units in
  let cma_file = { cma_file; cma_units } in
  List.iter (fun unit ->
      unit.byte_cmas <- cma_file :: unit.byte_cmas
    ) cma_units;
  CMA cma_file
;;

let file_of_file d file filename =
  try
    if Filename.check_suffix filename ".cmi" then
      cmi_of_file d file filename
    else
      if Filename.check_suffix filename ".cmo" then
        cmo_of_file d file filename
      else
        if Filename.check_suffix filename ".cma" then
          cma_of_file d file filename
        else
        if Filename.check_suffix filename ".cmx" then
          cmx_of_file d file filename
        else
        if Filename.check_suffix filename ".cmxa" then
          cmxa_of_file d file filename
        else
          FILE
  with exn ->
    Printf.eprintf "Exception %s with %S\n%!"
                   (Printexc.to_string exn) filename;
    FILE
;;

let () =

  let dir = "builder.files" in

  Printf.printf "Reading files from %s\n%!" dir;
  let files = Sys.readdir dir in

  let date =
    let tm = Unix.gmtime (Unix.gettimeofday ()) in
    Printf.sprintf "%d-%d-%d"
                   (1900+tm.Unix.tm_year)
                   (1+tm.Unix.tm_mon)
                   tm.Unix.tm_mday
  in

  let switch = with_ic "builder.switch" (fun ic ->
                         input_line ic) in

  let d = {
      date; switch;
      packages = StringMap.empty;
      intf_by_hash = StringMap.empty;
      asm_by_hash = StringMap.empty;
      byte_by_hash = StringMap.empty;
    } in

  Array.iter (fun file ->
      let dir = dir // file in
      let info_file = dir // "INFO" in
      if Sys.file_exists info_file then
        let ic = open_in_bin info_file in
        let (info : ApiTypes.new_file_INFO) = input_value ic in
        close_in ic;

        let package_name, version = OcpString.cut_at info.info_nv '.' in
        let package =
          try
            StringMap.find package_name d.packages
          with Not_found ->
            let p = {
                package_name;
                versions = [];
              } in
            d.packages <- StringMap.add package_name p d.packages;
            p
        in
        let v = {
            package;
            version;
            depends = info.info_depends;
            depopts = info.info_depopts;
            files = [];
          }
        in
        package.versions <- v :: package.versions;

        let rec iter_files revpath dir =
          let list = ref [] in
          let files = Sys.readdir dir in
          Array.sort compare files;
          Array.iter (fun basename ->
              let filename = dir // basename in
              let revpath = basename :: revpath in
              match revpath with
              | [ "INFO" ]
                | [ "ld.d";"ocaml";"lib"] ->
                 (* remove opam-builder artefacts *)
                 ()
              | _ ->
                 let file = {
                     revpath;
                     file_kind = Dir [];
                     file_package = v;
                   } in
                 list := (basename, file) :: !list;
                 try
                   let st = Unix.lstat filename in
                   match st.Unix.st_kind with
                   | Unix.S_DIR ->
                      file.file_kind <- Dir (iter_files revpath filename)
                   | Unix.S_REG ->
                      let file_type =
                        match file_of_file d file filename with
                        | FILE when 0o111 land st.Unix.st_perm <> 0 ->
                           EXEC
                        | file -> file

                      in
                      file.file_kind <- File (file_type, st.Unix.st_size)
                   | _ ->
                      file.file_kind <- File (FILE, st.Unix.st_size)
                 with exn ->
                   Printf.eprintf "%s raised %s\n%!" filename
                                  (Printexc.to_string exn);

                   ()
            ) files;
          List.rev !list
        in
        v.files <- iter_files [] dir;

    ) files;

  let oc = open_out "builder.database" in
  output_value oc 1; (* version 1 *)
  output_value oc d;
  close_out oc;

  Printf.printf "Database Generated: builder.database\n%!"
