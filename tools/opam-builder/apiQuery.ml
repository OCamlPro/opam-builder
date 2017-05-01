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

(* same as in ApiMain *)
let with_ic filename f =
  let ic = open_in_bin filename in
  try
    let res = f ic in
    close_in ic;
    res
  with exn ->
       close_in ic;
       raise exn

let file_name file =
  String.concat "/" (List.rev file.revpath)

type db =
  | DB_name of string list
  | DB_content of string * ApiTypes.database

let home = try Sys.getenv "HOME" with Not_found -> "/tmp"

let db = ref (DB_name ["builder.database";
                       home // ".ocp" // "builder.database"
             ])

let load_db () =
  match !db with
  | DB_name files ->
     let rec iter list =
       match list with
         [] -> Printf.eprintf "Error: could not find the database\n%!";
               exit 2
       | file :: files ->
          try
            with_ic file (fun ic ->
                      let (version : int) = input_value ic in
                      if version <> 1 then begin
                          Printf.eprintf "Error: database version %d, expected version %d\n%!" version 1;
                          exit 2
                        end;
                      let (d : database) = input_value ic in
                      db := DB_content (file, d);
                      d)
          with _ ->
               iter files
     in
     iter files
  | DB_content (file, d) -> d

let print_intf_sig = ref false

let print_intf intf =
  Printf.printf "Unit %s %s\n" intf.intf_modname
                (Digest.to_hex intf.intf_hash);
  List.iter (fun cmi_file ->
      Printf.printf "   %s (%s.%s)\n"
                    (file_name cmi_file.cmi_file)
                    cmi_file.cmi_file.file_package.package.package_name
                    cmi_file.cmi_file.file_package.version
    ) intf.intf_cmis;

  if !print_intf_sig then begin
      match intf.intf_api with
      | None -> ()
      | Some (s, _) ->
         Printf.printf "Signature:\n%s\n" s
    end;

  Printf.printf "\n%!"


let query_longident longident =
  let db = load_db () in
  let lid = OcpString.split longident '.' in
  let modname, path = match lid with
    | [] -> assert false
    | modname :: path -> modname, path
  in

  let rec iter intf path mod_desc =
    match path with
    | [] -> print_intf intf
    | ident :: path ->
       List.iter (fun (id,kind) ->
           if id = ident then
             match kind with
             | Value s ->
                Printf.printf "val %s : %s\n" longident s;
                print_intf intf
             | Type (s, decl) ->
                Printf.printf "type %s : %s\n" longident s;
                print_intf intf
             | Module (s, mod_desc) ->
                match mod_desc with
                | None when path = [] ->
                   print_intf intf
                | None ->
                   Printf.printf "maybe in\n";
                   print_intf intf
                | Some mod_desc ->
                   iter intf path mod_desc
         ) mod_desc
  in

  StringMap.iter (fun _ intf ->
      if intf.intf_modname = modname then
        match intf.intf_api with
        | None -> ()
        | Some (_, mod_desc) ->
           iter intf path mod_desc
    ) db.intf_by_hash

let longident_revpath revpath id =
  String.concat "." (List.rev (id :: revpath))

let query_ident ident =
  let db = load_db () in

  StringMap.iter (fun _ intf ->
      match intf.intf_api with
      | None -> ()
      | Some (_, mod_desc) ->

         let rec iter ident found revpath mod_desc =
           List.iter (fun (id,kind) ->
               begin
                 if id = ident then
                   match kind with
                   | Value s ->
                      Printf.printf "val %s : %s\n"
                                    (longident_revpath revpath id) s;
                      found := true;
                   | Type (s, decl) ->
                      Printf.printf "type %s : %s\n"
                                    (longident_revpath revpath id) s;
                      found := true;
                   | Module (s, mod_desc) ->
                      Printf.printf "module %s : ...\n"
                                    (longident_revpath revpath id);
                      found := true
               end;
               match kind with
               | Module (_, Some mod_desc) ->
                  iter ident found (id :: revpath) mod_desc
               | _ -> ()
             ) mod_desc
         in
         let found = ref false in
         iter ident found [intf.intf_modname] mod_desc;
         if !found then print_intf intf
    ) db.intf_by_hash

let query_intf_modname modname =
  let db = load_db () in
  StringMap.iter (fun _ intf ->
      if intf.intf_modname = modname then
        print_intf intf
    ) db.intf_by_hash

let query_intf_hash hash =
  let hash = Digest.from_hex hash in
  let db = load_db () in
  StringMap.iter (fun _ intf ->
      if intf.intf_hash = hash then
        print_intf intf
    ) db.intf_by_hash

let print_package_files = ref false

let version_info p v =
  let depends = String.concat "," v.depends in
  let depopts = String.concat "," v.depopts in
  let hash = Digest.to_hex (Digest.string
                              (Printf.sprintf "%s|%s|%s|%s"
                                              p.package_name
                                              v.version
                                              depends
                                              depopts)) in
  (depends, depopts, hash)

let print_version p v =
  let (depends, depopts, hash) = version_info p v in
  Printf.printf "Package %s.%s (%s):\n" p.package_name v.version hash;
  Printf.printf "  Depends: %s\n" depends;
  if depopts <> "" then
    Printf.printf "  Depopts: %s\n" depopts;

  if !print_package_files then
    let rec iter list =
      List.iter (fun (_, file) ->
          match file.file_kind with
          | Dir list ->
             Printf.printf "DIR  %s\n" (file_name file);
             iter list
          | File (file_type, file_size) ->
             Printf.printf "%s %s (%d bytes)\n"
                           (match file_type with
                            | FILE -> "    "
                            | EXEC -> "EXE "
                            | CMI _ -> "CMI "
                            | CMO _ -> "CMO "
                            | CMX _ -> "CMX "
                            | CMXA _ -> "CMXA"
                            | CMA _ -> "CMA ")
                           (file_name file)
                           file_size
        ) list
    in
    iter v.files;

    Printf.printf "\n%!"

let query_package package =
  let db = load_db () in
  try
    let _ = Digest.from_hex package in
    StringMap.iter (fun _ p ->
        List.iter (fun v ->
            let (depends, depopts, hash) = version_info p v in
            if hash = package then print_version p v
          ) p.versions
      ) db.packages
  with Invalid_argument _ ->
    let package,version = OcpString.cut_at package '.' in
    try
      let p = StringMap.find package db.packages in
      List.iter (fun v ->
          if version = "" || v.version = version then begin
              print_version p v
            end
        ) p.versions;
    with Not_found ->
      Printf.printf "No package %S in database\n%!" package

let list_packages () =
  let db = load_db () in
  StringMap.iter (fun _ p ->
      Printf.printf "%s\n" p.package_name;
      List.iter (fun v ->
          let (depends, depopts, hash) = version_info p v in
          Printf.printf "  %s.%s (%s)\n" p.package_name v.version hash
        ) p.versions
    ) db.packages

let query_stats () =
  let _ = load_db () in
  let (file, db) = match !db with
      DB_content (file, db) -> (file, db)
    | DB_name _ -> assert false
  in
  Printf.printf "Database: %s\n" file;
  Printf.printf "Database generated on: %s\n" db.date;
  Printf.printf "Database for switch: %s\n" db.switch;
  let nversions = ref 0 in
  StringMap.iter (fun _ p ->
      nversions := !nversions + List.length p.versions
    ) db.packages;
  Printf.printf "Number of installable packages: %d\n"
                (StringMap.cardinal db.packages);
  Printf.printf "Number of installable versions: %d\n" !nversions;
  let nintfs = ref 0 in
  StringMap.iter (fun _ intf ->
      if intf.intf_api <> None then incr nintfs;
    ) db.intf_by_hash;
  Printf.printf "Number of available interfaces: %d\n" !nintfs;

  Printf.printf "%!"

let arg_list = Arg.align [
                   "--intf-modname", Arg.String query_intf_modname,
                   "MODNAME Which package defines an interface for MODNAME ?";
                   "--intf-hash", Arg.String query_intf_hash,
                   "HASH Which package defines an interface with crc HASH ?";

                   "--longident", Arg.String query_longident,
                   "LONGIDENT Query origin of LONGIDENT";

                   "--ident", Arg.String query_ident,
                   "IDENT Query occurrences of IDENT";

                   "--package", Arg.String query_package,
                   "PACKAGE Which info on PACKAGE";

                   "--list-packages", Arg.Unit list_packages,
                   " List all packages contained in database";

                   "--stats", Arg.Unit query_stats,
                   " Query stats on database";

                   "", Arg.Unit (fun () -> ()), "";

                   "--print-intf-sig", Arg.Set print_intf_sig,
                   " Print interface signature";

                   "--print-package-files", Arg.Set print_package_files,
                   " Print package files";

                   "--database", Arg.String (fun s ->
                                     db := DB_name [s]),
                   "FILENAME Set the databae filename";
  ]

let () =
  let arg_usage = " : query builder.database" in
  let arg_anon s =
    Printf.eprintf "Error: unexpected argument %S\n%!" s;
    Arg.usage arg_list arg_usage;
    exit 2
  in

  Arg.parse arg_list arg_anon arg_usage
