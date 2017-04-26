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
open CheckTypes
open CheckTypes.OP

let arg_extract = ref true

let args = [
    "--no-extract", Arg.Clear arg_extract,
    " Extract files into builder.files";
  ]


let extract_files files_dir c =

  ignore (Printf.kprintf Sys.command "rm -rf %s" files_dir);
  Unix.mkdir files_dir 0o755;

  let find_archive nv r kind =
    match r.build_report_result with
    | ActionInstalled s ->
       (* Printf.eprintf "%s-%s :: installed\n%!" nv kind;*)
       let name = match r.build_report_depopts with
         | [] | [ "" ] -> nv
         | depopts ->
            (* List.iter (fun dep ->
                   Printf.eprintf "depopt: %S\n%!" dep) depopts; *)
            String.concat "+" (nv :: depopts)
       in
       let dir = files_dir // name in
       let p,_ = OcpString.cut_at nv '.' in
       let archive =
         CheckTree.cache_dir_fullname //
           p // nv //
             Printf.sprintf "%s-%s-%s.tar.gz"
                            r.build_report_hash
                            c.switch
                            kind
       in
       if not (Sys.file_exists archive) then
         Printf.eprintf "Missing archive %s\n%!" archive
       else
         if not (Sys.file_exists dir) then
           Unix.mkdir dir 0o755;

       Printf.eprintf "Generating %s\n%!" dir;
       Sys.chdir dir;

       let cmd = Printf.sprintf "tar zxf %s" archive in
       let ret =  Sys.command cmd in

       if ret <> 0 then begin
           Printf.eprintf "Command %S failed with error %d\n%!"
                          cmd ret;
         end;

       let (_: int) = Sys.command "rm -rf .opam-switch" in

       Sys.chdir CheckTree.current_dir;

       let (info : ApiTypes.new_file_INFO) = {
           info_nv = nv;
           info_depends = r.build_report_depends;
           info_depopts = r.build_report_depopts;
         } in
       let oc = open_out_bin (dir // "INFO") in
       output_value oc info;
       close_out oc;

    | _ -> ()
  in
  StringMap.iter (fun _ v ->
      match v.version_build with
      | Some actions ->
         List.iter (fun (nv, action) ->
             match action with
             | Install r -> find_archive nv r "install"
             | Build r -> find_archive nv r "build"

             | _ -> ()
           ) actions


      | _ -> ()
    ) c.versions;
  Printf.eprintf "Archives extracted in %S\n%!" files_dir;
  ()

let rec iter_files topdir dir f =
  let curdir = topdir // dir in
  let files = Sys.readdir curdir in
  Array.iter (fun file ->
      let filename = curdir // file in
      let file = dir // file in
      match try
          (Unix.lstat filename).Unix.st_kind
        with _ -> Unix.S_CHR
      with
      | Unix.S_REG -> f file
      | Unix.S_DIR -> iter_files topdir file f
      | _ -> ()
    ) files

type kind =
  | CMI
  | CMA | CMO
  | CMXA | CMX

let objinfo_files files_dir =
  let dirs = Sys.readdir files_dir in
  Array.iter (fun dir ->
      let dir = files_dir // dir in
      let info_file = dir // "INFO" in
      let ic = open_in_bin info_file in
      let ( _info : ApiTypes.new_file_INFO ) = input_value ic in
      close_in ic;

      let register_file file kind =
        Printf.eprintf "File %S\n%!" (dir // file);
        ()
      in

      iter_files dir "." (fun file ->
                   if Filename.check_suffix file ".cmo" then
                     register_file file CMO
                   else
                     if Filename.check_suffix file ".cma" then
                       register_file file CMA
                   else
                     if Filename.check_suffix file ".cmi" then
                       register_file file CMI
                   else
                     if Filename.check_suffix file ".cmx" then
                       register_file file CMX
                   else
                     if Filename.check_suffix file ".cmxa" then
                       register_file file CMXA
                 )
    ) dirs;
  ()

let action args =
  CheckTree.check_in_tree();

  let dir = CheckTree.reports_dir_basename in
  let reports = Sys.readdir dir in
  Array.sort compare reports;
  let last_report = ref None in
  Array.iter (fun report ->
      if Filename.check_suffix report ".dump" then
        last_report := Some report
    ) reports;
  match !last_report with
  | None ->
     CheckTree.fatal "api: no .dump file in %s\n%!" dir
  | Some report ->
     let report = dir // report in
     let c, _stats = CheckIO.load report in
     Printf.eprintf "api: dump %S loaded\n%!" report;

     let files_dir = "builder.files" in

     if !arg_extract then extract_files files_dir c;
     objinfo_files files_dir;
     ()
