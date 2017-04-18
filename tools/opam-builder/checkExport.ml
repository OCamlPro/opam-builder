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



open CheckTypes
open StringCompat
open CopamInstall

let export st c stats =
  let sw = st.sw in
  let dirs = st.dirs in
  let date = c.timestamp_date in
  let export_file = Filename.concat dirs.report_dir
      (Printf.sprintf "%s-%s-%s.export"
         date
         c.commit_name sw.sw_name) in

  let export_file_tmp = export_file ^ ".tmp" in
  let oc = open_out export_file_tmp in

  Printf.fprintf oc "commit:%s\n" c.commit_name;
  Printf.fprintf oc "switch:%s\n" sw.sw_name;
  Printf.fprintf oc "date:%f\n" (Unix.time());

  StringMap.iter (fun package_name p ->
      let package_dir = Filename.concat dirs.cache_dir p.package_name in
      Printf.fprintf oc "package:%s\n" package_name;
      let status = CheckCudf.solution_deps
          package_dir package_name sw.sw_name in
      begin
        match status with
        | NotInstallable ->
          Printf.fprintf oc "status:non-installable\n"
        | NotAvailable ->
          Printf.fprintf oc "status:non-available\n"
        | ExternalError ->
          Printf.fprintf oc "status:external-error\n"
        | Installable deps ->
          Printf.fprintf oc "status:installable\n";
          Printf.fprintf oc "deps:%s\n"
            (String.concat ","
               (List.map (fun (n,v) -> Printf.sprintf "%s.%s" n v) deps))
      end;
      StringMap.iter (fun version_name v ->
          Printf.fprintf oc "version:%s\n" version_name;

          let version_dir = Filename.concat package_dir v.version_name in
          let status = CheckCudf.solution_deps
              version_dir version_name sw.sw_name in
          begin
            match status with
            | NotInstallable ->
              Printf.fprintf oc "status:non-installable\n"
            | NotAvailable ->
              Printf.fprintf oc "status:non-available\n"
            | ExternalError ->
              Printf.fprintf oc "status:builder-error\n"
            | Installable deps ->
              Printf.fprintf oc "status:installable\n";
              Printf.fprintf oc "deps:%s\n"
                (String.concat ","
                   (List.map (fun (n,v) -> Printf.sprintf "%s.%s" n v) deps));
              let install_prefix =
                Filename.concat version_dir
                  (Printf.sprintf "%s-%s-install" v.version_name sw.sw_name) in
              let build_file = install_prefix ^ ".build" in
              let log_file = install_prefix ^ ".log" in
              let result_file = install_prefix ^ ".result" in
              try
                match FileString.read_file result_file with
                | "SUCCESS\n" ->
                  Printf.fprintf oc "status:success\n";
                  let build_content = FileString.read_file build_file in
                  Printf.fprintf oc "begin-build:true\n%s\nbegin-build:false\n"
                    build_content;

                | "FAILURE\n" ->
                  let log_content = FileString.read_file log_file in
                  let build_content = FileString.read_file build_file in
                  Printf.fprintf oc "status:failure\n";
                  Printf.fprintf oc "begin-build:true\n%s\nbegin-build:false\n"
                    build_content;
                  Printf.fprintf oc "begin-log:true\n%s\nbegin-log:false\n"
                    log_content;
                | _ -> raise Exit
              with _ ->
                Printf.fprintf oc "status:builder-error\n";
          end;

        ) p.package_versions;
    ) c.packages;
  Printf.fprintf oc "export:end\n";
  close_out oc;
  Sys.rename export_file_tmp export_file;
  Printf.eprintf "Export: File %S generated\n%!" export_file
