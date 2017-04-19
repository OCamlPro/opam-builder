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
open CheckTypes
open CheckTypes.OP
open CopamInstall.TYPES

let report st c stats =
  let sw = st.sw in
  let dirs = st.dirs in
  let report_file = dirs.report_dir //
      (Printf.sprintf "%s-build-%s.html"
         c.commit_name sw.sw_name) in




  let npackages = ref 0 in
  let nversions = ref 0 in
  let nunavailable_versions = ref 0 in
  let nbroken_versions = ref 0 in
  let nfailed_versions = ref 0 in
  let nsuccess_versions = ref 0 in
  let nerror_versions = ref 0 in

  StringMap.iter (fun package_name p ->
      incr npackages;
      StringMap.iter (fun version_name v ->
          incr nversions;
          let status = CheckCudf.solution_deps v.version_cache_dir version_name sw.sw_name in
          begin
            match status with
            | NotInstallable -> incr nbroken_versions
            | NotAvailable -> incr nunavailable_versions
            | ExternalError -> incr nerror_versions
            | Installable deps ->
              let install_prefix =
                v.version_cache_dir //
                  (Printf.sprintf "%s-%s-install" v.version_name sw.sw_name) in
              let _build_file = install_prefix ^ ".build" in
              let _log_file = install_prefix ^ ".log" in
              let result_file = install_prefix ^ ".result" in
              try
                match FileString.read_file result_file with
                | "SUCCESS\n" -> incr nsuccess_versions
                | "FAILURE\n" -> incr nfailed_versions
                | _ -> raise Exit
              with _ ->
                incr nerror_versions
          end;
        ) p.package_versions;
    ) c.packages;







  let oc = open_out report_file in
  Printf.fprintf oc "<h1>Commit %s, Switch %s</h1>\n" c.commit_name sw.sw_name;

  Printf.fprintf oc "<pre>\n";
  Printf.fprintf oc "Num packages: %d\n" !npackages;
  Printf.fprintf oc "Num versions: %d\n" !nversions;
  Printf.fprintf oc "Num unavailable versions: %6d (version not available for this OCaml version)\n" !nunavailable_versions;
  Printf.fprintf oc "Num broken versions:      %6d (broken dependencies)\n" !nbroken_versions;
  Printf.fprintf oc "Num failed versions:      %6d (compilation failed)\n" !nfailed_versions;
  Printf.fprintf oc "Num success versions:     %6d (installation succeeded)\n" !nsuccess_versions;
  Printf.fprintf oc "Num error versions:       %6d (builder internal error)\n" !nerror_versions;
  Printf.fprintf oc "</pre>\n";

  Printf.fprintf oc "<table>\n";
  Printf.fprintf oc "<tr>\n";
  Printf.fprintf oc "  <td>Package</td>\n";
  Printf.fprintf oc "  <td>OCaml %s</td>\n" sw.sw_name;
  Printf.fprintf oc "</tr>\n";

  StringMap.iter (fun package_name p ->

      Printf.fprintf oc "<tr>\n";
      Printf.fprintf oc "  <td>%s</td>\n" package_name;
      Printf.fprintf oc "  <td></td>\n";
      Printf.fprintf oc "</tr>\n";

      StringMap.iter (fun version_name v ->
          Printf.fprintf oc "<tr>\n";
          Printf.fprintf oc "  <td><a href=\"http://github.com/ocaml/opam-repository/tree/master/packages/%s/%s/opam\">%s</a></td>\n"
            package_name version_name version_name;

          let status = CheckCudf.solution_deps v.version_cache_dir version_name sw.sw_name in
          begin
            match status with
            | NotInstallable ->
              Printf.fprintf oc "  <td style=\"background-color: red;\">BROKEN</td>\n"
            | NotAvailable ->
              Printf.fprintf oc "  <td style=\"background-color: white;\"></td>\n"
            | ExternalError ->
              Printf.fprintf oc "  <td style=\"background-color: yellow;\"></td>\n"
            | Installable deps ->
              let install_prefix =
                v.version_cache_dir //
                  (Printf.sprintf "%s-%s-install" v.version_name sw.sw_name) in
              let _build_file = install_prefix ^ ".build" in
              let _log_file = install_prefix ^ ".log" in
              let result_file = install_prefix ^ ".result" in
              try
                match FileString.read_file result_file with
                | "SUCCESS\n" ->
                  Printf.fprintf oc "  <td style=\"background-color: green;\"></td>\n";
                | "FAILURE\n" ->
                  Printf.fprintf oc "  <td style=\"background-color: orange;\"><a href=\"#%s\">FAILURE</a></td>\n" v.version_name;
                | _ -> raise Exit
              with _ ->
                Printf.fprintf oc "  <td style=\"background-color: orange;\">???</td>\n";
          end;

          Printf.fprintf oc "</tr>\n";

        ) p.package_versions;
    ) c.packages;

  Printf.fprintf oc "</table>\n";

  let version_header p v =
    Printf.fprintf oc "<a name=\"%s\">\n" v.version_name;
    Printf.fprintf oc "<h2><a href=\"http://github.com/ocaml/opam-repository/tree/master/packages/%s/%s/opam\">%s</a></h2>\n"
      p.package_name v.version_name v.version_name;
    Printf.fprintf oc "</a>\n";
  in

  StringMap.iter (fun package_name p ->

      StringMap.iter (fun version_name v ->

          let status = CheckCudf.solution_deps v.version_cache_dir version_name sw.sw_name in
          begin
            match status with
            | NotAvailable -> ()
            | ExternalError -> ()
            | NotInstallable ->
              version_header p v
            | Installable deps ->
              let install_prefix =
                v.version_cache_dir //
                  (Printf.sprintf "%s-%s-install" v.version_name sw.sw_name) in
              let build_file = install_prefix ^ ".build" in
              let log_file = install_prefix ^ ".log" in
              let result_file = install_prefix ^ ".result" in
              try
                match FileString.read_file result_file with
                | "SUCCESS\n" -> ()
                | "FAILURE\n" ->
                  version_header p v;
                  Printf.fprintf oc "<pre>%s</pre>"
                    (FileString.read_file build_file);
                  Printf.fprintf oc "<pre>%s</pre>"
                    (FileString.read_file log_file);
                | _ -> raise Exit
              with _ ->
                ()
          end;

          Printf.fprintf oc "</tr>\n";

        ) p.package_versions;
    ) c.packages;



  ()
