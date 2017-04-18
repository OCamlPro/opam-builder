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

let compute_stats st c =
  Printf.eprintf "Analyzing commit %s...\n%!" c.commit_name;

  let stats_switch = st.sw in

  let stats = ref [] in
  let version_stats = Hashtbl.create 1111 in

  let get_stats package_name version_num =
    let version_name = package_name ^ "." ^ version_num in
    try
      Hashtbl.find version_stats version_name
    with Not_found ->
      let s = {
          s_version = (try
                         StringMap.find version_name c.versions
                       with _ ->
                         Printf.eprintf "Version %s could not be found !!!\n%!" version_name;
                         assert false);
          s_used = 0;
          s_used_last = 0;
        } in
      stats := s :: !stats;
      Hashtbl.add version_stats version_name s;
      s
  in

  let n_installable_versions = ref 0 in
  let n_installable_packages = ref 0 in
  let stats_uninstallable_packages = ref 0 in
  let stats_uninstallable_versions = ref 0 in
  let stats_unavailable_packages = ref 0 in
  let stats_unavailable_versions = ref 0 in
  let stats_error_packages = ref 0 in
  let stats_error_versions = ref 0 in
  StringMap.iter (fun _ p ->
      begin
        match p.package_status with
        | None -> ()
        | Some s ->
           match s.s_status with
           | Installable deps ->
              incr n_installable_packages;
              List.iter (fun (package_name, version_num) ->
                  let s = get_stats package_name version_num in
                  s.s_used_last <- s.s_used_last + 1;

                  if package_name = p.package_name then begin
                      StringMap.iter (fun _ v ->
                          let _, version = OcpString.cut_at v.version_name '.' in
                          if CopamDebian.compare version_num version < 0 then begin
                              Printf.eprintf "Package %s. Selected version %s is not the most recent (%s)\n%!" p.package_name version_num v.version_name;
                            end
                        ) p.package_versions;
                    end
                ) deps;
           | ExternalError -> incr stats_error_packages
           | NotInstallable -> incr stats_uninstallable_packages
           | NotAvailable -> incr stats_unavailable_packages
      end;
      StringMap.iter (fun _ v ->
          match v.version_status with
          | None -> ()
          | Some s ->
             match s.s_status with
             | Installable deps ->
                incr n_installable_versions;
                List.iter (fun (package_name, version_num) ->
                    let s = get_stats package_name version_num in
                    s.s_used <- s.s_used + 1
                  ) deps
             | ExternalError -> incr stats_error_versions
             | NotInstallable -> incr stats_uninstallable_versions
             | NotAvailable -> incr stats_unavailable_versions
        ) p.package_versions;
    ) c.packages;

  let stats_version = Array.of_list !stats in
  let stats_version2 = Array.copy stats_version in

  Array.sort (fun s1 s2 -> compare s1.s_used s2.s_used)
             stats_version;
  Array.sort (fun s1 s2 -> compare s1.s_used_last s2.s_used_last)
             stats_version2;

  let stats_installable_packages = !n_installable_packages in
  let stats_installable_versions = !n_installable_versions in
  let stats_uninstallable_versions = !stats_uninstallable_versions in
  let stats_uninstallable_packages = !stats_uninstallable_packages in
  let stats_unavailable_versions = !stats_unavailable_versions in
  let stats_unavailable_packages = !stats_unavailable_packages in
  let stats_error_packages = !stats_error_packages in
  let stats_error_versions = !stats_error_versions in

  Printf.eprintf "Analyzing commit %s... done\n%!" c.commit_name;

  { stats_switch; stats_version; stats_version2;
    stats_installable_versions; stats_installable_packages;
    stats_uninstallable_packages; stats_uninstallable_versions;
    stats_unavailable_packages; stats_unavailable_versions;
    stats_error_packages; stats_error_versions
  }
