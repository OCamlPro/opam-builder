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
open OcpJson.TYPES
open CopamInstall.TYPES
open CheckTypes
open CheckTypes.OP

let minify = true

module VersionMap = Map.Make(struct
                              type t = string
                              let compare = Versioning.Debian.compare
                              end)

let of_commit ~replace_commit_tree c =
  let dirname = Printf.sprintf "%s-%s-%s.files"
                               c.timestamp_date c.commit_name c.switch in
  if Sys.file_exists dirname &&
       not replace_commit_tree then ()
  else
    let () = () in
    if not ( Sys.file_exists dirname ) then
      Unix.mkdir dirname 0o755;
    StringMap.iter (fun _ p ->
        StringMap.iter (fun _ v ->
            let status =
              match v.version_result with
              | Some true -> "Ok"
              | Some false -> "Fail"
              | None ->
                 match v.version_status with
                 | None -> "NotChecked"
                 | Some status ->
                    match status.s_status with
                    | ExternalError -> "Error"
                    | NotAvailable -> "Missing"
                    | NotInstallable -> "BadDeps"
                    | Installable _ -> "NotBuilt"
            in
            let t = O [
                        "package_name", S p.package_name;
                        "version_name", S v.version_name;
                        "status", S status;
                        "timestamp_date", S c.timestamp_date;
                        "check_date", S c.check_date;
                        "switch_name", S c.switch;
                        "commit_name", S c.commit_name;
                        "build_result",
                        S (match v.version_result with
                           | None -> "not available"
                           | Some true -> "Success"
                           | Some false -> "Failed");
                        "build_log",
                        S (match v.version_log with
                           | None -> "not available"
                           | Some s -> s);
                        "build_info",
                        S (match v.version_build with
                           | None -> "not available"
                           | Some s -> "available");
                        "depends",
                        L (match v.version_status with
                           | Some { s_status = Installable deps } ->
                              List.map (fun (s,v) ->
                                  S (s ^ "." ^ v)) deps
                           | _ -> [])
                      ]
            in

            let s = OcpJson.to_string ~minify t in
            (*
            let dirname = dirname // p.package_name in
            if not (Sys.file_exists dirname) then
              Unix.mkdir dirname 0o755; *)
            let filename = dirname //(v.version_name ^ ".json") in
            let oc = open_out filename in
            output_string oc s;
            close_out oc

          ) p.package_versions
      ) c.packages;
    ()


let of_commits ~replace_commit_tree filename title cs =

  let dates = L (List.map (fun c -> S c.check_date) cs) in
  let timestamps = L (List.map (fun c -> S c.timestamp_date) cs) in
  let commits = L (List.map (fun c -> S c.commit_name) cs) in
  let switches = L (List.map (fun c -> S c.switch) cs) in

  let ncommits = List.length cs in

  let map = ref StringMap.empty in
  let get_package p =
    try
      StringMap.find p.package_name !map
    with
      Not_found ->
      let x = (p.package_name, ref VersionMap.empty) in
      map := StringMap.add p.package_name x !map;
      x
  in
  let get_version v =
    let (_, map) = get_package v.version_package in
    let _, version = OcpString.cut_at v.version_name '.' in
    try
      VersionMap.find version !map
    with
      Not_found ->
      let x = (v.version_name, Array.make ncommits "") in
      map := VersionMap.add version x !map;
      x
  in
  OcpList.iteri (fun i c ->
      StringMap.iter (fun _ p ->
          StringMap.iter (fun _ v ->
              let _, t = get_version v in
              let state =
                match v.version_result with
                | Some true -> "Ok"
                | Some false -> "Fail"
                | None ->
                   match v.version_status with
                   | None -> "NotChecked"
                   | Some status ->
                      match status.s_status with
                      | ExternalError -> "Error"
                      | NotAvailable -> "Missing"
                      | NotInstallable -> "BadDeps"
                      | Installable _ -> "NotBuilt"
              in
              t.(i) <- state;
            ) p.package_versions
        ) c.packages;
      of_commit replace_commit_tree c;
    ) cs;

  let packages = ref [] in
  StringMap.iter (fun _ (package_name, map) ->
      let versions = ref [] in
      VersionMap.iter (fun _ (version_name, vv) ->
          let v = O [
                      "v", S version_name;
                      "r", L (List.map (fun s -> S s) (Array.to_list vv));
                    ] in
          versions := v :: !versions
        ) !map;
      let p =
        O [
            "p", S package_name;
            "v", L (List.rev !versions);
          ]
      in
      packages := p :: !packages
    ) !map;
  let packages = List.rev !packages in
  let t = O [
              "title", S title;
              "dates", dates;
              "timestamps", timestamps;
              "commits", commits;
              "switches", switches;
              "packages", L packages ] in
  let s = OcpJson.to_string ~minify t in
  let oc = open_out filename in
  output_string oc s;
  close_out oc
