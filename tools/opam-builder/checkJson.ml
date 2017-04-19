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

let minify = true

module VersionMap = Map.Make(struct
                              type t = string
                              let compare = Versioning.Debian.compare
                              end)

let of_commits filename cs =

  let dates = L (List.map (fun c -> S c.check_date) cs) in
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
              t.(i) <- state
            ) p.package_versions
        ) c.packages;
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
  let t = O [ "dates", dates;
              "commits", commits;
              "switches", switches;
              "packages", L packages ] in
  let s = OcpJson.to_string ~minify t in
  let oc = open_out filename in
  output_string oc s;
  close_out oc
