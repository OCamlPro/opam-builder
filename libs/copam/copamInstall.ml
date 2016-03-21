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



(* Manage an opam installation in an opam-repository ! *)

open CopamMisc

type status =
| ExternalError
| NotAvailable
| NotInstallable
| Installable of (string * string) list (* (name * version) list *)

exception AtLeastOneSwitch

let exit2_ifnot b = if not b then exit 2

type t = { rootdir : string; }

let opam_command = ref "opam.dev"

let opam_cmd t cmd =
  Printf.sprintf "%s %s --root %s" !opam_command cmd t.rootdir

let init ?opam rootdir switches =
  begin match opam with
    None -> ()
  | Some opam -> opam_command := opam
  end;
  let t = { rootdir } in

  if not (Sys.file_exists ".opam") then begin
    match switches with
      [] -> raise AtLeastOneSwitch
    | switch :: _ ->
(* BUG-TODO: even with --jobs=1, it seems opam 1.3~dev does not correctly
   set the jobs variable in .opam/config. *)

      exit2_ifnot (Printf.kprintf command
                     "%s --jobs=1 --compiler %s --switch %s --no-setup --use-internal-solver local ."
                     (opam_cmd t "init")
                     switch switch);
  end;
  List.iter (fun switch ->
    if not (Sys.file_exists (Filename.concat rootdir switch)) then begin
      exit2_ifnot (Printf.kprintf command "%s %s" (opam_cmd t "switch") switch)
    end
  ) switches;
  t

let get_json_field json name =
  let rep = ref None in
  try
    match json with
    | OcpJson.O fields ->
      List.iter (fun (field_name, field_value) ->
        if field_name = name then begin rep := Some field_value; raise Exit end
      ) fields;
      Printf.eprintf "get_json_field %S: Could not find field\n%!" name;
      raise Not_found
    | _ ->
      Printf.eprintf "get_json_field %S: Bad format\n%!" name;
      raise Not_found
  with Exit ->
    match !rep with
    | None -> raise Not_found
    | Some v -> v

let parse_json_solution =
  function
  | OcpJson.A installs  ->
    let packages = List.map (fun d ->
      match d with
        OcpJson.O [ "install",
                    OcpJson.O [
                      "name", OcpJson.String name;
                      "version", OcpJson.String version;
                    ]] -> (name, version)
      | _ ->
        Printf.eprintf "parse_json_solution: no %S field\n%!" "install";
        raise Exit
    ) installs in
    let packages = List.sort compare packages in
    packages
  | _ ->
    Printf.eprintf "parse_json_solution: bad format\n%!";
    raise Exit

let packages_of_json json =
  let d = OcpJson.of_string json in
  try
    match d with
    | OcpJson.A [ solution ] -> parse_json_solution solution

    | OcpJson.A [] -> [ ]

    | OcpJson.O list ->
      begin try
        let solution = get_json_field d "solution" in
        parse_json_solution solution
        with Not_found -> raise Exit
      end

    | _ -> raise Exit
  with _ ->
    OcpJson.print d;
    Printf.printf "%!";
    failwith "status_of_json"

let check_install t cudf ~switch version =
  match !cudf with
  | None ->
    let json_file = "check-install.json" in
    let log_file = "check-install.log" in
    let cudf_file = "check-install-1.cudf" in
    (try Sys.remove json_file with _ -> ());
    (try Sys.remove log_file with _ -> ());
    let status =
      let cmd = Printf.sprintf
        "%s --switch %s --dry-run --cudf=check-install --json=%s --quiet --show-actions %s > %s 2>&1"
        (opam_cmd t "install") switch json_file version log_file in
      Printf.eprintf "cmd=%s\n%!" cmd;
      match Sys.command cmd with
      | 0 ->
        Printf.eprintf "reading json..\n%!";
        if not (Sys.file_exists json_file) then
          Printf.eprintf "Json does not exist ???\n%!";
        let json = File.string_of_file json_file in
        Printf.eprintf "removing json..\n%!";
        (try Sys.remove json_file with _ -> ());
        begin
          try
            let cudf_content = CopamCudf.parse_cudf cudf_file in
            cudf := Some cudf_content
          with _ ->
            Printf.eprintf "Warning: Could not parse cudf file\n%!";
        end;
        Installable (packages_of_json json)
      | 66 -> NotAvailable
      | 3 -> NotInstallable
      | exit -> ExternalError
    in
    let log_content = File.string_of_file log_file in
    (try Sys.remove log_file with _ -> ());
    (status, log_content)
  | Some cudf ->
    let status =
      try
        match CopamCudf.call_aspcud cudf version with
        | None -> NotInstallable
        | Some deps -> Installable deps
      with
      | Not_found -> NotAvailable
      | exn ->
        Printf.eprintf "Warning: aspcud error %s\n%!" (Printexc.to_string exn);
        ExternalError
    in
    (status, "direct call to aspcud\n")
