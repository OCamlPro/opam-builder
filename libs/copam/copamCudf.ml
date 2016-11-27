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



let solver_cmd cudf_file solution_file =
  Printf.sprintf "aspcud %s %s '-count(removed),-notuptodate(request),-sum(request,version-lag),-count(down),-notuptodate(changed),-count(changed),-notuptodate(solution),-sum(solution,version-lag)' > /dev/null"
    cudf_file solution_file

exception CorruptedCudfFile
exception SolverFailed
exception CorruptedSolutionFile

type t = {
  cudf_lines : string;
  cudf2opam : (string * string, string * string) Hashtbl.t;
  opam2cudf : (string, string * string) Hashtbl.t;
}

let parse_cudf cudf_file =
  let last_package = ref None in
  let last_version = ref None in
  let last_name = ref None in
  let b = Buffer.create 100000 in
  let cudf2opam = Hashtbl.create 10000 in
  let opam2cudf = Hashtbl.create 10000 in
  try
    FileString.iter_lines (fun line ->
      Printf.bprintf b "%s\n" line;
      if String.length line > 0 then
        let header, value = OcpString.cut_at line ' ' in
        match header with
        | "package:" -> last_package := Some value
        | "version:" -> last_version := Some value
        | "opam-name:" -> last_name := Some value
        | "opam-version:" ->
          begin match !last_package, !last_version, !last_name with
          | Some package, Some version, Some name ->
            last_package := None;
            last_version := None;
            last_name := None;
            let nv = name ^ "." ^ value in
            Hashtbl.add cudf2opam (package, version) (name, value);
            Hashtbl.add opam2cudf nv (package, version);
          | _ ->
            Printf.eprintf "Error: incomplete package with opam-version: %s\n%!"
              value;
            raise CorruptedCudfFile
          end
        | "request:" -> raise Exit
        | _ -> ()
    ) cudf_file;
    Printf.eprintf "Error: no request in cudf file\n%!";
    raise CorruptedCudfFile
  with Exit ->
    let cudf_lines = Buffer.contents b in
    { cudf_lines; cudf2opam; opam2cudf }

let parse_solution cudf solution_file =
  let packages = ref [] in
  try
    let last_package = ref None in
    FileString.iter_lines (fun line ->
      if line = "FAIL" then raise Exit;
      if String.length line > 0 then
        let header, value = OcpString.cut_at line ' ' in
        match header with
        | "package:" -> last_package := Some value
        | "version:" ->
          begin
            match !last_package with
            | Some package ->
              let nv = Hashtbl.find cudf.cudf2opam (package, value) in
              packages := nv :: !packages
            | None ->
              Printf.eprintf "Error: incomplete package with version: %s\n%!"
                value;
            raise CorruptedSolutionFile
          end
        | _ -> ()
    ) solution_file;
    Some (List.sort compare !packages)
  with Exit -> None

let call_aspcud cudf nv =
  let cudf_file = Filename.temp_file nv ".cudf" in
  let solution_file = Filename.temp_file nv ".solution" in
  let oc = open_out cudf_file in
  output_string oc cudf.cudf_lines;
  begin
    try
      let (package, version) = Hashtbl.find cudf.opam2cudf nv in
      Printf.fprintf oc "install: %s = %s\n" package version;
    with
    | Not_found ->
      (* Probably, no version was specified *)
      Printf.fprintf oc "install: %s\n" nv;
  end;
  close_out oc;
  let exitcode = Sys.command (solver_cmd cudf_file solution_file) in
  if exitcode <> 0 then begin
    Printf.eprintf "Error: aspcud returned status %d\n%!" exitcode;
    raise SolverFailed
  end;
  let solution = parse_solution cudf solution_file in
  (try Sys.remove solution_file with _ -> ());
  (try Sys.remove cudf_file with _ -> ());
  solution
