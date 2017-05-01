open Common
open Algo
open WeatherTypes


let cudf_file = Sys.argv.(1)

let () =

  let universe = WeatherDiag.load_cudf_universe cudf_file in
  let results = WeatherDiag.initial_summary cudf_file in
  let results = {results with universe} in

  let callback diag =
    let open Algo.Diagnostic in
    let package = match diag.request with
      | [p] -> p
      | _ -> assert false
    in
    let package_info = match diag.result with
      | Success _ -> {package; broken = []}
      | Failure f -> {package; broken = f()}
    in
    results.packages <- package_info :: results.packages
  in

  let nfail =
    Depsolver.univcheck ~global_constraints:[] ~callback universe
  in
  (* The unavailable packages must be absent to perform the checks,
     but we re-add dummy versions now for easier lookups during reports
     generation. *)
  WeatherDiag.add_unav_packages universe cudf_file;

  let report_bin =  "report.bin" in
  let oc = open_out_bin report_bin in
  output_string oc "OPAMWTHR";
  output_char oc '\001';  (* version 1 *)
  output_char oc '\000';
  output_char oc '\000';
  output_char oc '\000';
  output_value oc results;
  close_out oc;
  Printf.eprintf "Binary report saved in %S\n%!" report_bin;
  exit nfail
