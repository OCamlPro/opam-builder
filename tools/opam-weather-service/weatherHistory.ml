(*******************************************************************)
(*                                                                 *)
(*   Copyright (C) 2014, OCamlPro SAS & INRIA                      *)
(*   Fabrice Le Fessant                                            *)
(*                                                                 *)
(*******************************************************************)

open WeatherTypes
open WeatherMisc
open WeatherConfig

let (//) = Filename.concat

type event_kind =
  | EventNew of bool (* initial status *)
  | EventChange of bool (* old *) * bool (* new *)
  | EventRemove of bool (* last status *)

type event = {
  event_date : float;
  event_line : string;
  event_kind : event_kind;
}

type report = {
  report_repo : string;
  report_date : float;
  report_dir : string;
  report_lines : bool StringMap.t;
}

type history = {
  mutable first_report : bool;
  mutable lines : bool StringMap.t;
  mutable history : event list;
  mutable last_reportdir : string;
  mutable last_date : float;
}


let initial_history () =
  {
    lines =  StringMap.empty;
    history =  [];
    first_report = true;
    last_reportdir =  "";
    last_date = 0.;
  }

let load_history h history_file =
  let ic = open_in_bin history_file in
  h.first_report <- input_value ic;
  h.lines <- input_value ic;
  h.history <- input_value ic;
  h.last_reportdir <- input_value ic;
  h.last_date <- input_value ic;
  close_in ic

let load_report filename =
  Printf.eprintf "Loading %S\n%!" filename;
  let lines = lines_of_file filename in
  let report_repo = ref "" in
  let report_dir = ref "" in
  let report_date = ref 0. in
  let report_lines = ref StringMap.empty in
  List.iter (fun line ->
    match cut_at line ':' with
    | ("repo", repo) -> report_repo := repo
    | ("dir", dir) -> report_dir := dir
    | ("date", date) -> report_date := float_of_string date
    | ("ok", line) ->
      report_lines := StringMap.add line true !report_lines
    | ("bad", line) ->
      report_lines := StringMap.add line false !report_lines
    | _ -> assert false
  ) lines;
  {
    report_repo = !report_repo;
    report_date = !report_date;
    report_lines = !report_lines;
    report_dir = !report_dir;
  }

let string_of_ok = function true -> "ok" | false -> "bad"

let string_of_event_kind = function
  | EventNew ok -> Printf.sprintf "New version %s" (string_of_ok ok)
  | EventChange (old, to_new) ->
    Printf.sprintf "Status changed from %s to %s"
      (string_of_ok old)
      (string_of_ok to_new)
  | EventRemove ok ->
    Printf.sprintf "Version removed"

let daylong = 3600. *. 24.

let print_history filename =
  let h = initial_history () in
  load_history h filename;
  let history = List.rev h.history in
  List.iter (fun e ->
    Printf.printf "%s %s %s\n%!"
      (string_of_date (Unix.gmtime e.event_date))
      e.event_line
      (string_of_event_kind e.event_kind)
  ) history;
  ()

let plot_history filename =
  let h = initial_history () in
  load_history h filename;
  let history = List.rev h.history in

  let packages = Hashtbl.create 111 in
  (* pkg*v => bool array (x.(ocaml vers) = exists) *)
  let packages_versions = Hashtbl.create 373 in
  let packages_with_version = Array.init nocaml_versions
      (fun _ -> Hashtbl.create 111) in
  (* let packages_by_ocaml_version = Array.init nocaml_versions *)
  (*     (fun _ -> Hashtbl.create 111) in *)
  let initial_date = match history with
      [] -> Printf.eprintf "Error: %s: empty history\n" filename; 0.
    | event :: _ -> event.event_date
  in

  let current_date = Unix.gettimeofday () in
  let period = (current_date -. initial_date) /. daylong in
  let period = int_of_float period + 2 in
  let npackages = Array.create period 0 in
  let npackages_versions = Array.create period (-1) in
  (* working *)
  let npackages_with_version = Array.init nocaml_versions
      (fun _ -> Array.create period 0) in
  (* all *)
  let total_packages_with_version = Array.init nocaml_versions
      (fun _ -> Array.create period 0) in
  List.iter (fun e ->
    (* let day = e.event_date in *)
    let day =
      int_of_float ((e.event_date -. initial_date) /. daylong +. 0.001 ) in
    let (ocaml_version, pkg_with_version) = cut_at e.event_line ':' in
    let (pkg, version) = cut_at pkg_with_version ':' in
    let ocaml_version = index_of_ocaml_version ocaml_version in

    if not (Hashtbl.mem packages pkg) then
      Hashtbl.add packages pkg pkg;
    npackages.(day) <- npackages.(day) + 1;

    let total = total_packages_with_version.(ocaml_version).(day) in
    let existing =
      try Hashtbl.find packages_versions (pkg,version)
      with Not_found ->
        let a = Array.create nocaml_versions false in
        Hashtbl.add packages_versions (pkg,version) a;
        a
    in
    existing.(ocaml_version) <- true;
    (match e.event_kind with
     | EventRemove _ ->
       total_packages_with_version.(ocaml_version).(day) <- total - 1;
       existing.(ocaml_version) <- false;
       if not (Array.fold_left (||) false existing) then
         Hashtbl.remove packages_versions (pkg,version)
     | EventNew _ ->
       total_packages_with_version.(ocaml_version).(day) <- total + 1
     | _ -> ());
    let npackages_with_version = npackages_with_version.(ocaml_version) in
    let packages_with_version = packages_with_version.(ocaml_version) in
    npackages_versions.(day) <- Hashtbl.length packages_versions;
    if not (Hashtbl.mem packages_with_version pkg_with_version) then
      match e.event_kind with
      | EventNew true
      | EventChange (false, true) ->
        (*      Printf.printf "new pkg = %S\n" pkg; *)
        Hashtbl.add packages_with_version pkg_with_version pkg_with_version;
        npackages_with_version.(day) <- npackages_with_version.(day) + 1;
      | EventChange (true, false) -> ()
      | EventRemove _ | EventNew false -> ()
      | EventChange (true, true)
      | EventChange (false, false)
        -> assert false
    else begin
      match e.event_kind with
      | EventNew true ->
        Printf.eprintf "Warning: EventNew true for %s already in base\n%!"
          pkg_with_version;
      | EventChange (false, true) -> assert false
      | EventChange (true, false) | EventRemove true ->
        Hashtbl.remove packages_with_version pkg_with_version;
        npackages_with_version.(day) <- npackages_with_version.(day) - 1;
      | EventNew false -> ()
      | EventRemove false -> ()
      | EventChange (true, true)
      | EventChange (false, false) -> assert false
    end)
    history;

  for i = 1 to period - 1 do
    let prev a = a.(i-1) in
    npackages.(i) <- prev npackages + npackages.(i);
    if npackages_versions.(i) < 0 then
      npackages_versions.(i) <- if i > 0 then prev npackages_versions else 0;
    for o = 0 to nocaml_versions - 1 do
      npackages_with_version.(o).(i) <- prev npackages_with_version.(o) + npackages_with_version.(o).(i);
      total_packages_with_version.(o).(i) <- prev total_packages_with_version.(o) + total_packages_with_version.(o).(i);
    done;
  done;

  (* for i = 1 to 373 do *)
  (*   npackages_with_version.(2).(i) <- 0; *)
  (* done; *)
  let oc = open_out "packages.out" in
  Printf.fprintf oc "# date npackages nversions";
  Array.iter
   (fun v -> Printf.fprintf oc " nworking(%s) ntotal(%s)" v v)
   ocaml_version_table;
  Printf.fprintf oc "\n";
  for i = 0 to period - 1 do
    let time = (initial_date +. daylong *. (float_of_int i)) in
    let date = Unix.gmtime time in
    Printf.fprintf oc "%04d-%02d-%02d %4d %4d"
      (date.Unix.tm_year + 1900) (date.Unix.tm_mon + 1) (date.Unix.tm_mday)
      npackages.(i)
      npackages_versions.(i);
    for j = 0 to nocaml_versions - 1 do
      if time >= ocaml_version_day.(j) then
        Printf.fprintf oc " %4d %4d"
          npackages_with_version.(j).(i)
          total_packages_with_version.(j).(i)
      else
        Printf.fprintf oc " %4d %4d" 0 0
    done;
    Printf.fprintf oc "\n"
  done;
  close_out oc


let update_history () =

  let h = initial_history () in

  let history_file = html_dir // "history.bin" in
  if Sys.file_exists history_file then load_history h history_file;

  let loaded = ref [] in

  let dirs = Sys.readdir html_dir in
  Array.sort compare dirs;

  Array.iter (fun file ->

    let filename = html_dir // file in

    if Filename.check_suffix file ".txt" &&
       String.length file >= 7 && String.sub file 0 7 = "report-" then
      let report = load_report filename in
      Printf.eprintf "Date: %s -- lines: %d\n"
        (string_of_date (Unix.gmtime report.report_date))
        (StringMap.cardinal report.report_lines);
      loaded := filename :: !loaded;
      let add date line kind =
        h.history <- {
          event_date = date;
          event_line = line;
          event_kind = kind;
        } :: h.history
      in
      StringMap.iter (fun line ok ->
        try
          let old = StringMap.find line h.lines in
          if old <> ok then begin
            add report.report_date line (EventChange (old, ok))
          end;
          h.lines <- StringMap.remove line h.lines
        with Not_found ->
          add report.report_date line (EventNew ok)
      ) report.report_lines;

      StringMap.iter (fun line ok ->
        add report.report_date line (EventRemove ok)
      ) h.lines;

      h.lines <- report.report_lines;

      if h.first_report then
        h.first_report <- false;

      h.last_reportdir <- report.report_dir;
      h.last_date <- report.report_date)
    dirs;

  let new_history_file = history_file ^ ".new" in
  flush stdout;
  Printf.eprintf
    "Writing %s (%d lines)\n%!"
    new_history_file (StringMap.cardinal h.lines);
  let oc = open_out_bin new_history_file in
  output_value oc h.first_report;
  output_value oc h.lines;
  output_value oc h.history;
  output_value oc h.last_reportdir;
  output_value oc h.last_date;
  close_out oc;
  (try Sys.remove history_file with _ -> ());
  Sys.rename new_history_file history_file;

  List.iter (fun filename ->
    Sys.rename filename (filename ^ ".loaded")
  ) !loaded;

  let index_html = html_dir // "index.html" in
  let new_html = index_html ^ ".new" in
  let oc = open_out new_html in
  let begin_file = string_of_file (templates_dir // "index.header") in
  let end_file = string_of_file (templates_dir // "index.trailer") in

  output_string oc begin_file;
  let tm = Unix.gmtime h.last_date in
  Printf.fprintf oc
    "<h3>Latest report generated on %s</h3>" (string_of_date tm);
  Printf.fprintf oc "<ul>\n";
  Array.iter (fun version ->
    Printf.fprintf oc
      "<li><a href=\"%s/report-%s.html\">For version %s</a></li>\n"
      (Filename.basename h.last_reportdir) version version;
  ) ocaml_version_table;
  Printf.fprintf oc
    "<li><a href=\"%s/table.html\">Summary table</a></li>\n"
    (Filename.basename h.last_reportdir);
  Printf.fprintf oc "</ul>\n";

  output_string oc end_file;

  close_out oc;

  (try Sys.remove index_html with _ -> ());
  Sys.rename new_html index_html


let nothing_done = ref true

let arg_usage = "opam-weather-history [OPTIONS] : update history file"
let arg_list = [
  "-plot", Arg.String (fun filename ->
    nothing_done := false;
    plot_history filename;
  ), "FILENAME Plot content of history file to 'history.out'";

  "-print", Arg.String (fun filename ->
    nothing_done := false;
    print_history filename;
  ), "FILENAME Print content of history file";

]
let arg_anon _ =
  Arg.usage arg_list arg_usage

let _ =
  Arg.parse arg_list arg_anon arg_usage;
  if !nothing_done then
    update_history ()


