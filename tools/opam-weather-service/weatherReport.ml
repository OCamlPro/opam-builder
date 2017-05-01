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

let html_dir = try Sys.getenv "HTMLDIR" with Not_found -> "html"
let fullreport_html = html_dir // "report_full.html"
let report_html = html_dir // "report.html"

let report_dir = Sys.argv.(1)

let date = Sys.argv.(2)
let time, tm =
  Scanf.sscanf date "%i-%i-%i-%i-%i-%i"
    (fun year mon mday hour min sec ->
      Unix.mktime {
        Unix.tm_year = year - 1900;
        Unix.tm_mon = mon - 1;
        Unix.tm_mday = mday;
        Unix.tm_hour = hour;
        Unix.tm_min = min;
        Unix.tm_sec = sec;

        Unix.tm_wday = 0;
        Unix.tm_yday = 0;
        Unix.tm_isdst = false;
      })

let templates_dir = try Sys.getenv "TMPLDIR" with Not_found -> "templates"
let opam_repo_dir = try Sys.getenv "OPAMREPO" with Not_found -> "opam-repository"

let tmp_report_dir = Sys.argv.(3)

let version_pages = ref StringMap.empty

let add_to_refmap key v map =
  try
    let ref = StringMap.find key map in
    ref := v :: !ref;
    map
  with Not_found ->
    StringMap.add key (ref [v]) map

let string_of_ts_name to_record st ts_name =
  String.concat "|| "
    (List.map (fun (pkgname, constr) ->
       (match to_record with None -> ()
                           | Some ti ->
                             st.version_unsat_constraints <-
                               add_to_refmap pkgname (ti, constr)
                                 st.version_unsat_constraints);
       match constr with
       | None ->
         Printf.sprintf "unknown package %S" pkgname;
       | Some ts_constr ->
         Printf.sprintf "<a href=\"%s-%s.html\">%s</a> %s"
           st.ocaml_version
           pkgname
           pkgname
           ts_constr)
       ts_name)

let cudf2ti p =
  let ti_name,ti_version = WeatherReasons.cudf2opam p in
  { ti_package = p;
    ti_name;
    ti_version;
    ti_needed_by = [] }

let make_version_page p v expl =
  let key = p ^ "." ^ v ^ ".html" in
  let page = try
    StringMap.find key !version_pages
  with Not_found ->
    let page = (p, v, ref []) in
    version_pages := StringMap.add key page !version_pages;
    page
  in
  begin match expl, page with
      Some (ocaml_version, universe, root, reasons), (_, _, list) ->
      list := (ocaml_version, universe, cudf2ti root, reasons) :: !list
    | None, _ -> ()
  end;
  key

let print_broken_package oc universe st root reasons version_page =
  Printf.bprintf oc "<p>Broken version <a href=\"%s\">%s %s</a></p>\n"
    version_page root.ti_name root.ti_version;
  Printf.bprintf oc "<table>\n";

  let namelink name =
    if Cudf.lookup_packages universe (Common.CudfAdd.encode name) <> [] then
      Printf.sprintf "<a href=\"%s-%s.html\">%s</a>" st.ocaml_version name name
    else name
  in
  let reason_string = WeatherReasons.string_of_reasons namelink universe reasons in
  Printf.bprintf oc "%s\n" reason_string;
  Printf.bprintf oc "</table>\n";
  ()


let generate_version_pages () =
  StringMap.iter (fun key (p,v, list) ->
    let key = p ^ "." ^ v ^ ".html" in
    let oc = Html.begin_page
        (report_dir // key)
        (Printf.sprintf "Package %s, version %s" p v) in
    let (@>) f g = if Sys.file_exists f then f else g in
    let opam_file =
      Printf.sprintf "%s/packages/%s/%s.%s/opam" opam_repo_dir (String.lowercase p) p v @>
      Printf.sprintf "%s/packages/%s/%s.%s/opam" opam_repo_dir p p v @>
      Printf.sprintf "%s/packages/%s.%s/opam" opam_repo_dir p v @>
      Printf.sprintf "%s/opam/%s.%s.opam" opam_repo_dir p v
    in
    Printf.bprintf oc "<h3>%s</h3>\n" opam_file;
    begin try
      let content = string_of_file opam_file in
      Printf.bprintf oc "<pre>%s</pre>\n" content;
    with e ->
      Printf.eprintf "Warning: %s not found\n%!" opam_file;
      Printf.bprintf oc "<pre>(could not be loaded)</pre>\n"
    end;

    List.iter (fun (st, universe, root, reasons) ->
      Printf.bprintf oc "<h3>OCaml Version %s</h3>\n" st.ocaml_version;
      match reasons with
      | [] -> Printf.bprintf oc "<p>Correct</p>\n"
      | _ ->
        print_broken_package oc universe st root reasons key
    ) (List.sort
         (fun (_, _, p1, _ as x1) (_, _, p2, _ as x2) ->
            let c = compare p1.ti_name p2.ti_name in
            if c <> 0 then c else
            let c = Versioning.Debian.compare p1.ti_version p2.ti_version in
            if c <> 0 then c else
              compare x1 x2)
         !list);
  ) !version_pages


(*
let print_summary oc s =
  List.iter (fun bp ->
    match bp.broken with
    | [] -> ()
    | reasons ->
      print_broken_package oc bp.package reasons
  ) s.packages;
  ()
*)

let load_bin_report filename =
  let ic = open_in filename in
  let s = String.create 12 in
  really_input ic s 0 12;
  let (results : summary) = input_value ic in
  close_in ic;
  results

let repo_version =
  let ic = open_in (html_dir // "new-repo.txt") in
  let line = input_line ic in
  close_in ic;
  line


let make_report report_html results st =

    StringMap.iter (fun ti_name pkg ->
      let oc =
        Html.begin_page
          (report_dir // (Printf.sprintf "%s-%s.html" st.ocaml_version ti_name))
          (Printf.sprintf "%s on version %s" ti_name st.ocaml_version) in

      Printf.bprintf oc "<h3>%s is not available on <a href=\"report-%s.html\">version %s</a></h3> " ti_name st.ocaml_version st.ocaml_version;
      Printf.bprintf oc "<h4>Incorrect versions (%d)</h4>\n"
        (StringMap.cardinal pkg.pkg_incorrect);
      Printf.bprintf oc "<ul>\n";
      StringMap.iter (fun version pkg ->
        Printf.bprintf oc "<li>\n";
        let root = pkg.package in
        let ti = cudf2ti root in
        let version_page =
          make_version_page
            ti.ti_name ti.ti_version (Some (st, results.universe, pkg.package, pkg.broken)) in
        print_broken_package oc results.universe st ti pkg.broken version_page;
        Printf.bprintf oc "</li>\n";
      ) pkg.pkg_incorrect;
      Printf.bprintf oc "</ul>\n";
    ) st.packages_unavailable;


    StringMap.iter (fun ti_name pkg ->
      let oc =
        Html.begin_page
          (report_dir // (Printf.sprintf "%s-%s.html" st.ocaml_version ti_name))
          (Printf.sprintf "%s on version %s" ti_name st.ocaml_version) in

      Printf.bprintf oc "<h3>%s is partially available on <a href=\"report-%s.html\">version %s</a></h3> " ti_name st.ocaml_version st.ocaml_version;

      Printf.bprintf oc "<h4>Installable versions (%d)</h4>\n"
        (StringMap.cardinal pkg.pkg_correct);
      Printf.bprintf oc "<p>All dependencies for these versions can be met.</p>\n";

      Printf.bprintf oc "<ul>\n";
      StringMap.iter (fun version pkg ->
        Printf.bprintf oc "<li><a href=\"%s\">%s</a></li>\n"
          (make_version_page ti_name version (Some (st, results.universe, pkg.package, [])) ) version;
      ) pkg.pkg_correct;
      Printf.bprintf oc "</ul>\n";
      Printf.bprintf oc "<h4>Incorrect versions (%d)</h4><ul>\n"
        (StringMap.cardinal pkg.pkg_incorrect);
      Printf.bprintf oc "<p>Some dependencies for these versions can NEVER be met.</p>\n";

      StringMap.iter (fun version pkg ->
        Printf.bprintf oc "<li>\n";
        let root = pkg.package in
        let ti = cudf2ti root in
        let version_page =
          make_version_page
            ti.ti_name ti.ti_version (Some (st, results.universe, pkg.package,pkg.broken)) in
        print_broken_package oc results.universe st ti pkg.broken version_page;
        Printf.bprintf oc "</li>\n";
      ) pkg.pkg_incorrect;
      Printf.bprintf oc "</ul>\n";
    ) st.packages_partial;




    StringMap.iter (fun ti_name pkg ->
      let oc =
        Html.begin_page
          (report_dir // (Printf.sprintf "%s-%s.html" st.ocaml_version ti_name))
          (Printf.sprintf "%s on version %s" ti_name st.ocaml_version) in

      Printf.bprintf oc "<h3>%s is fully available on <a href=\"report-%s.html\">version %s</a></h3> " ti_name st.ocaml_version st.ocaml_version;
      Printf.bprintf oc "<h4>Installable versions (%d)</h4>\n"
      (StringMap.cardinal pkg.pkg_correct);
      Printf.bprintf oc "<p>All dependencies for these versions can be met.</p>\n";
      Printf.bprintf oc "<p><ul>\n";
      StringMap.iter (fun version pkg ->
        Printf.bprintf oc "<li><a href=\"%s\">%s</a></li>\n"
          (make_version_page ti_name version (Some (st,results.universe,pkg.package,[]))) version;
      ) pkg.pkg_correct;
      Printf.bprintf oc "</ul></p>\n";

    ) st.packages_correct;



  let missing_packages = ref [] in
  let with_missing_constraints = ref [] in

  StringMap.iter (fun ti_name list_ref ->
    let filename =
      report_dir // Printf.sprintf "%s-%s.html" st.ocaml_version ti_name in
    try
      let oc = Html.find_page filename in

      Printf.bprintf oc "<h4>Failed constraints on this package (%d)</h4>\n"
        (List.length !list_ref);
      Printf.bprintf oc "<p>The following packages/versions are not installable, because some constraint cannot be met on this package.</p>";
      Printf.bprintf oc "<p><ul>\n";
      List.iter (fun (ti, constr) ->
        Printf.bprintf oc "<li><a href=\"%s.%s.html\">%s (%s)</a></li>\n" ti.ti_name ti.ti_version  ti.ti_name ti.ti_version
      ) !list_ref;
      Printf.bprintf oc "</ul></p>\n";

      with_missing_constraints := ti_name :: !with_missing_constraints

    with Not_found ->
      let oc =
        Html.begin_page
          (report_dir // Printf.sprintf "%s-%s.html" st.ocaml_version ti_name)
          (Printf.sprintf "%s on version %s" ti_name st.ocaml_version) in

      Printf.bprintf oc "<h3>%s is unknown on <a href=\"report-%s.html\">version %s</a></h3> " ti_name st.ocaml_version st.ocaml_version;
      Printf.bprintf oc "<h4>Missing in the following constraints (%d)</h4>\n"
        (List.length !list_ref);
      Printf.bprintf oc "<ul>\n";
      List.iter (fun (ti, constr) ->
        Printf.bprintf oc "<li><a href=\"%s.%s.html\">%s (%s)</a></li>\n" ti.ti_name ti.ti_version  ti.ti_name ti.ti_version
      ) !list_ref;
      Printf.bprintf oc "</ul>\n";
      missing_packages := ti_name :: !missing_packages;
  ) st.version_unsat_constraints;

  let oc =  Html.begin_page report_html
    (Printf.sprintf "Report for version %s" st.ocaml_version) in
  let tm = results.time in
  Printf.bprintf oc "<h3><a href=\"../index.html\">Return to Home</a></h3>\n";
  Printf.bprintf oc "<h3>Repository: %s</h3>\n" repo_version;
  Printf.bprintf oc "<h3>Generated on %s</h3>\n" (string_of_date tm);
  Printf.bprintf oc "<h3><a href=\"%s.cudf\">CUDF file</a></h3>"
    st.ocaml_version;

  Printf.bprintf oc "<h2>Statistics</h2>\n";

  Printf.bprintf oc "<table>\n";
  Printf.bprintf oc "<tr><td>Total packages</td><td>%d</td></tr>\n" st.total_npackages;
  Printf.bprintf oc "<tr><td>Total installable packages</td><td>%d</td></tr>\n"
    (StringMap.cardinal st.packages_correct +
       StringMap.cardinal st.packages_partial);
  Printf.bprintf oc "<tr><td>Total correct packages</td><td>%d</td></tr>\n"
    (StringMap.cardinal st.packages_correct);
  Printf.bprintf oc "<tr><td>Total <a href=\"#partial\">partial packages</a></td><td>%d</td></tr>\n"
    (StringMap.cardinal st.packages_partial);
  Printf.bprintf oc "<tr><td>Total incorrect packages</td><td>%d</td></tr>\n"
    (StringMap.cardinal st.packages_partial +
       StringMap.cardinal st.packages_unavailable);
  Printf.bprintf oc "<tr><td>Total <a href=\"#unavailable\">unavailable packages </a></td><td>%d</td></tr>\n"
    (StringMap.cardinal st.packages_unavailable);

  Printf.bprintf oc "<tr><td>Total versions</td><td>%d</td></tr>\n" st.total_nversions;
  Printf.bprintf oc "<tr><td>Total installable versions</td><td>%d</td></tr>\n" st.total_nversions_correct;
  Printf.bprintf oc "<tr><td>Total broken versions</td><td>%d</td></tr>\n" st.total_nversions_incorrect;
  Printf.bprintf oc "<tr><td>Total <a href=\"#missing\">missing packages</a></td><td>%d</td></tr>\n" (List.length !missing_packages);
  Printf.bprintf oc "<tr><td>With <a href=\"#failures\">failed constraints</a> </td><td>%d</td></tr>\n"
  (List.length !with_missing_constraints);
  Printf.bprintf oc "</table>\n";

    Printf.bprintf oc "<h2>Unavailable Packages (%d)</h2>\n"
      (StringMap.cardinal st.packages_unavailable);
  Printf.bprintf oc "<a name=\"%s\"> </a>\n" "unavailable";

    Printf.bprintf oc "<p><ul>\n";
    StringMap.iter (fun ti_name pkg ->
      Printf.bprintf oc "<li><a href=\"%s-%s.html\">%s</a></li>\n"
        st.ocaml_version ti_name ti_name;
    ) st.packages_unavailable;
    Printf.bprintf oc "</ul></p>\n";


    Printf.bprintf oc "<h2>Partial Packages (%d)</h2>\n"
      (StringMap.cardinal st.packages_partial);

  Printf.bprintf oc "<a name=\"%s\"> </a>\n" "partial";

    Printf.bprintf oc "<p><ul>\n";
    StringMap.iter (fun ti_name pkg ->
      Printf.bprintf oc "<li><a href=\"%s-%s.html\">%s</a></li>\n" st.ocaml_version ti_name ti_name;
    ) st.packages_partial;
    Printf.bprintf oc "</ul></p>\n";

    Printf.bprintf oc "<h2>Correct Packages (%d)</h2>\n"
      (StringMap.cardinal st.packages_correct);

  Printf.bprintf oc "<a name=\"%s\"> </a>\n" "correct";

    Printf.bprintf oc "<p><ul>\n";
    StringMap.iter (fun ti_name pkg ->
      Printf.bprintf oc "<li><a href=\"%s-%s.html\">%s</a></li>\n" st.ocaml_version ti_name ti_name;
    ) st.packages_correct;
    Printf.bprintf oc "</ul></p>\n";


    Printf.bprintf oc "<h2>Missing Packages (%d)</h2>\n"
      (List.length !missing_packages);
  Printf.bprintf oc "<a name=\"%s\"> </a>\n" "missing";


    Printf.bprintf oc "<p>These packages appear in constraints, but are either not compatible with this version, or are completely unknown.</p>\n";

    Printf.bprintf oc "<p><ul>\n";
    List.iter (fun ti_name ->
      Printf.bprintf oc "<li><a href=\"%s-%s.html\">%s</a></li>\n" st.ocaml_version ti_name ti_name;
    ) !missing_packages;
    Printf.bprintf oc "</ul></p>\n";

    Printf.bprintf oc "<h2>Packages with failed constraints  (%d)</h2>\n"
      (List.length !with_missing_constraints);

    Printf.bprintf oc "<a name=\"%s\"> </a>\n" "failures";

    Printf.bprintf oc "<p>These packages already appeared before. They appear here again because they are causing broken constraints on other packages.</p>\n";

    Printf.bprintf oc "<p><ul>\n";
    List.iter (fun ti_name ->
      Printf.bprintf oc "<li><a href=\"%s-%s.html\">%s</a></li>\n" st.ocaml_version ti_name ti_name;
    ) !with_missing_constraints;
    Printf.bprintf oc "</ul></p>\n";

  ()

let make_version_report version results =

  let st = {
    ocaml_version = version;
    total_nversions_incorrect = 0;
    total_npackages = 0;
    packages_correct = StringMap.empty;
    packages_partial = StringMap.empty;
    packages_unavailable = StringMap.empty;
    total_nversions = 0;
    total_nversions_correct = 0;
    total_packages = StringMap.empty;

    version_unsat_constraints = StringMap.empty;
  } in

  let get_pkg_stats ti_name =
    try
      StringMap.find ti_name st.total_packages
    with Not_found ->
      let pkg = {
        pkg_name = ti_name;
        pkg_correct = StringMap.empty;
        pkg_incorrect = StringMap.empty;
      } in
      st.total_packages <- StringMap.add ti_name pkg st.total_packages;
      st.total_npackages <- st.total_npackages + 1;
      pkg
  in
  List.iter (fun package ->
      let name,version = WeatherReasons.cudf2opam package.package in
      let pkg = get_pkg_stats name in
      match package.broken with
      | [] ->
          pkg.pkg_correct <- StringMap.add version package pkg.pkg_correct;
          st.total_nversions_correct <- st.total_nversions_correct + 1
      | _reasons ->
          st.total_nversions <- st.total_nversions + 1;
          pkg.pkg_incorrect <- StringMap.add version package pkg.pkg_incorrect;
          st.total_nversions_incorrect <- st.total_nversions_incorrect + 1
    ) results.packages;

  StringMap.iter (fun ti_name pkg ->
    if pkg.pkg_incorrect = StringMap.empty then begin
      st.packages_correct <-
        StringMap.add ti_name pkg st.packages_correct
    end else
    if pkg.pkg_correct = StringMap.empty then begin
      st.packages_unavailable <- StringMap.add ti_name pkg
          st.packages_unavailable
    end else begin
      st.packages_partial <- StringMap.add ti_name pkg
          st.packages_partial
    end
  ) st.total_packages;


  let reporthtml = report_dir // Printf.sprintf "report-%s.html" version in
  make_report reporthtml results st;

  st

let generate_table results =

  let packages = ref StringMap.empty in
  Array.iter
    (fun (ocaml_version, results) ->
       List.iter
         (fun pkg ->
            let name, version = WeatherReasons.cudf2opam pkg.package in
            let versions =
              try StringMap.find name !packages
              with Not_found ->
                let versions = ref StringMap.empty in
                packages := StringMap.add name versions !packages;
                versions
            in
            let ocaml_versions =
              try StringMap.find version !versions
              with Not_found ->
                let ocaml_versions = Array.create nocaml_versions None in
                versions :=
                  StringMap.add version ocaml_versions !versions;
                ocaml_versions
            in
            ocaml_versions.(ocaml_version) <- Some (pkg.broken = []))
         results.packages)
    results;

  let oc = Html.begin_page (report_dir // "table.html") "Table" in

  Printf.bprintf oc "<h3><a href=\"../index.html\">Return to Home</a></h3>\n";
  Printf.bprintf oc "<h3>Repository: %s</h3>\n" repo_version;
  Printf.bprintf oc "<h3>Generated on %s</h3>\n" (string_of_date tm);
  Printf.bprintf oc
    "<style type=\"text/css\">.ok {background-color:green} \
    \ .bad {background-color:red}</style>\n";
  Printf.bprintf oc "<table><tr><th>package</th><th>version</th>";
  Array.iter
    (fun v -> Printf.bprintf oc "<th>%s</th>" v)
    ocaml_version_table;
  Printf.bprintf oc "</tr>\n";

  StringMap.iter (fun package versions ->
    let first_version = ref true in
    StringMap.iter (fun version ocaml_versions ->
      if !first_version then begin
        Printf.bprintf oc "<tr><th><a name=\"%s\">%s</a></th>" package package;
        first_version := false
      end else
        Printf.bprintf oc "<tr><th></th>";
      Printf.bprintf oc
        "<td><a name=\"%s.%s\">%s</a></td>" package version version;
      Array.iteri (fun ocaml_version ok ->
        (* let ocaml_version = ocaml_version_table.(ocaml_version) in *)
        match ok with
        | None ->
          Printf.bprintf oc "<td></td>"
        | Some ok ->
          let ok = if ok then "ok" else "bad" in
          Printf.bprintf oc
            "<td class=\"%s\"><a href=\"%s.%s.html\">%s</a></td>"
            ok package version ok
      ) ocaml_versions;
      Printf.bprintf oc "</tr>\n";
    ) !versions
  ) !packages;

  Printf.bprintf oc "</table>\n"


let _ =
  (try Unix.mkdir html_dir 0o755 with _ -> ());
  (try Unix.mkdir report_dir 0o755 with _ -> ());

  let oc = open_out (report_dir // "report.txt") in
  Printf.fprintf oc "repo:%s\n" repo_version;
  Printf.fprintf oc "date:%.f\n" time;
  Printf.fprintf oc "dir:%s\n" (Filename.basename report_dir);
  let reports =
    Array.mapi
      (fun ocaml_version_idx ocaml_version ->
         let results =
           load_bin_report
             (tmp_report_dir // Printf.sprintf "%s-all-1.bin" ocaml_version) in

         let _version_report = make_version_report ocaml_version results in
         List.iter
           (fun p ->
              let name, version = WeatherReasons.cudf2opam p.package in
              Printf.fprintf oc "%s:%s:%s:%s\n"
                (match p.broken with
                   [] -> "ok"
                | _ -> "bad")
                ocaml_version
                name
                version)
           results.packages;
         (ocaml_version_idx, results))
      ocaml_version_table in
    close_out oc;

  generate_version_pages ();

  generate_table reports;

  Html.end_pages ()
