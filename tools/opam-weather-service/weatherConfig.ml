
let ocaml_version_table =
  [| "3.12.1"; "4.00.1"; "4.01.0"; "4.02.0" |]
let ocaml_version_day =
  Array.map WeatherMisc.parse_date
    [| "2012-05-19"; "2012-07-26"; "2013-09-13"; "2014-08-26" |]
let index_of_ocaml_version = function
  | "3.12.1" -> 0
  | "4.00.1" -> 1
  | "4.01.0" -> 2
  | "4.02.0" -> 3
  | v -> failwith @@ "Unknown version of OCaml: " ^ v
let nocaml_versions = Array.length ocaml_version_table

let html_dir = try Sys.getenv "HTMLDIR" with Not_found -> "html"
let templates_dir = try Sys.getenv "TMPLDIR" with Not_found -> "templates"

