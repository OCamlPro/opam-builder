(*******************************************************************)
(*                                                                 *)
(*   Copyright (C) 2014, OCamlPro SAS & INRIA                      *)
(*   Fabrice Le Fessant                                            *)
(*                                                                 *)
(*******************************************************************)

open WeatherTypes

let (//) = Filename.concat

let string_of_file filename =
  let ic = open_in filename in
  let b = Buffer.create 0x10000 in
  let buff = String.create 0x1000 in
  let rec copy () =
    let n = input ic buff 0 0x1000 in
    if n = 0 then begin
      close_in ic;
      Buffer.contents b
    end else
      (Buffer.add_substring b buff 0 n; copy())
  in
  copy()

let lines_of_file file =
  let ic = open_in file in
  let lines = ref [] in
  begin try
          while true do
            lines := input_line ic :: !lines
          done
    with End_of_file -> ()
  end;
  close_in ic;
  List.rev !lines


let before s pos = String.sub s 0 pos
let after s pos =
  let len = String.length s in
  String.sub s pos (len - pos)

let cut_at s c =
    try
      let pos = String.index s c in
        before s pos,
      after s (pos+1);
    with _ -> s, ""

let string_of_date tm =
  Printf.sprintf "%04d/%02d/%02d at %02d:%02d (UTC)"
      (1900+tm.Unix.tm_year)
      (1+tm.Unix.tm_mon)
      tm.Unix.tm_mday
      tm.Unix.tm_hour
      tm.Unix.tm_min

module Html = struct

  type t = Buffer.t

  let files = ref StringMap.empty

  let escape s = s (* TODO *)

  let begin_page filename title =
    let b = Buffer.create 1000 in
    files := StringMap.add filename b !files;

    Printf.bprintf b "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 Transitional//EN\">
<html>
  <head>
    <meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\">
    <title>%s</title>
    <link rel=\"shortcut icon\" href=\"/favicon.ico\">
    <link type=\"text/css\" rel=\"stylesheet\" href=\"style.css\">
  </head>

  <body>
    <h1>%s</h1>" (escape title) (escape title);
    b

  let find_page filename = StringMap.find filename !files

  let end_pages () =
    StringMap.iter (fun filename b ->
      Printf.bprintf b "</body></html>";
      let oc = open_out filename in
      Buffer.output_buffer oc b;
      close_out oc;
    ) !files;
    files := StringMap.empty

end

let sort_nodups compare l =
  let rec remove_duplicates = function
    | a::(b::_ as r) when a = b -> remove_duplicates r
    | a::r -> a::remove_duplicates r
    | [] -> []
  in
  remove_duplicates (List.sort compare l)

module OString = struct
  type t = string
  let compare = compare
end

module StringSet = Set.Make(OString)

let parse_date s =
  Scanf.sscanf s "%d-%d-%d" (fun y m d ->
      fst (Unix.mktime {
          Unix.
          tm_sec = 0;
          tm_min = 0;
          tm_hour = 0;
          tm_mday = d;
          tm_mon = m - 1;
          tm_year = y - 1900;
          (* recomputed *)
          tm_wday = 0;
          tm_yday = 0;
          tm_isdst = false;
        }))
