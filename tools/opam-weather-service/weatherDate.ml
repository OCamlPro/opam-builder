
open WeatherMisc

let start = parse_date Sys.argv.(1)

let time = Unix.gettimeofday ()

let daylong = 1. *. 3600. *. 24.

let dates =
  let rec aux acc time =
    if time < start then acc else
      let tm = Unix.gmtime time in
      let date =
        Printf.sprintf "%04d-%02d-%02d"
          (tm.Unix.tm_year + 1900)
          (tm.Unix.tm_mon + 1)
          tm.Unix.tm_mday
      in
      aux (date::acc) (time -. daylong)
  in
  aux [] (Unix.gettimeofday())

let _ =
  List.iter print_endline dates
