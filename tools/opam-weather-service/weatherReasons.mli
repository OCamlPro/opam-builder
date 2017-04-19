
val cudf2opam : Cudf.package -> string * string
val string_of_reasons :
  (string -> string) ->
  Cudf.package ->
  Cudf.universe ->
  Algo.Diagnostic.reason list -> string
