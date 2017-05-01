(*******************************************************************)
(*                                                                 *)
(*   Copyright (C) 2014, OCamlPro SAS & INRIA                      *)
(*   Fabrice Le Fessant                                            *)
(*                                                                 *)
(*******************************************************************)

module StringSet = Set.Make(String)
module StringMap = Map.Make(String)

type to_install = {
  ti_package : Cudf.package;

  ti_name : string;
  ti_version : string;

  mutable ti_needed_by : to_solve list;
}

and to_solve = {
  ts_name : (string * string option) list;

  mutable ts_needed_by : to_install list;
}

(* A broken package *)
type package = {
  package : Cudf.package;
  broken : Algo.Diagnostic.reason list;
}

type summary = {
  time : Unix.tm;
  cudf : Digest.t;
  universe : Cudf.universe;
  mutable packages : package list;
}


type pkg_stats = {
  pkg_name : string;
  mutable pkg_correct : package StringMap.t;
  mutable pkg_incorrect : package StringMap.t;
}

type version_stats = {
  ocaml_version : string;
  mutable total_npackages : int;
  mutable packages_correct : pkg_stats StringMap.t;
  mutable packages_partial : pkg_stats StringMap.t;
  mutable packages_unavailable : pkg_stats StringMap.t;
  mutable total_nversions : int;
  mutable total_nversions_correct : int;
  mutable total_nversions_incorrect : int;

  mutable total_packages : pkg_stats StringMap.t;

  mutable version_unsat_constraints : (to_install * string option) list ref StringMap.t;
}

