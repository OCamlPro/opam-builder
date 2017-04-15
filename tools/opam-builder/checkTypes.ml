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



open StringCompat

module V1 = struct

type status = {
  s_log : string option;
  s_status : CopamInstall.status;
}

type version = {
  version_package : package;
  version_name : string;
  (* The checksum of the content of the directory NAME.VERSION/ *)
  version_checksum : CheckDigest.t;
  mutable version_visited : int;
  mutable version_deps : package StringMap.t;
  mutable version_status : status array;
  mutable version_lint : version_lint option;
}

and package = {
  package_name : string;
  mutable package_local_checksum : CheckDigest.t option;
  mutable package_transitive_checksum :
    (CheckDigest.t * package StringMap.t) option;
  mutable package_visited : int;
  mutable package_versions : version StringMap.t;
  mutable package_deps : package StringMap.t;
  mutable package_status : status array;
}

and version_lint = {
  lint_version : version;
  lint_warnings: (int * string) list;
  lint_errors: (int * string) list;
}

type commit = {
  check_date : string;
  commit_name : string;
  switches : string array;

  (* map from NAME.VERSION to version *)
  mutable versions : version StringMap.t;

  (* map from NAME to package *)
  mutable packages : package StringMap.t;
}

end

module V = V1

type cudf = {
    mutable known_universe : Cudf.universe option;
    solver_cache : (string, Algo.Diagnostic.reason list) Hashtbl.t;
    cudf_backup : CopamCudf.t option ref;
  }

type switch = {
  sw_name : string;
  sw_dir : string;
  sw_snapshot : CheckSnapshot.t;
  sw_backup : MemoryBackup.t;
  sw_cudf : cudf;
}

type directories = {
  opam_dir : string;
  cache_dir : string;
  repo_dir : string;
  repo_subdir : string;
  report_dir : string;
  current_dir : string;
}

type state = {
  dirs : directories;
  sws : switch array;
  root : CopamInstall.t;
}
