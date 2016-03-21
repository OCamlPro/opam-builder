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




type version_stats = {
  s_version : CheckTypes.V.version;
  mutable s_used : int;      (* #occurrences to compile each version *)
  mutable s_used_last : int; (* #occurrences to compile each package *)
}

type stats = {
  stats_switch : CheckTypes.switch;

  stats_version : version_stats array;
  (* sorted by #occurrences to compile each version *)

  stats_version2 : version_stats array;
  (* sorted by #occurrences to compile each package *)

  stats_installable_versions : int;
  stats_installable_packages : int;
  stats_unavailable_packages : int;
  stats_unavailable_versions : int;
  stats_uninstallable_versions : int;
  stats_uninstallable_packages : int;
  stats_error_versions : int;
  stats_error_packages : int;
}

val analyze : CheckTypes.state -> CheckTypes.V.commit -> stats array
