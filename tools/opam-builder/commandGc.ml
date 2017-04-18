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

let arg_fake = ref false
let arg_ndays = ref 7

let args = [
    "--fake", Arg.Set arg_fake, " Fake a GC, do not remove files";
    "--ndays", Arg.Int (fun n -> arg_ndays := n),
    Printf.sprintf "NDAYS Remove only files older than NDAYS (default %d)"
                   !arg_ndays;

  ]

let action args =
  CheckTree.check_in_tree ();

  let switch = CheckTree.read_switch () in
  let fake = !arg_fake in
  let ndays_threshold = !arg_ndays in
  let cache_dir = CheckTree.cache_dir_basename in
  CheckGC.clean ~fake ~ndays_threshold ~cache_dir ~switch
