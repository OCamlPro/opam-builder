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


let action switches =
  List.iter (fun switch ->
      if Sys.file_exists switch then
        CheckTree.fatal "dir %S already exists" switch;
    ) switches;

  if not (Sys.file_exists "opam-repository/.git") then
    CheckTree.fatal "no git repository %S" "opam-repository/.git";

  Unix.putenv "OPAM_DOWNLOAD_CACHE"
              (Filename.concat CheckTree.current_dir "opam-download-cache");

  List.iter (fun switch ->
      if not (Printf.kprintf CheckBuild.command
                             "git clone opam-repository %s" switch)
      then
        CheckTree.fatal "cannot clone repository in %S\n%!" switch;

      CheckBuild.chdir switch;

      if not (Printf.kprintf CheckBuild.command
                             "git remote add ocaml %s"
                             (Filename.concat
                                CheckTree.current_dir
                                "opam-repository")
             )
      then
        CheckTree.fatal "cannot update remote of repository in %S" switch;


      CopamInstall.opam_command := "opam.dev";

      CheckBuild.upgrade_opam2 "2.0";

      let opam_root =
        Filename.concat
          (Filename.concat CheckTree.current_dir switch) ".opam" in
      let (_ : CopamInstall.t) = CopamInstall.init
                                   ~repo_subdir:"2.0"
                                   opam_root switch
      in

      CheckTree.write_switch switch;

      Unix.mkdir CheckTree.cache_dir_basename 0o755;
      Unix.mkdir CheckTree.reports_dir_basename 0o755;

      CheckBuild.chdir CheckTree.current_dir;

    ) switches
