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


let arg_old_export = ref false

let args =
    CommandWatch.generic_args
  @ CommandExport.args
  @ CommandGc.args

let save_pid () =
  let pid = Unix.getpid () in
  let oc = open_out "builder.pid" in
  Printf.fprintf oc "%d\n" pid;
  close_out oc;
  at_exit (function () ->
                    try Sys.remove "builder.pid" with _ -> ()
          )
      
let action args =
  
  CheckTree.check_in_tree ();
  CommandScan.check_env ();
  
  let in_switch = CheckTree.read_switch () in
  begin
    match args with
    | [ switch ] ->
       if switch <> in_switch then begin
           CheckTree.fatal "specified switch differs from directory content"
         end
    | _ -> ()       
  end;

  save_pid ();
  CommandWatch.watch (fun commit ->
      Printf.eprintf "Watch: re-building...\n%!";
      if !arg_old_export then
        CommandExport.generate_export_file ()
      else
        ignore (CommandBuild.build_packages () : _);

      Printf.eprintf "Watch: GC...\n%!";
      CommandGc.do_gc ()
    )
