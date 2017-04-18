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

module Subcommand : sig

  type help = string list

  val add : string ->
            (string * Arg.spec * string) list ->
            bool ->
            string ->
            help ->
            (string list -> unit) ->
            unit

  val parse : help -> unit

  end = struct

  type help = string list

  type subcommand = {
      sub_command : string;
      sub_args : (string * Arg.spec * string) list;
      sub_help : help;
      sub_usage : string;
      sub_anon : bool;
      sub_action : (string list -> unit);
    }

  let subcommands = ref []
  let add sub_command sub_args sub_anon sub_usage sub_help sub_action =
    subcommands := {
        sub_command;
        sub_args;
        sub_help;
        sub_usage;
        sub_anon;
        sub_action;
      } :: !subcommands

  let rec find_subcommand list command =
    match list with
      [] -> raise Not_found
    | s :: list ->
       if s.sub_command = command then s else
         find_subcommand list command

  let parse help =
    let arg_anons = ref [] in
    let arg_spec = ref [] in
    let subcommands = List.rev !subcommands in
    let arg_usage =
      String.concat "\n" (
                      help @
                        [
                          "";
                          "where SUBCOMMAND is one of:";
                        ]
                        @
                          List.map (fun s ->
                              Printf.sprintf "   %s: %s"
                                             s.sub_command s.sub_usage
                            ) subcommands @
                          [
                            "";
                            "and arguments are:";
                    ])
    in
    let subcommand = ref None in
    let arg_anon arg =
      match !subcommand with
      | Some s ->
         if s.sub_anon then arg_anons := arg :: !arg_anons
         else
           raise (Arg.Bad "no anonymous argument")
      | None ->
         try
           let s = find_subcommand subcommands arg in
           subcommand := Some s;
           let print_help () =
             Arg.usage !arg_spec
                       (String.concat "\n"
                                      (
                                        s.sub_help @
                                          ["";
                                           "and arguments are:";
                                          ]
                                      )
                       );
             exit 0
           in
           arg_spec := Arg.align
             (s.sub_args  @
                [
                  "-help", Arg.Unit print_help,
                  " Display the options of this subcommand";
                  "--help", Arg.Unit print_help,
                  " Display the options of this subcommand";
             ])
         with Not_found ->
           Arg.usage !arg_spec arg_usage;
           exit 2
    in
    Arg.parse_dynamic arg_spec arg_anon arg_usage;
    match !subcommand with
    | None ->
        Arg.usage !arg_spec arg_usage;
        exit 2
    | Some s ->
       s.sub_action (List.rev !arg_anons)

end

let () =
  Printexc.record_backtrace true;
  Subcommand.add "watch"
                 CommandWatch.args
                 false
                 "Watch an opam-repository"
                 [ "opam-builder watch [arguments]";
                   "";
                   "  Watch the opam-repository in which it is called.";
                 ]
                 CommandWatch.action;

  Subcommand.add "create" []
                 true
                 "Create the specified switch"
                 [ "opam-builder create SWITCHNAME [arguments]";
                   "";
                   "  Create the proposed switch SWITCHNAME.";
                 ]
                 CommandCreate.action;

  Subcommand.add "file" []
                 true
                 "Read files"
                 [ "opam-builder file [FILES] [arguments]";
                   "";
                   "  Try to read files FILES.";
                 ]
                 CommandFile.action;

  Subcommand.add "scan" CommandScan.args
                 false
                 "Scan packages"
                 [ "opam-builder scan [arguments]";
                   "";
                   "  Scan packages and versions.";
                 ]
                 CommandScan.action;

  Subcommand.add "weather" CommandWeather.args
                 false
                 "Compute dependencies on the current switch"
                 [ "opam-builder weather [arguments]";
                   "";
                   "  Compute dependencies on the switches.";
                 ]
                 CommandWeather.action;

  Subcommand.add "build" CommandBuild.args
                 false
                 "Try to build the updated packages"
                 [ "opam-builder build [arguments]";
                   "";
                   "  Try to build the updated packages.";
                 ]
                 CommandBuild.action;

  Subcommand.add "export" CommandExport.args
                 false
                 "Generates the export file"
                 [ "opam-builder export [arguments]";
                   "";
                   "  Generates the export file";
                 ]
                 CommandExport.action;

  Subcommand.add "gc" CommandGc.args
                 false
                 "Perform a GC on the switch"
                 [ "opam-builder gc [arguments]";
                   "";
                   "  Perform a GC on the current switch.";
                 ]
                 CommandGc.action;

  Subcommand.add "import" CommandImport.args
                 true
                 "Aggregate reports from the specified switches"
                 [ "opam-builder import REPORTDIRS [arguments]";
                   "";
                   "  Aggregate reports from the specified switches.";
                 ]
                 CommandImport.action;

  Subcommand.parse [ "opam-builder SUBCOMMAND [arguments]" ]
