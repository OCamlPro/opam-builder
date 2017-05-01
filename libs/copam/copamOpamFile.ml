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
open CopamTypes

let string_of_relop = function
  | `Eq -> "Eq"
  | `Neq -> "Neq"
  | `Geq -> "Geq"
  | `Gt -> "Gt"
  | `Leq -> "Leq"
  | `Lt -> "Lt"

let string_of_logop = function
  | `And -> "And"
  | `Or -> "Or"

let string_of_pfxop = function `Not -> "Not"

let print_env_update_op indent v =
  Printf.printf "%s%s\n" indent
                (match v with
                   Eq -> "Eq"
                 | PlusEq -> "PlusEq"
                 | EqPlus -> "EqPlus"
                 | EqPlusEq -> "EqPlusEq"
                 | ColonEq -> "ColonEq"
                 | EqColon -> "EqColon")

let rec print_value indent v =
  Printf.printf "%s" indent;
  begin
  match v with
  | Bool (pos, bool) -> Printf.printf "Bool %b" bool
  | Int (pos, int) -> Printf.printf "Int %d" int
  | String (pos, string) -> Printf.printf "String %S"  string
  | Relop (pos, relop, v1, v2) ->
    Printf.printf "Relop (%s,\n" (string_of_relop relop);
    print_value (indent ^ "  ") v1;
    print_value (indent ^ "  ") v2;
    Printf.printf "%s  )" indent
  | Prefix_relop (pos, relop, value) ->
    Printf.printf "Prefix_relop (%s,\n" (string_of_relop relop);
    print_value (indent ^ "  ") value;
    Printf.printf "%s  )" indent
  | Logop (pos, relop, v1, v2) ->
    Printf.printf "Logop (%s,\n" (string_of_logop relop);
    print_value (indent ^ "  ") v1;
    print_value (indent ^ "  ") v2;
    Printf.printf "%s  )" indent
  | Pfxop (pos, pfxop, value) ->
    Printf.printf "Pfxop (%s,\n" (string_of_pfxop pfxop);
    print_value (indent ^ "  ") value;
    Printf.printf "%s  )" indent
  | Ident (pos, string) -> Printf.printf "Ident %s" string
  | List (pos, list) ->
    Printf.printf "List [\n";
    List.iter (print_value (indent ^ "   ")) list;
    Printf.printf "%s]" indent
  | Group (pos, list) ->
    Printf.printf "Group [\n";
    List.iter (print_value (indent ^ "   ")) list;
    Printf.printf "%s]" indent

  | Option (pos, value, list) ->
    Printf.printf "Option (\n";
    print_value (indent ^ "  ") value;
    Printf.printf "%s  [\n" indent;
    List.iter (print_value (indent ^ "     ")) list;
    Printf.printf "%s  ])" indent
  | Env_binding (pos, v1, env_update_op, v2) ->
    Printf.printf "Env_binding (\n";
    print_value (indent ^ "  ") v1;
    print_env_update_op (indent ^ "  ") env_update_op;
    print_value (indent ^ "  ") v2;
    Printf.printf "%s  )" indent;
  end;
  Printf.printf "\n"

and print_file_item indent = function
  | Section (_,section) ->
    Printf.printf "%sSection (\n" indent;
    print_file_section (indent ^ "  ") section;
    Printf.printf "%s)\n" indent
  | Variable (_,s, value) ->
    Printf.printf "%sVariable (%s,\n" indent s;
    print_value (indent ^ "  ") value;
    Printf.printf "%s)\n" indent

and print_file_section indent  {
  section_kind ;
  section_name ;
  section_items;
} =
  Printf.printf "%s{\n" indent;
  Printf.printf "%s   section_kind = %s;\n" indent section_kind;
  Printf.printf "%s   section_name = %S;\n" indent
                (match section_name with None -> "" | Some s -> s);
  Printf.printf "%s   section_items = [\n" indent;
  List.iter (print_file_item (indent ^ "    ")) section_items;
  Printf.printf "%s        ];\n" indent;
  Printf.printf "%s}\n" indent

let print_file indent {
  file_contents;
  file_name    ;
} =
  Printf.printf "%s{\n" indent;
  Printf.printf "%s   file_name = %s;\n" indent file_name;
  Printf.printf "%s   file_contents = [\n" indent;
  List.iter (print_file_item (indent ^ "    ")) file_contents;
  Printf.printf "%s        ];\n" indent;
  Printf.printf "%s}\n" indent

let print file =
  print_file "  " file

let parse opam_file =
  let ic = open_in opam_file in
  try
    let lexbuf = Lexing.from_channel ic in
    let v = OpamParser.main OpamLexer.token lexbuf opam_file in
    close_in ic;
    v
  with e ->
    close_in ic;
    raise e

let rec iter_value_dep alldeps v =
  match v with
  | String (_, s)
  | Option (_, String (_,s),_ ) ->
    alldeps := StringSet.add s !alldeps
  | Option _ -> assert false
  | Relop _ -> assert false
  | Logop (_, op, v1,v2) ->
    iter_value_dep alldeps v1;
    iter_value_dep alldeps v2
  | Pfxop _ -> assert false
  | Ident _ -> assert false
  | Int _ -> assert false
  | Env_binding _ -> assert false
  | Group (_, v) -> List.iter (iter_value_dep alldeps) v
  | List (_,list) ->
    List.iter (iter_value_dep alldeps) list
  | Prefix_relop _ -> assert false
  | Bool _ -> assert false

let all_possible_deps file =
  try
    let alldeps = ref StringSet.empty in
    List.iter (fun item ->
      match item with
      | Variable (_, "depends", List (_,deps)) ->
        List.iter (iter_value_dep alldeps) deps
      | _ -> ()
    ) file.file_contents;
    !alldeps
  with exn ->
    Printf.printf "Error in all_possible_deps %s: %s\n%!"
      file.file_name (Printexc.to_string exn);
    print file;
    Printf.printf "%!";
    raise exn
