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



type relop = Eq | Neq | Geq | Gt | Leq | Lt
type logop =  And | Or
type pfxop = Not


(** Source file positions: filename, line, column *)
type pos = string * int * int

(** Base values *)
type value =
  | Bool of pos * bool
  | Int of pos * int
  | String of pos * string
  | Relop of pos * relop * value * value
  | Prefix_relop of pos * relop * value
  | Logop of pos * logop * value * value
  | Pfxop of pos * pfxop * value
  | Ident of pos * string
  | List of pos * value list
  | Group of pos * value list
  | Option of pos * value * value list
  | Env_binding of pos * string * value * value

(** A file section *)
type file_section = {
  section_kind  : string;
  section_name  : string;
  section_items : file_item list;
}

(** A file is composed of sections and variable definitions *)
and file_item =
  | Section of pos * file_section
  | Variable of pos * string * value

(** A file is a list of items and the filename *)
type file = {
  file_contents: file_item list;
  file_name    : string;
}

exception Lexer_error of string

    (*
          let item_pos items =
            let open CopamTypes in
            match items with
            | [] -> ("", max_int, 0)
            | item :: _ ->
              match item with
              | CopamTypes.Bool (pos, _) -> pos
              | Int (pos, _) -> pos
              | String (pos, _) -> pos
              | Relop (pos, _, _, _) -> pos
              | Prefix_relop (pos, _, _) -> pos
              | Logop (pos, _, _, _) -> pos
              | Pfxop (pos, _, _) -> pos
              | Ident (pos, _) -> pos
              | List (pos, _ ) -> pos
              | Group (pos, _ ) -> pos
              | Option (pos, _, _ ) -> pos
              | Env_binding (pos, _, _, _) -> pos
    *)
