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



open Genlex

let lexer = Genlex.make_lexer [ ":" ]

type error =
  IdentExpected
| ColonExpected
| StringExpected

exception SyntaxError of error * string * int

let load filename =
  let ic = open_in filename in
  let lexbuf = Stream.of_channel ic in
  let lexer = lexer lexbuf in
  let idents = ref [] in

  let rec iter0 () =
    match Stream.next lexer with
    | Ident s -> iter1 (String.lowercase s)
    | Kwd _
    | Float _
    | String _
    | Char _
    | Int _
      -> raise (SyntaxError (IdentExpected, filename, Stream.count lexbuf))

  and iter1 s =
    match Stream.next lexer with
    | Kwd ":" -> iter2 s
    | _ -> raise (SyntaxError (ColonExpected, filename, Stream.count lexbuf))
  and iter2 s =
    match Stream.next lexer with
    | String v ->
      idents := (s, v) :: !idents;
      iter0 ()
    | _ -> raise (SyntaxError (StringExpected, filename, Stream.count lexbuf))

  in
  try
    iter0 ()
  with
  | Stream.Failure -> (* EOF *)
    close_in ic;
    !idents
  | e ->
    close_in ic;
    raise e
