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



open Jsonm
(* String conversion *)
exception Escape of ((int * int) * (int * int)) * error
type t =
  Null | Bool of bool | Float of float| String of string
  | A of t list | O of (string * t) list

let json_of_src ?encoding src =
  let dec d = match decode d with
    | `Lexeme l -> l
    | `Error e -> raise (Escape (decoded_range d, e))
    | `End | `Await -> assert false
  in
  let rec value v k d = match v with
    | `Os -> obj [] k d
    | `As -> arr [] k d
    | `Null -> k Null d
    | `Bool b -> k (Bool b) d
    | `String s -> k (String s) d
    | `Float f -> k (Float f) d
    | _ -> assert false
  and arr vs k d = match dec d with
    | `Ae -> k (A (List.rev vs)) d
    | v -> value v (fun v -> arr (v :: vs) k) d
  and obj ms k d = match dec d with
    | `Oe -> k (O (List.rev ms)) d
    | `Name n -> value (dec d) (fun v -> obj ((n, v) :: ms) k) d
    | _ -> assert false
  in
  let d = decoder ?encoding src in
  try `JSON (value (dec d) (fun v _ -> v) d) with
  | Escape (r, e) -> `Error (r, e)

let of_string str: t =
  match json_of_src (`String str) with
  | `JSON j  -> j
  | `Error _ -> failwith "json_of_string"

let rec print indent t =
  Printf.printf "%s" indent;
  begin
  match t with
    Null -> Printf.printf "Null"
  | Bool bool -> Printf.printf "Bool %b" bool
  | Float float -> Printf.printf "Float %f" float
  | String string -> Printf.printf "String %S" string
  | A  list ->
    Printf.printf "A [\n";
    List.iter (print (indent ^ "  ")) list;
    Printf.printf "%s  ]" indent;
  | O list ->
    Printf.printf "O [\n";
    let indent4 = indent ^ "    " in
    List.iter (fun (s, t) ->
      Printf.printf "%s  %S =\n" indent s;
      print indent4 t;
    ) list;
    Printf.printf "%s  ]" indent;
  end;
  Printf.printf "\n"

let print t = print "  " t
