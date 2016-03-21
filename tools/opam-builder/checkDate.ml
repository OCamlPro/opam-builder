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



open Unix

module type Date_sig = sig
  val current : unit -> string
  val of_float : float -> string
  val of_tm : Unix.tm -> string
end

module Make(X: sig val of_tm : Unix.tm -> string end) = struct
  let of_tm = X.of_tm
  let of_float t = of_tm (Unix.gmtime t)
  let current () = of_float (Unix.time())
  end

module ISO8601 = Make(struct
  let of_tm tm =
    Printf.sprintf
      "%04d%02d%02dT%02d:%02d:%02dZ"
      (1900 + tm.tm_year)
      (1 + tm.tm_mon)
      tm.tm_mday
      tm.tm_hour
      tm.tm_min
      tm.tm_sec
  end)

module TIMESTAMP = Make(struct
    let of_tm tm =
      Printf.sprintf
        "%04d%02d%02d%02d%02d%02d"
        (1900 + tm.tm_year)
        (1 + tm.tm_mon)
        tm.tm_mday
        tm.tm_hour
        tm.tm_min
        tm.tm_sec
  end)
