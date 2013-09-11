(****************************************************************************)
(*                                                                          *)
(*    This file is part of Zephyrus.                                        *)
(*                                                                          *)
(*    Zephyrus is free software: you can redistribute it and/or modify      *)
(*    it under the terms of the GNU General Public License as published by  *)
(*    the Free Software Foundation, either version 3 of the License, or     *)
(*    (at your option) any later version.                                   *)
(*                                                                          *)
(*    Zephyrus is distributed in the hope that it will be useful,           *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*    GNU General Public License for more details.                          *)
(*                                                                          *)
(*    You should have received a copy of the GNU General Public License     *)
(*    along with Zephyrus.  If not, see <http://www.gnu.org/licenses/>.     *)
(*                                                                          *)
(****************************************************************************)


(* Depends on
  MUST DEPEND ON NOTHING !!
    - Str (ocaml standard library, for pattern matching)
    - Printf
*)

(* 1. For indentation *) 

let a_capo = ref true
let indent_stage = ref ""
let new_stage () = indent_stage := "  " ^ (!indent_stage)
let end_stage () = indent_stage := String.sub (!indent_stage) 0 ((String.length (!indent_stage)) -2)

let get_current_indent () = !indent_stage

(* 2. for printing *)
let rec indent_string_rec s =
  let n = (String.length s) - 1 in
  let i = try String.index s '\n' with Not_found -> -1 in (* Printf.printf "%i => %b\n" i (n <= i); *)
  if i = -1 then (a_capo := false; s)
  else (
    if n <= i then (a_capo := true; s)
    else ((String.sub s 0 (i+1)) ^ (!indent_stage) ^ (indent_string_rec (String.sub s (i+1) (n - i))))
  )

let indent_string s = (* Printf.printf "%b\n" (!a_capo); *) if !a_capo then (!indent_stage) ^ (indent_string_rec s) else (indent_string_rec s)


let print file s = Pervasives.output_string file (indent_string s); flush file
let println file s = print file (s ^ "\n")

(* 3. print output *)

let print_output filename s = let file = Pervasives.open_out filename in
  Pervasives.output_string file s;
  Pervasives.close_out file

(* Because there is no general Helper module... *) (* TODO: should go in datatypes/Data_common.List *)
let filter_map (f : 'a -> 'b option) (l : 'a list) : ('b list) = 
  List.fold_right (fun (el : 'a) (l : 'b list) -> 
    match f el with
    | None   -> l 
    | Some x -> x :: l
  ) l []
