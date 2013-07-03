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

let indent_stage = ref ""
let extend_indent_stage () = indent_stage := "  " ^ (!indent_stage)
let shorten_indent_stage () = indent_stage := String.sub (!indent_stage) 0 ((String.length (!indent_stage)) -2)

let get_current_indent () = !indent_stage


(* 2. for printing *)

let new_line_regexp = Str.regexp "\n"
let print file s = Printf.fprintf file "%s%s\n" (!indent_stage) (Str.global_replace new_line_regexp ("\n" ^ (!indent_stage)) s)
let print_capo file s = Printf.fprintf file "%s\n" s



(* Because there is no general Helper module... *)
let filter_map (f : 'a -> 'b option) (l : 'a list) : ('b list) = 
  List.fold_right (fun (el : 'a) (l : 'b list) -> 
    match f el with
    | None   -> l 
    | Some x -> x :: l
  ) l []