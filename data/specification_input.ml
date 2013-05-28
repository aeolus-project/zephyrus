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

open Aeolus_types_t

module type SPECIFICATION_INPUT =
  sig
    val specification_of_string : string -> specification
  end

(* The specification is written in JSON, ATD can parse it directly. *)
module JSON : SPECIFICATION_INPUT = Aeolus_types_j

(* The specification is written in Aeolus specifiaction syntax, we have to parse it. *)
module Aeolus_specification_language : SPECIFICATION_INPUT =
  struct

    let specification_of_string specification_string =
      let lexbuf = Lexing.from_string specification_string 
      in
      let specification = Specification_parser.main Specification_lexer.token lexbuf
      in
      specification

  end