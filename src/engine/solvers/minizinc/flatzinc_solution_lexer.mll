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

{
  open Flatzinc_solution_parser        (* The type token is defined in flatzinc_solution_parser.mli *)
  exception Eof  
}

rule token = parse

  (* No solution *)
  | "=====UNSATISFIABLE====="             { UNSATISFIABLE }

  (* Blanks *)
  | [' ' '\t']                            { token lexbuf }     (* skip blanks *)
  | ['\n']                                { token lexbuf }     (* skip newlines *)

  (* Variable names *)
  | ['a'-'z' 'A'-'Z'] ['0'-'9' 'a'-'z' 'A'-'Z' '_']+ as lxm { NAME(lxm) }

  (* Constants *)
  | '-'? ['0'-'9']+ as lxm                { INT(int_of_string lxm) }

  (* Other *)
  | '='                                   { EQ }
  | ';'                                   { SEMICOLON }

  (* Horizontal lines *)
  (* They are usually separating solutions and may appear after the last solution. *)
  | '=' ['=']+                            { HLINE }
  | '-' ['-']+                            { HLINE }

  (* End of file *)                       
  | eof                                   { EOF }
