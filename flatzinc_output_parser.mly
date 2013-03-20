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

%token <int> INT
%token <string> NAME
%token EQ SEMICOLON
%token EOF 
%start main                  /* the entry point */
%type <(string * int) list> main
%%
main:
  variable_bindings EOF { $1 }
;

minizinc_variable :
  | NAME { $1 }

value :
  | INT { $1 }

variable_binding:
  | minizinc_variable EQ value SEMICOLON { ($1, $3) }

variable_bindings:
  | variable_binding* { $1 }
