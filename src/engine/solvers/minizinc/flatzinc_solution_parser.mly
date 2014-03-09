%{ (****************************************************************************)
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
    - engine/solvers/minizinc/Minizinc (for the map)
*)

%}

%token <int> INT
%token <string> NAME
%token EQ SEMICOLON
%token EOF
%token UNSATISFIABLE
%token HLINE
%start main                  /* the entry point */
%type <int Minizinc.Name_map.t option> main
%%
main:
  | UNSATISFIABLE { None }
  | variable_bindings hlines EOF  { Some($1) }
  | variable_bindings        EOF  { Some($1) }
  | variable_bindings hlines main {      $3  }
;

hlines:
  | HLINE hlines {}
  | HLINE        {}

minizinc_variable:
  | NAME { $1 }

value :
  | INT { $1 }

variable_binding:
  | minizinc_variable EQ value SEMICOLON { ($1, $3) }

variable_bindings:
  |                                    { Minizinc.Name_map.empty }
  | variable_binding variable_bindings { Minizinc.Name_map.add (fst $1) (snd $1) $2 }


