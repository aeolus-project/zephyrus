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
    - input/Settings (obviously)
    - datatypes/Data_common (for the map)
*)

let manage_element ident value = try Settings.add (Settings.setting_of_string ident) value with
  | Not_found   -> Settings.Settings_log.log_wrong_setting ident

%}

%token Equals Left_bracket Right_bracket Left_paren Right_paren Comma Semicolon EOF
%token<bool> Bool
%token<string> Ident
%token<int> Int


%start main
%type<unit> main


%%

main:
  | element main { () }
  | EOF          { () }

element:
  | Ident Equals value { manage_element $1 $3 }

value:
  | Bool                                      { Settings.BoolValue($1) }
  | Int                                       { Settings.IntValue($1) }
  | Ident                                     { Settings.IdentValue($1) }
  | Left_paren value Comma value Right_paren  { Settings.PairValue($2,$4) }
  | Left_bracket Right_bracket                { Settings.ListValue([]) }
  | Left_bracket list Right_bracket           { Settings.ListValue($2) }

list:
  | value                { [$1] }
  | value Semicolon list { $1 :: $3 }


