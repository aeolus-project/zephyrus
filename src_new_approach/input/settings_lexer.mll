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
    - input/Settings_parser (obviously)
    - datatypes/Data_common (for the map)
*)

{
  open Settings_parser
  exception Eof

  let keywords = [
    ("input_file_universe", Input_file_universe);
    ("input_file_repositories", Input_file_repositories);
    ("input_file_initial_configuration", Input_file_initial_configuration);
    ("input_file_specification", Input_file_specification);
    ("input_optimization_function", Input_optimization_function);

    ("verbose_warning_setting_not_set", Verbose_warning_setting_not_set);


    ("true", Bool(true)); ("yes", Bool(true)); ("y", Bool(true));
    ("false", Bool(false)); ("no", Bool(false)); ("n", Bool(false));
  ]

  let keyword_map = Data_common.MapString.map_of_associated_list keywords

  let check_string id = try Data_common.MapString.find (String.lowercase id) keyword_map with
    | Not_found -> Ident(id)
}

let blanks = [' ' '\t' '\n']
let digits = ['0'-'9']
let alpha  = ['a'-'z' 'A'-'Z']
let lines  = ['-' '_']

rule token = parse
  | blanks                           { token lexbuf        }     (* skip blanks and new lines *)
  | '"'                              { token_string lexbuf } (* enter string mode *)
  | ((alpha digits lines)+ as lxm) { check_string lxm    }
  | "="                              { Equals              }
  | '['                              { Left_bracket        }
  | ']'                              { Right_bracket       }
  | '('                              { Left_paren          }
  | ')'                              { Right_paren         }
  | ','                              { Comma               }
  | ';'                              { Semicolon           }
  | eof                              { EOF                 }

and token_string = parse
  | ((blanks digits alpha lines '(' ')')+ as lxm) { check_string lxm }
  | '"'                                           { token lexbuf     }



