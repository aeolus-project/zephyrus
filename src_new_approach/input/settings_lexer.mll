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
    ("true",  Bool(true));
    ("yes",   Bool(true));
    ("y",     Bool(true));
    ("false", Bool(false));
    ("no",    Bool(false)); 
    ("n",     Bool(false));
  ]

  let keyword_map = Data_common.MapString.map_of_associated_list keywords

  let process_string s = 
    try 
      Data_common.MapString.find (String.lowercase s) keyword_map
    with
    | Not_found -> Ident(s)
}

let blanks          = [' ' '\t' '\n']
let digits          = ['0'-'9']
let alpha           = ['a'-'z' 'A'-'Z']
let lines           = ['-' '+' '_' '.' '/']
let other_caracters = ['[' '{' '(' ';' ':' ',' '\'' ')' '}' ']']

(* Strings which are not quoted *)
let ident  = (alpha | digits | lines)* (alpha | lines)+ (alpha | digits | lines)*

(* Strings which are quoted *)
let string = (blanks | digits | alpha | lines | other_caracters)*

(* Integers *)
let int = (digits)+

rule token = parse
  | blanks                       { token lexbuf           }  (* skip blanks and new lines *)
  | '#'                          { token_comment lexbuf   }  (* enter comment mode *)
  | ('"'  (string as lxm) '"' )  { Ident(lxm)             }
  | ('\'' (string as lxm) '\'')  { Ident(lxm)             }
  | (int   as lxm)               { Int(int_of_string lxm) }
  | (ident as lxm)               { process_string lxm     }
  | "="                          { Equals                 }
  | '['                          { Left_bracket           }
  | ']'                          { Right_bracket          }
  | '('                          { Left_paren             }
  | ')'                          { Right_paren            }
  | ','                          { Comma                  }
  | ';'                          { Semicolon              }
  | eof                          { EOF                    }

and token_comment = parse
  | '\n'                         { token lexbuf         }
  | _                            { token_comment lexbuf } 

