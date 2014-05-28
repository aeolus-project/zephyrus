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
    - input/Specification_parser (obviously)
*)


{
  open Stateful_specification_parser (* The type token is defined in stateful_specification_parser.mli *)
  exception Eof
}

let blanks           = [' ' '\t' '\n']
let digits           = ['0'-'9']
let alpha_lower      = ['a'-'z']
let alpha_upper      = ['A'-'Z']
let alpha            = (alpha_lower | alpha_upper)
let lines            = ['-' '_']
let other_characters = ['+' ':']
let ident            = (alpha | digits | lines)

rule token = parse

  (* Blanks *)
  | [' ' '\t']         { token lexbuf }     (* skip blanks *)
  | ['\n' ]            { token lexbuf }     (* skip newlines *)

  (* Keywords *)
  | "everywhere"       { EVERYWHERE }
  | "at"               { AT }

  (* Constants *)
  | "true"                               
  | "True"                               
  | "TRUE"             { TRUE }

  | ['0'-'9']+ as lxm  { INT(int_of_string lxm) }

  (* Arithmetic operators *)
  | '+'                { PLUS }
  | '-'                { MINUS }
  | '*'                { TIMES }

  (* Logical operators *)
  | "and"                                
  | "&"                                  
  | "&&"               { AND }

  | "or"                                 
  | "|"                                  
  | "||"               { OR }

  | "->"                                 
  | "=>"                                 
  | "impl"                               
  | "implies"          { IMPL }

  | '!'                { NOT }

  (* Arithmetic comparisons *)
  | '<'                { LT  }
  | "<="               { LEQ } 
  | '='                { EQ  }
  | ">="               { GEQ } 
  | '>'                { GT  }
  | "!="               { NEQ } 

  (* Arity operator *)
  | '#'                { HASH }

  (* Other *)
  | ','                { COMMA }
  | ':'                { COLON }
  | ';'                { SEMICOLON }
  | '_'                { UNDERSCORE }

  (* Parentheses *)
  | '{'                { LCURLY }
  | '}'                { RCURLY }
  | '('                { LPAREN }
  | ')'                { RPAREN }

  (* Names *)
  
  (* Naming convention for component types: if first character is a capital letter, then it is a component type name. *)
  |      ((alpha_upper) (ident)* as lxm)                                                          { COMPONENT_TYPE_NAME(lxm) }
  | '"'  ((alpha_upper) (ident)* as lxm) '"'                                                      { COMPONENT_TYPE_NAME(lxm) }
  | '\'' ((alpha_upper) (ident)* as lxm) '\''                                                     { COMPONENT_TYPE_NAME(lxm) }
  
  (* Naming convention for ports: if first character is the '@' symbol, then it is a port name. *)
  |      (          '@' (ident)+ as lxm)                                                          { PORT_NAME(lxm)           }
  | '"'  (          '@' (ident)+ as lxm) '"'                                                      { PORT_NAME(lxm)           }
  | '\'' (          '@' (ident)+ as lxm) '\''                                                     { PORT_NAME(lxm)           }

  (* Naming convention for packages: if first character is a non-capital letter, then it is a package name. *)
  |      ((alpha_lower) (ident)* ('(' ['x' '='] ' ' (ident | other_characters)+ ')') as lxm)      { PACKAGE_NAME(lxm) }
  | '"'  ((alpha_lower) (ident)* ('(' ['x' '='] ' ' (ident | other_characters)+ ')') as lxm) '"'  { PACKAGE_NAME(lxm) }
  | '\'' ((alpha_lower) (ident)* ('(' ['x' '='] ' ' (ident | other_characters)+ ')') as lxm) '\'' { PACKAGE_NAME(lxm) }

  (* Other names *)
  |      (      (alpha) (ident)* as lxm)                                                          { NAME(lxm) }
  | '"'  (      (alpha) (ident)* as lxm) '"'                                                      { NAME(lxm) }
  | '\'' (      (alpha) (ident)* as lxm) '\''                                                     { NAME(lxm) }


  (* End of file *)
  | eof { EOF }
