
{
  open Specification_parser        (* The type token is defined in specification_parser.mli *)
  exception Eof
}

rule token = parse
    [' ' '\t']        { token lexbuf }     (* skip blanks *)
  | ['\n' ]           { token lexbuf }
  | ['0'-'9']+              as lxm { INT(int_of_string lxm) }
  | "v$" (['A'-'Z' 'a'-'z' '-']+ as lxm) { VAR_NAME(lxm) }
  | "t$" (['A'-'Z' 'a'-'z' '-']+ as lxm) { COMPONENT_TYPE_NAME(lxm) }
  | "p$" (['A'-'Z' 'a'-'z' '-']+ as lxm) { PORT_NAME(lxm) }
  | "k$" (['A'-'Z' 'a'-'z' '-']+ as lxm) { PACKAGE_NAME(lxm) }
  | "r$" (['A'-'Z' 'a'-'z' '-']+ as lxm) { REPOSITORY_NAME(lxm) }
  | "o$" (['A'-'Z' 'a'-'z' '-']+ as lxm) { RESOURCE_NAME(lxm) }
  | "true"            { TRUE }
  | '+'               { PLUS }
  | '-'               { MINUS }
  | '*'               { TIMES }
  | "and"             { AND }
  | "or"              { OR }
  | "=>"              { IMPL }
  | '!'               { NOT }
  | '<'               { LT  }
  | "<="              { LEQ } 
  | '='               { EQ  }
  | ">="              { GEQ } 
  | '>'               { GT  }
  | "!="              { NEQ } 
  | '#'               { HASH }
  | ':'               { COLON }
  | ';'               { SEMICOLON }
  | '_'               { UNDERSCORE }
  | '{'               { LCURLY }
  | '}'               { RCURLY }
  | '('               { LPAREN }
  | ')'               { RPAREN }
  | eof               { EOF }
(*  | eof               { raise Eof } *)