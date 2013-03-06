{
  open Flatzinc_output_parser        (* The type token is defined in flatzinc_output_parser.mli *)
  exception Eof  
}

rule token = parse

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

  (* Before the end *)
  | '=' ['=']+                            { token lexbuf }
  | '-' ['-']+                            { token lexbuf }

  (* End of file *)                       
  | eof                                   { EOF }