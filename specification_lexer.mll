{
  open Specification_parser        (* The type token is defined in specification_parser.mli *)
  exception Eof

  open Aeolus_types_t
  open Typing_context

  type element = CT | Pkg | Prt

  let names_table = Hashtbl.create 50000

  (* We only need to keep track of names of component types, ports and packages,
     because when one of these three appears in the specification we have no way
     to guess from the parsing context if it is a name of a component type,
     a port or a package. *)
  let initialize_names_table (universe : universe) =
    List.iter (fun component_type_name -> Hashtbl.add names_table component_type_name CT  ) (get_component_type_names universe);
    List.iter (fun port_name           -> Hashtbl.add names_table port_name           Prt )           (get_port_names universe);
    List.iter (fun package_name        -> Hashtbl.add names_table package_name        Pkg )        (get_package_names universe);
    ()

  
  let name_token lxm =
    try
      (* Does a name used in the specification corresponds to a name
         of a component type, a port or a package present in the universe? *)
      match Hashtbl.find names_table lxm with
      | CT  -> COMPONENT_TYPE_NAME(lxm)
      | Prt -> PORT_NAME(lxm)
      | Pkg -> PACKAGE_NAME(lxm)
      
    with Not_found ->
      (* If not, then it must be a name of a resource or a repository or 
         a specification variable. *)
      NAME(lxm)
  
}

rule token = parse

  (* Blanks *)
  | [' ' '\t']                           { token lexbuf }     (* skip blanks *)
  | ['\n' ]                              { token lexbuf }     (* skip newlines *)

  (* Constants *)
  | "true"                               
  | "True"                               
  | "TRUE"                               { TRUE }

  | ['0'-'9']+ as lxm                    { INT(int_of_string lxm) }

  (* Arithmetic operators *)
  | '+'                                  { PLUS }
  | '-'                                  { MINUS }
  | '*'                                  { TIMES }

  (* Logical operators *)
  | "and"                                
  | "&"                                  
  | "&&"                                 { AND }

  | "or"                                 
  | "|"                                  
  | "||"                                 { OR }

  | "->"                                 
  | "=>"                                 
  | "impl"                               
  | "implies"                            { IMPL }

  | '!'                                  { NOT }

  (* Arithmetic comparisons *)
  | '<'                                  { LT  }
  | "<="                                 { LEQ } 
  | '='                                  { EQ  }
  | ">="                                 { GEQ } 
  | '>'                                  { GT  }
  | "!="                                 { NEQ } 

  (* Arity operator *)
  | '#'                                  { HASH }

  (* Other *)
  | ':'                                  { COLON }
  | ';'                                  { SEMICOLON }
  | '_'                                  { UNDERSCORE }

  (* Parentheses *)
  | '{'                                  { LCURLY }
  | '}'                                  { RCURLY }
  | '('                                  { LPAREN }
  | ')'                                  { RPAREN }

  (* Names *)
  
  |      (['A'-'Z'] ['a'-'z' 'A'-'Z' '-' '_']+ as lxm)      { COMPONENT_TYPE_NAME(lxm) }
  | '"'  (['A'-'Z'] ['a'-'z' 'A'-'Z' '-' '_']+ as lxm) '"'  { COMPONENT_TYPE_NAME(lxm) }
  | '\'' (['A'-'Z'] ['a'-'z' 'A'-'Z' '-' '_']+ as lxm) '\'' { COMPONENT_TYPE_NAME(lxm) }
  
  |      ('@'       ['a'-'z' 'A'-'Z' '-' '_']+ as lxm)      { PORT_NAME(lxm)           }
  | '"'  ('@'       ['a'-'z' 'A'-'Z' '-' '_']+ as lxm) '"'  { PORT_NAME(lxm)           }
  | '\'' ('@'       ['a'-'z' 'A'-'Z' '-' '_']+ as lxm) '\'' { PORT_NAME(lxm)           }

  |      (['a'-'z'] ['a'-'z' 'A'-'Z' '-' '_']* ('(' ['x' '='] ' ' ['0'-'9' 'a'-'z' 'A'-'Z' '-' '_' '+' ':']+ ')') as lxm)      { PACKAGE_NAME(lxm) }
  | '"'  (['a'-'z'] ['a'-'z' 'A'-'Z' '-' '_']* ('(' ['x' '='] ' ' ['0'-'9' 'a'-'z' 'A'-'Z' '-' '_' '+' ':']+ ')') as lxm) '"'  { PACKAGE_NAME(lxm) }
  | '\'' (['a'-'z'] ['a'-'z' 'A'-'Z' '-' '_']* ('(' ['x' '='] ' ' ['0'-'9' 'a'-'z' 'A'-'Z' '-' '_' '+' ':']+ ')') as lxm) '\'' { PACKAGE_NAME(lxm) }

  |      (['a'-'z' 'A'-'Z' '-' '_']+ as lxm)      { NAME(lxm) }
  | '"'  (['a'-'z' 'A'-'Z' '-' '_']+ as lxm) '"'  { NAME(lxm) }
  | '\'' (['a'-'z' 'A'-'Z' '-' '_']+ as lxm) '\'' { NAME(lxm) }


  (* End of file *)
  | eof                                  { EOF }