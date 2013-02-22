{
  open Specification_parser        (* The type token is defined in specification_parser.mli *)
  exception Eof

  open Aeolus_types_t
  open Typing_context

  let names_table = Hashtbl.create 53

  (* We only need to keep track of names of component types, ports and packages,
     because when one of these three appears in the specification we have no way
     to guess from the parsing context if it is a name of a component type,
     a port or a package. *)
  let initialize_names_table (universe : universe) =
    List.iter (fun component_type_name -> Hashtbl.add names_table component_type_name (fun lxm -> COMPONENT_TYPE_NAME(lxm))) (get_component_type_names universe);
    List.iter (fun port_name           -> Hashtbl.add names_table port_name           (fun lxm -> PORT_NAME(lxm)))           (get_port_names universe);
    List.iter (fun package_name        -> Hashtbl.add names_table package_name        (fun lxm -> PACKAGE_NAME(lxm)))        (get_package_names universe);
    ()

  let name_token lxm =
    try
      (* Does a name used in the specification corresponds to a name
         of a component type, a port or a package present in the universe? *)
      (Hashtbl.find names_table lxm) lxm
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
  | '"'  (['a'-'z' '-']+ as lxm) '"'     { name_token lxm }
  | '\'' (['a'-'z' '-']+ as lxm) '\''    { name_token lxm }
  |      (['a'-'z' '-']+ as lxm)         { name_token lxm }

  (* End of file *)
  | eof                                  { EOF }