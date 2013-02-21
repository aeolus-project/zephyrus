{
  open Specification_parser        (* The type token is defined in specification_parser.mli *)
  exception Eof

  open Aeolus_types_t
  open Typing_context

  let names_table = Hashtbl.create 53

  let initialize_names_table (universe : universe) =
    List.iter (fun component_type_name -> Hashtbl.add names_table component_type_name (fun lxm -> COMPONENT_TYPE_NAME(lxm))) (get_component_type_names universe);
    List.iter (fun port_name           -> Hashtbl.add names_table port_name           (fun lxm -> PORT_NAME(lxm)))           (get_port_names universe);
    List.iter (fun package_name        -> Hashtbl.add names_table package_name        (fun lxm -> PACKAGE_NAME(lxm)))        (get_package_names universe);
    List.iter (fun repository_name     -> Hashtbl.add names_table repository_name     (fun lxm -> REPOSITORY_NAME(lxm)))     (get_repository_names universe);
    List.iter (fun resource_name       -> Hashtbl.add names_table resource_name       (fun lxm -> RESOURCE_NAME(lxm)))       (get_resource_names universe);
    ()

  let name_token lxm =
    try
      (* Does a name used in the specification corresponds to a name present in the universe? *)
      (Hashtbl.find names_table lxm) lxm
    with Not_found ->
      (* If not, then it must be a specification variable name. *)
      VAR_NAME(lxm)

}

rule token = parse
    [' ' '\t']                           { token lexbuf }     (* skip blanks *)
  | ['\n' ]                              { token lexbuf }     (* skip newlines *)
  | ['0'-'9']+ as lxm                    { INT(int_of_string lxm) }
  | "true"                               { TRUE }
  | "True"                               { TRUE }
  | "TRUE"                               { TRUE }
  | '+'                                  { PLUS }
  | '-'                                  { MINUS }
  | '*'                                  { TIMES }
  | "and"                                { AND }
  | "&"                                  { AND }
  | "&&"                                 { AND }
  | "or"                                 { OR }
  | "|"                                  { OR }
  | "||"                                 { OR }
  | "->"                                 { IMPL }
  | "=>"                                 { IMPL }
  | "impl"                               { IMPL }
  | "implies"                            { IMPL }
  | '!'                                  { NOT }
  | '<'                                  { LT  }
  | "<="                                 { LEQ } 
  | '='                                  { EQ  }
  | ">="                                 { GEQ } 
  | '>'                                  { GT  }
  | "!="                                 { NEQ } 
  | '#'                                  { HASH }
  | ':'                                  { COLON }
  | ';'                                  { SEMICOLON }
  | '_'                                  { UNDERSCORE }
  | '{'                                  { LCURLY }
  | '}'                                  { RCURLY }
  | '('                                  { LPAREN }
  | ')'                                  { RPAREN }
  | '"'  (['a'-'z' '-']+ as lxm) '"'     { name_token lxm }
  | '\'' (['a'-'z' '-']+ as lxm) '\''    { name_token lxm }
  |      (['a'-'z' '-']+ as lxm)         { name_token lxm }
  | eof                                  { EOF }