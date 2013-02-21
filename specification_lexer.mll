
{
  open Specification_parser        (* The type token is defined in specification_parser.mli *)
  exception Eof

  open Aeolus_types_t

  let (component_type_names : component_type_name list ref) = ref []
  and (port_names           : port_name           list ref) = ref []
  and (package_names        : package_name        list ref) = ref []
  and (repository_names     : repository_name     list ref) = ref []  
  and (resource_names       : resource_name       list ref) = ref []

  let names_table = Hashtbl.create 53

  let initialize_names_table () =
    List.iter (fun component_type_name -> Hashtbl.add names_table component_type_name (fun lxm -> COMPONENT_TYPE_NAME(lxm))) !component_type_names;
    List.iter (fun port_name           -> Hashtbl.add names_table port_name           (fun lxm -> PORT_NAME(lxm)))           !port_names;
    List.iter (fun package_name        -> Hashtbl.add names_table package_name        (fun lxm -> PACKAGE_NAME(lxm)))        !package_names;
    List.iter (fun repository_name     -> Hashtbl.add names_table repository_name     (fun lxm -> REPOSITORY_NAME(lxm)))     !repository_names;
    List.iter (fun resource_name       -> Hashtbl.add names_table resource_name       (fun lxm -> RESOURCE_NAME(lxm)))       !resource_names;
    ()

  let name_token lxm =
    try
      let f = Hashtbl.find names_table lxm
      in
      f lxm
    with Not_found ->
      VAR_NAME(lxm)

}

rule token = parse
    [' ' '\t']                           { token lexbuf }     (* skip blanks *)
  | ['\n' ]                              { token lexbuf }
  | ['0'-'9']+ as lxm                    { INT(int_of_string lxm) }
  (*
  | "v$" (['A'-'Z' 'a'-'z' '-']+ as lxm) { VAR_NAME(lxm) }
  | "t$" (['A'-'Z' 'a'-'z' '-']+ as lxm) { COMPONENT_TYPE_NAME(lxm) }
  | "p$" (['A'-'Z' 'a'-'z' '-']+ as lxm) { PORT_NAME(lxm) }
  | "k$" (['A'-'Z' 'a'-'z' '-']+ as lxm) { PACKAGE_NAME(lxm) }
  | "r$" (['A'-'Z' 'a'-'z' '-']+ as lxm) { REPOSITORY_NAME(lxm) }
  | "o$" (['A'-'Z' 'a'-'z' '-']+ as lxm) { RESOURCE_NAME(lxm) }
  *)
  | "true"                               { TRUE }
  | '+'                                  { PLUS }
  | '-'                                  { MINUS }
  | '*'                                  { TIMES }
  | "and"                                { AND }
  | "or"                                 { OR }
  | "=>"                                 { IMPL }
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
(*  | eof               { raise Eof } *)