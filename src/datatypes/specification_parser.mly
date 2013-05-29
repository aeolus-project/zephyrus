
%token <int> INT
%token <string> NAME
%token <string> COMPONENT_TYPE_NAME
%token <string> PORT_NAME
%token <string> PACKAGE_NAME
%token TRUE
%token PLUS MINUS TIMES
%token AND OR IMPL NOT
%token LT LEQ EQ GEQ GT NEQ
%token HASH
%token COLON SEMICOLON UNDERSCORE
%token LCURLY RCURLY
%token LPAREN RPAREN
%token EOF
%left AND OR IMPL            /* lowest precedence */
%left PLUS MINUS
%left TIMES                  /* medium precedence */
%nonassoc NOT                /* highest precedence */
%start main                  /* the entry point */
%type <Model_t.specification> main

%%
main:
  specification EOF { $1 }
;

spec_const:
  | INT                      { $1 }
  | LPAREN spec_const RPAREN { $2 }

/* Three names which types have to be distinguished.
   In the context where they are used we do not know a priori if
   a given name denotes a component type, a port or a package.
   We need to find out using a universe. */
component_type_name:
  | COMPONENT_TYPE_NAME               { $1 }
  | LPAREN component_type_name RPAREN { $2 }

port_name:
  | PORT_NAME               { $1 }
  | LPAREN port_name RPAREN { $2 }

package_name:
  | PACKAGE_NAME               { $1 }
  | NAME                       { $1 }
  | LPAREN package_name RPAREN { $2 }

/* These names always appear in a context which clearly
   decides which one of them we mean. */
repository_name:
  | NAME                          { $1 }
  | LPAREN repository_name RPAREN { $2 }

resource_name:
  | NAME                        { $1 }
  | LPAREN resource_name RPAREN { $2 }

spec_variable_name:
  | NAME                             { $1 }
  | LPAREN spec_variable_name RPAREN { $2 }

specification:
  | TRUE                             { `SpecTrue              }
  | spec_expr spec_op spec_expr      { `SpecOp   ($1, $2, $3) }
  | specification AND  specification { `SpecAnd  ($1, $3)     }
  | specification OR   specification { `SpecOr   ($1, $3)     }
  | specification IMPL specification { `SpecImpl ($1, $3)     }
  | NOT specification                { `SpecNot  ($2)         }
  | LPAREN specification RPAREN      { $2 }


spec_expr:
  | spec_variable_name          { `SpecExprVar   ($1) }
  | spec_const                  { `SpecExprConst ($1) }
  | HASH spec_element           { `SpecExprArity ($2) }
  | spec_const TIMES spec_expr  { `SpecExprMul ($1, $3) }
  | spec_expr  TIMES spec_const { `SpecExprMul ($3, $1) }
  | spec_expr  PLUS  spec_expr  { `SpecExprAdd ($1, $3) }
  | spec_expr  MINUS spec_expr  { `SpecExprSub ($1, $3) }
  | LPAREN spec_expr RPAREN     { $2 }

spec_element:
  | package_name        { `SpecElementPackage       ($1) }
  | component_type_name { `SpecElementComponentType ($1) }
  | port_name           { `SpecElementPort          ($1) }
  | LPAREN spec_resource_constraints RPAREN 
    LCURLY spec_repository_constraints COLON local_specification RCURLY
     { `SpecElementLocalisation ($2, $5, $7) }
  | LPAREN spec_element RPAREN { $2 }

local_specification:
  | TRUE                                         { `SpecLocalTrue              }
  | spec_local_expr spec_op spec_local_expr      { `SpecLocalOp   ($1, $2, $3) }
  | local_specification AND  local_specification { `SpecLocalAnd  ($1, $3)     }
  | local_specification OR   local_specification { `SpecLocalOr   ($1, $3)     }
  | local_specification IMPL local_specification { `SpecLocalImpl ($1, $3)     }
  | NOT local_specification                      { `SpecLocalNot  ($2)         }
  | LPAREN local_specification RPAREN            { $2 }

spec_local_expr:
  | spec_variable_name                      { `SpecLocalExprVar   ($1) }
  | spec_const                              { `SpecLocalExprConst ($1) }
  | HASH spec_local_element                 { `SpecLocalExprArity ($2) }
  | spec_const      TIMES spec_local_expr   { `SpecLocalExprMul ($1, $3) }
  | spec_local_expr TIMES spec_const        { `SpecLocalExprMul ($3, $1) }
  | spec_local_expr PLUS  spec_local_expr   { `SpecLocalExprAdd ($1, $3) }
  | spec_local_expr MINUS spec_local_expr   { `SpecLocalExprSub ($1, $3) }
  | LPAREN spec_local_expr RPAREN           { $2 }

spec_local_element:
  | package_name        { `SpecLocalElementPackage       ($1) }
  | component_type_name { `SpecLocalElementComponentType ($1) }
  | port_name           { `SpecLocalElementPort          ($1) }
  | LPAREN spec_local_element RPAREN { $2 }

spec_resource_constraints:
  | UNDERSCORE                                                     { [] }
  | spec_resource_constraint                                       { [$1] }
  | spec_resource_constraint SEMICOLON spec_resource_constraints   { ($1) :: ($3) }

spec_resource_constraint:
  | resource_name spec_op spec_const   { ($1, $2, $3) }
  | LPAREN spec_resource_constraint RPAREN { $2 }

spec_repository_constraints:
  | UNDERSCORE                                                 { [] }
  | spec_repository_constraint                                 { [$1] }
  | spec_repository_constraint OR spec_repository_constraints  { $1 :: $3 }

spec_repository_constraint:
    repository_name { $1 }

spec_op:
  | LT  { `Lt  }
  | LEQ { `LEq }
  | EQ  { `Eq  }
  | GEQ { `GEq }
  | GT  { `Gt  }
  | NEQ { `NEq }
