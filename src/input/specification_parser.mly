%{ (****************************************************************************)
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
%}

%token <int> INT
%token <string> NAME
%token <string> COMPONENT_TYPE_NAME
%token <string> PORT_NAME
%token <string> PACKAGE_NAME
%token EVERYWHERE AT
%token TRUE
%token PLUS MINUS TIMES
%token AND OR IMPL NOT
%token LT LEQ EQ GEQ GT NEQ
%token HASH
%token COMMA COLON SEMICOLON UNDERSCORE
%token LCURLY RCURLY
%token LPAREN RPAREN
%token EOF
%left AND OR IMPL            /* lowest precedence */
%left PLUS MINUS
%left TIMES                  /* medium precedence */
%nonassoc NOT                /* highest precedence */
%start main                  /* the entry point */
%type <Abstract_io.specification> main

%%
main:
  specification EOF { $1 }
;

spec_const:
  | INT                      { $1 }
  | LPAREN spec_const RPAREN { $2 }

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

repository_name:
  | NAME                          { $1 }
  | LPAREN repository_name RPAREN { $2 }

resource_name:
  | NAME                        { $1 }
  | LPAREN resource_name RPAREN { $2 }

location_name:
  | NAME                        { $1 }
  | LPAREN location_name RPAREN { $2 }

state_name:
  | NAME                     { $1 }
  | LPAREN state_name RPAREN { $2 }  

spec_variable_name:
  | NAME                             { $1 }
  | LPAREN spec_variable_name RPAREN { $2 }



specification:
  | TRUE                                                                   { Abstract_io.SpecTrue              }
  | spec_expr spec_op spec_expr                                            { Abstract_io.SpecOp   ($1, $2, $3) }
  | specification AND  specification                                       { Abstract_io.SpecAnd  ($1, $3)     }
  | specification OR   specification                                       { Abstract_io.SpecOr   ($1, $3)     }
  | specification IMPL specification                                       { Abstract_io.SpecImpl ($1, $3)     }
  | NOT specification                                                      { Abstract_io.SpecNot  ($2)         }
  | LPAREN specification RPAREN                                            { $2 }
  | EVERYWHERE                           LPAREN local_specification RPAREN { Abstract_io.SpecEverywhere ($3)     }
  | AT LCURLY spec_location_names RCURLY LPAREN local_specification RPAREN { Abstract_io.SpecAt         ($3, $6) }

spec_location_names:
  | location_name                            { [$1] }
  | location_name COMMA spec_location_names  { $1 :: $3 }

spec_expr:
  | spec_variable_name          { Abstract_io.SpecExprVar   ($1)     }
  | spec_const                  { Abstract_io.SpecExprConst ($1)     }
  | HASH spec_element           { Abstract_io.SpecExprArity ($2)     }
  | spec_const TIMES spec_expr  { Abstract_io.SpecExprMul   ($1, $3) }
  | spec_expr  TIMES spec_const { Abstract_io.SpecExprMul   ($3, $1) }
  | spec_expr  PLUS  spec_expr  { Abstract_io.SpecExprAdd   ($1, $3) }
  | spec_expr  MINUS spec_expr  { Abstract_io.SpecExprSub   ($1, $3) }
  | LPAREN spec_expr RPAREN     { $2 }

spec_element:
  | LPAREN repository_name COMMA package_name RPAREN       { Abstract_io.SpecElementPackage       ($2, $4) }
  | component_type_name COLON state_name                   { Abstract_io.SpecElementComponentType (Abstract_io.Component_type_state($1, $3)) }
  | LPAREN component_type_name COLON state_name RPAREN     { Abstract_io.SpecElementComponentType (Abstract_io.Component_type_state($2, $4)) }
  | component_type_name                                    { Abstract_io.SpecElementComponentType (Abstract_io.Component_type_simple($1   )) }
  | LPAREN component_type_name RPAREN                      { Abstract_io.SpecElementComponentType (Abstract_io.Component_type_simple($2   )) }
  | port_name                                              { Abstract_io.SpecElementPort          ($1) }
  | LPAREN spec_resource_constraints RPAREN 
    LCURLY spec_repository_constraints COLON 
    local_specification RCURLY                             { Abstract_io.SpecElementLocalisation ($2, $5, $7) }
  | LPAREN spec_element RPAREN                             { $2 }

local_specification:
  | TRUE                                         { Abstract_io.SpecLocalTrue              }
  | spec_local_expr spec_op spec_local_expr      { Abstract_io.SpecLocalOp   ($1, $2, $3) }
  | local_specification AND  local_specification { Abstract_io.SpecLocalAnd  ($1, $3)     }
  | local_specification OR   local_specification { Abstract_io.SpecLocalOr   ($1, $3)     }
  | local_specification IMPL local_specification { Abstract_io.SpecLocalImpl ($1, $3)     }
  | NOT local_specification                      { Abstract_io.SpecLocalNot  ($2)         }
  | LPAREN local_specification RPAREN            { $2 }

spec_local_expr:
  | spec_variable_name                      { Abstract_io.SpecLocalExprVar   ($1) }
  | spec_const                              { Abstract_io.SpecLocalExprConst ($1) }
  | HASH spec_local_element                 { Abstract_io.SpecLocalExprArity ($2) }
  | spec_const      TIMES spec_local_expr   { Abstract_io.SpecLocalExprMul ($1, $3) }
  | spec_local_expr TIMES spec_const        { Abstract_io.SpecLocalExprMul ($3, $1) }
  | spec_local_expr PLUS  spec_local_expr   { Abstract_io.SpecLocalExprAdd ($1, $3) }
  | spec_local_expr MINUS spec_local_expr   { Abstract_io.SpecLocalExprSub ($1, $3) }
  | LPAREN spec_local_expr RPAREN           { $2 }

spec_local_element:
  | LPAREN repository_name COMMA package_name RPAREN   { Abstract_io.SpecLocalElementPackage       ($2, $4) }
  | component_type_name COLON state_name               { Abstract_io.SpecLocalElementComponentType (Abstract_io.Component_type_state($1, $3)) }
  | LPAREN component_type_name COLON state_name RPAREN { Abstract_io.SpecLocalElementComponentType (Abstract_io.Component_type_state($2, $4)) }
  | component_type_name                                { Abstract_io.SpecLocalElementComponentType (Abstract_io.Component_type_simple($1   )) }
  | LPAREN component_type_name RPAREN                  { Abstract_io.SpecLocalElementComponentType (Abstract_io.Component_type_simple($2   )) }
  | port_name                                          { Abstract_io.SpecLocalElementPort          ($1) }
  | LPAREN spec_local_element RPAREN                   { $2 }

spec_resource_constraints:
  | UNDERSCORE                                                     { [] }
  | spec_resource_constraint                                       { [$1] }
  | spec_resource_constraint SEMICOLON spec_resource_constraints   { ($1) :: ($3) }

spec_resource_constraint:
  | resource_name spec_op spec_const       { ($1, $2, $3) }
  | LPAREN spec_resource_constraint RPAREN { $2 }

spec_repository_constraints:
  | UNDERSCORE                                                 { [] }
  | spec_repository_constraint                                 { [$1] }
  | spec_repository_constraint OR spec_repository_constraints  { $1 :: $3 }

spec_repository_constraint:
    repository_name { $1 }

spec_op:
  | LT  { Abstract_io.Lt  }
  | LEQ { Abstract_io.LEq }
  | EQ  { Abstract_io.Eq  }
  | GEQ { Abstract_io.GEq }
  | GT  { Abstract_io.Gt  }
  | NEQ { Abstract_io.NEq }



