
%token <int> INT
%token <string> NAME
%token EQ SEMICOLON
%token EOF 
%start main                  /* the entry point */
%type <(string * int) list> main
%%
main:
  variable_bindings EOF { $1 }
;

minizinc_variable :
  | NAME { $1 }

value :
  | INT { $1 }

variable_binding:
  | minizinc_variable EQ value SEMICOLON { ($1, $3) }

variable_bindings:
  | variable_binding { [ $1 ] }
  | variable_binding variable_bindings { $1 :: $2 }
