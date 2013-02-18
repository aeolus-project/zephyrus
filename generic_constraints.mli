
(* Operator types definitions *)

type unary_arith_op =
  | Abs

type binary_arith_op =
  | Add
  | Sub
  | Mul
  | Div
  | Mod

and nary_arith_op =
  | Sum

and binary_arith_cmp_op =
  | Lt
  | LEq
  | Eq
  | GEq
  | Gt
  | NEq

and binary_cstr_op =
  | And
  | Or
  | Impl
  | IfAndOnlyIf
  | Xor

and unary_cstr_op =
  | Not



(* Type definitions *)

type var = 
  | NamedVar  of Variable_keys.variable_key

and expr =
  | Const               of int                                (* expr = integer constant *)
  | Var                 of var                                (* expr = value of a variable *)
  | Reified             of cstr                               (* expr = if the constraint is satisfied then 1 else 0 *)
  | UnaryArithExpr      of unary_arith_op      * expr         (* expr = OP expr *)
  | BinaryArithExpr     of binary_arith_op     * expr * expr  (* expr = lexpr OP rexpr *)
  | NaryArithExpr       of nary_arith_op       * expr list    (* expr = OP (expr1, expr2, ... , expr) *)
  | BinaryArithCmpExpr  of binary_arith_cmp_op * expr * expr  (* expr = if (lexpr OP rexpr) then 1 else 0 *)

and cstr =
  | TrueCstr
  | FalseCstr
  | BinaryArithCmpCstr  of binary_arith_cmp_op * expr * expr  (* cstr : lexpr OP rexpr *)
  | BinaryCstrOpCstr    of binary_cstr_op      * cstr * cstr  (* cstr : lcstr OP rcstr *)
  | UnaryCstrOpCstr     of unary_cstr_op       * cstr         (* cstr : OP cstr' *)



(* Printing *)

val string_of_var  : var  -> string
val string_of_expr : expr -> string
val string_of_cstr : cstr -> string



(* Building expressions *)

val var        : Variable_keys.variable_key -> var

val var2expr   : var  -> expr
val const2expr : int  -> expr

(* Building constraints *)

val truecstr  : cstr
val falsecstr : cstr


(* Reification *)

val reify : cstr -> expr

(* Arithmetic operators *)

val ( +~ ) : expr -> expr -> expr
val ( -~ ) : expr -> expr -> expr
val ( *~ ) : expr -> expr -> expr
val ( /~ ) : expr -> expr -> expr
val ( %~ ) : expr -> expr -> expr
val abs    : expr -> expr

val sum : expr list -> expr


(* Reified arithmetic comparisons *)

val (  <~~ ) : expr -> expr -> expr
val ( <=~~ ) : expr -> expr -> expr
val (  =~~ ) : expr -> expr -> expr
val ( >=~~ ) : expr -> expr -> expr
val (  >~~ ) : expr -> expr -> expr
val ( <>~~ ) : expr -> expr -> expr


(* Arithmetic comparisons *)

val (  <~ ) : expr -> expr -> cstr
val ( <=~ ) : expr -> expr -> cstr
val (  =~ ) : expr -> expr -> cstr
val ( >=~ ) : expr -> expr -> cstr
val (  >~ ) : expr -> expr -> cstr
val ( <>~ ) : expr -> expr -> cstr


(* Constraint operators *)

val (  &&~~ ) : cstr -> cstr -> cstr    
val (  ||~~ ) : cstr -> cstr -> cstr    
val (  =>~~ ) : cstr -> cstr -> cstr    
val ( <=>~~ ) : cstr -> cstr -> cstr    
val xor       : cstr -> cstr -> cstr    
val not       : cstr -> cstr



val variable_keys_of_cstr : cstr -> Variable_keys.variable_key list