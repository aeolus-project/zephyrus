
open Helpers

open Facile_variables

module Facile_constraints (* :
  sig

    type var   = Facile.Var.Fd.t
    type cstr  = Facile.Cstr.t
    type expr  = Facile.Arith.t


    (* Printing *)

    val string_of_var         : var -> string
    val string_of_expr        : expr -> string
    val string_of_cstr        : cstr -> string


    (* Building expressions *)

    val var2expr   : var  -> expr
    val const2expr : int  -> expr

    val expr2var   : expr -> var


    (* Arith *)

    val ( +~ ) : expr -> expr -> expr
    val ( -~ ) : expr -> expr -> expr
    val ( *~ ) : expr -> expr -> expr
    val ( /~ ) : expr -> expr -> expr
    val ( %~ ) : expr -> expr -> expr

    val (  <~~ ) : expr -> expr -> expr
    val ( <=~~ ) : expr -> expr -> expr
    val (  =~~ ) : expr -> expr -> expr
    val ( >=~~ ) : expr -> expr -> expr
    val (  >~~ ) : expr -> expr -> expr
    val ( <>~~ ) : expr -> expr -> expr

    val sum : expr list -> expr


    (* Cstr *)
    
    val (  <~ ) : expr -> expr -> cstr
    val ( <=~ ) : expr -> expr -> cstr
    val (  =~ ) : expr -> expr -> cstr
    val ( >=~ ) : expr -> expr -> cstr
    val (  >~ ) : expr -> expr -> cstr
    val ( <>~ ) : expr -> expr -> cstr


    (* Reify *)

    val (  &&~~ ) : cstr -> cstr -> cstr    
    val (  ||~~ ) : cstr -> cstr -> cstr    
    val (  =>~~ ) : cstr -> cstr -> cstr    
    val ( <=>~~ ) : cstr -> cstr -> cstr    
    val xor       : cstr -> cstr -> cstr    
    val not       : cstr -> cstr


    (* Post constraints *)

    val post : cstr -> unit

  end *) = struct

    open Facile
    open Easy

    type var   = Facile.Var.Fd.t
    type cstr  = Facile.Cstr.t
    type expr  = Facile.Arith.t


    (* Printing *)

    let string_of_var  = Helpers.string_of_printing_function (Facile.Var.Fd.fprint)
    let string_of_expr = Helpers.string_of_printing_function (Facile.Arith.fprint)
    let string_of_cstr = Helpers.string_of_printing_function (Facile.Cstr.fprint)


    (* Building expressions *)

    let var2expr   (var   : var)  : expr = fd2e var
    let const2expr (const : int)  : expr = i2e  const
    
    let expr2var   (expr  : expr) : var  = Arith.e2fd expr

    (* Building constraints *)

    let trueexpr  : cstr = Cstr.one
    let falseexpr : cstr = Cstr.zero

    (* Reification *)
    
    let reify (cstr : cstr) = (var2expr (Reify.boolean cstr))

    (* Arith *)

    let ( +~ ) = Arith.( +~ )
    let ( -~ ) = Arith.( -~ )
    let ( *~ ) = Arith.( *~ )
    let ( /~ ) = Arith.( /~ )
    let ( %~ ) = Arith.( %~ )

    let (  <~~ ) = Arith.(  <~~ ) 
    let ( <=~~ ) = Arith.( <=~~ ) 
    let (  =~~ ) = Arith.(  =~~ ) 
    let ( >=~~ ) = Arith.( >=~~ ) 
    let (  >~~ ) = Arith.(  >~~ ) 
    let ( <>~~ ) = Arith.( <>~~ ) 

    let sum (exprs_to_sum : expr list) : expr = Arith.sum (Array.of_list exprs_to_sum)


    (* Cstr *)
    
    let (  <~ ) = Arith.(  <~ )
    let ( <=~ ) = Arith.( <=~ )
    let (  =~ ) = Arith.(  =~ )
    let ( >=~ ) = Arith.( >=~ )
    let (  >~ ) = Arith.(  >~ )
    let ( <>~ ) = Arith.( <>~ )


    (* Reify *)

    let (  &&~~ ) = Reify.(  &&~~ )
    let (  ||~~ ) = Reify.(  ||~~ )
    let (  =>~~ ) = Reify.(  =>~~ )
    let ( <=>~~ ) = Reify.( <=>~~ )
    let xor       = Reify.xor      
    let not       = Reify.not      


    (* Post constraints *)

    let post (cstr : cstr) = Cstr.post cstr


    (* Translation from the generic constraints *)

    module C = Generic_constraints


    let rec translate_var (variables : typing_context_variables) (var : C.var) : var =
      match var with
      | C.NamedVar  (var_key) -> get_variable variables var_key

    and translate_expr (variables : typing_context_variables) (expr : C.expr) : expr =
      match expr with
      | C.Const (const) -> const2expr const

      | C.Var (var) -> var2expr (translate_var variables var)
      
      | C.Reified (cstr) -> reify cstr

      | C.BinaryArithExpr (op, lexpr, rexpr) ->
          (match op with
          | C.Add -> (translate_expr variables lexpr) +~ (translate_expr variables rexpr)
          | C.Sub -> (translate_expr variables lexpr) -~ (translate_expr variables rexpr)
          | C.Mul -> (translate_expr variables lexpr) *~ (translate_expr variables rexpr)
          | C.Div -> (translate_expr variables lexpr) /~ (translate_expr variables rexpr)
          | C.Mod -> (translate_expr variables lexpr) %~ (translate_expr variables rexpr) )

      | C.NaryArithExpr (op, exprs) ->
          (match op with
          | C.Sum -> sum (List.map (translate_expr variables) exprs) )

      | C.BinaryArithCmpExpr (op, lexpr, rexpr) ->    
          (match op with
          | C.Lt  -> (translate_expr variables lexpr)  <~~ (translate_expr variables rexpr)
          | C.LEq -> (translate_expr variables lexpr) <=~~ (translate_expr variables rexpr)
          | C.Eq  -> (translate_expr variables lexpr)  =~~ (translate_expr variables rexpr)
          | C.GEq -> (translate_expr variables lexpr) >=~~ (translate_expr variables rexpr)
          | C.Gt  -> (translate_expr variables lexpr)  >~~ (translate_expr variables rexpr)
          | C.NEq -> (translate_expr variables lexpr) <>~~ (translate_expr variables rexpr) )

    and translate_cstr (variables : typing_context_variables) (cstr : C.cstr) : cstr =
      match cstr with
      | C.TrueCstr  -> trueexpr
      | C.FalseCstr -> falseexpr

      | C.BinaryArithCmpCstr (op, lexpr, rexpr) ->
          (match op with
          | C.Lt  -> (translate_expr variables lexpr)  <~ (translate_expr variables rexpr)
          | C.LEq -> (translate_expr variables lexpr) <=~ (translate_expr variables rexpr)
          | C.Eq  -> (translate_expr variables lexpr)  =~ (translate_expr variables rexpr)
          | C.GEq -> (translate_expr variables lexpr) >=~ (translate_expr variables rexpr)
          | C.Gt  -> (translate_expr variables lexpr)  >~ (translate_expr variables rexpr)
          | C.NEq -> (translate_expr variables lexpr) <>~ (translate_expr variables rexpr) )

      | C.BinaryCstrOpCstr (op, lcstr, rcstr) ->
          (match op with
          | C.And         -> (translate_cstr variables lcstr)  &&~~ (translate_cstr variables rcstr)
          | C.Or          -> (translate_cstr variables lcstr)  ||~~ (translate_cstr variables rcstr)
          | C.Impl        -> (translate_cstr variables lcstr)  =>~~ (translate_cstr variables rcstr)
          | C.IfAndOnlyIf -> (translate_cstr variables lcstr) <=>~~ (translate_cstr variables rcstr)
          | C.Xor         -> xor (translate_cstr variables lcstr) (translate_cstr variables rcstr) )

      | C.UnaryCstrOpCstr (op, cstr) ->
          (match op with
          | C.Not -> not (translate_cstr variables cstr) )


  end

open Facile_constraints

type typing_context_constraints = (string * (cstr list)) list
type specification_constraints = cstr list

(* Posting *)

let post_constraints constraints = 
  List.iter post constraints

let post_translation_constraints translation_constraints =
  List.iter (fun (constraints_group_name, constraints) -> 
    post_constraints constraints) 
  translation_constraints

let post_specification_constraints specification_constraints =
  post_constraints specification_constraints

let string_of_constraint = Facile_constraints.string_of_cstr

(* Printing *)
let string_of_constraints translation_constraints specification_constraints =
  let string_of_constraint_list constraints =
    let strings = List.map string_of_constraint constraints in
    lines_of_strings strings
  in
  let all_constraints : (string * (cstr list)) list =
    translation_constraints @ [("specification", specification_constraints)]
  in
  let strings =
    List.map (fun (constraints_group_name, constraints) ->

      Printf.sprintf "+ %s constraints:\n%s\n" constraints_group_name (string_of_constraint_list constraints)
      
    ) all_constraints
  in
  Printf.sprintf
    "\n%s\n"
    (lines_of_strings strings)


(* Goals *)
let create_minimal_resource_count_goal variables store_solution_here print_solutions =

    (* The goal is simple: all domain variables have to be set. *)
    let goal =
      Facile.Goals.List.forall (Facile.Goals.indomain) ( (domain_variables variables) @ (binding_variables variables) )
    in

    (* The cost variable to optimize (minimize): number of all resources. *)
    let number_of_all_resources =
      sum (List.map (fun variable -> var2expr variable) (domain_variables variables))
    in
    let cost_var =
      expr2var number_of_all_resources
    in
    
    (* Function called when a solution is found. *)
    let solution_found cost = 
      let solution = get_solution variables in
      store_solution_here := solution;
      
      if print_solutions then (
        Printf.printf
          "\n> Found a solution! Cost = %d %s" 
          cost 
          (string_of_solution solution) 
      );

    in
    Facile.Goals.minimize goal cost_var solution_found
