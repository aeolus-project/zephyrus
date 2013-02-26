
open Helpers

open Aeolus_types_j

open Typing_context
open Variable_keys
open Facile_variables
open Solution

module Facile_constraints = 
  struct

    open Facile
    open Easy

    type const = int
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

    let truecstr  : cstr = Cstr.one
    let falsecstr : cstr = Cstr.zero

    (* Reification *)
    
    let reify (cstr : cstr) = (var2expr (Reify.boolean cstr))

    (* Arith *)

    let ( +~ ) = Arith.( +~ )
    let ( -~ ) = Arith.( -~ )
    let ( *~ ) = Arith.( *~ )
    let ( /~ ) = Arith.( /~ )
    let ( %~ ) = Arith.( %~ )
    let abs    = Arith.abs

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

    let post (cstr : cstr) : unit = Cstr.post cstr


    (* Translation from the generic constraints *)

    module C = Generic_constraints


    let rec translate_var (facile_variables : facile_variables) (var : C.var) : var =
      match var with
      | C.NamedVar  (var_key) -> get_facile_variable facile_variables var_key

    and translate_expr (facile_variables : facile_variables) (expr : C.expr) : expr =
      match expr with
      | C.Const (const) -> 
        (
          match const with
          | C.Int i    -> const2expr i
          | C.Inf sign -> const2expr 1000 
          (* TODO: Either make a true infinity (is it possible in FaCiLe?) or transform the generic constraints. *)
        )

      | C.Var (var) -> var2expr (translate_var facile_variables var)
      
      | C.Reified (cstr) -> reify (translate_cstr facile_variables cstr)
      
      | C.UnaryArithExpr (op, expr) ->
          (match op with
          | C.Abs -> abs (translate_expr facile_variables expr) )
      
      | C.BinaryArithExpr (op, lexpr, rexpr) ->
          (match op with
          | C.Add -> (translate_expr facile_variables lexpr) +~ (translate_expr facile_variables rexpr)
          | C.Sub -> (translate_expr facile_variables lexpr) -~ (translate_expr facile_variables rexpr)
          | C.Mul -> (translate_expr facile_variables lexpr) *~ (translate_expr facile_variables rexpr)
          | C.Div -> (translate_expr facile_variables lexpr) /~ (translate_expr facile_variables rexpr)
          | C.Mod -> (translate_expr facile_variables lexpr) %~ (translate_expr facile_variables rexpr) )

      | C.NaryArithExpr (op, exprs) ->
          (match op with
          | C.Sum -> sum (List.map (translate_expr facile_variables) exprs) )

      | C.BinaryArithCmpExpr (op, lexpr, rexpr) ->    
          (match op with
          | C.Lt  -> (translate_expr facile_variables lexpr)  <~~ (translate_expr facile_variables rexpr)
          | C.LEq -> (translate_expr facile_variables lexpr) <=~~ (translate_expr facile_variables rexpr)
          | C.Eq  -> (translate_expr facile_variables lexpr)  =~~ (translate_expr facile_variables rexpr)
          | C.GEq -> (translate_expr facile_variables lexpr) >=~~ (translate_expr facile_variables rexpr)
          | C.Gt  -> (translate_expr facile_variables lexpr)  >~~ (translate_expr facile_variables rexpr)
          | C.NEq -> (translate_expr facile_variables lexpr) <>~~ (translate_expr facile_variables rexpr) )

    and translate_cstr (facile_variables : facile_variables) (cstr : C.cstr) : cstr =
      match cstr with
      | C.TrueCstr  -> truecstr
      | C.FalseCstr -> falsecstr

      | C.BinaryArithCmpCstr (op, lexpr, rexpr) ->
          (match op with
          | C.Lt  -> (translate_expr facile_variables lexpr)  <~ (translate_expr facile_variables rexpr)
          | C.LEq -> (translate_expr facile_variables lexpr) <=~ (translate_expr facile_variables rexpr)
          | C.Eq  -> (translate_expr facile_variables lexpr)  =~ (translate_expr facile_variables rexpr)
          | C.GEq -> (translate_expr facile_variables lexpr) >=~ (translate_expr facile_variables rexpr)
          | C.Gt  -> (translate_expr facile_variables lexpr)  >~ (translate_expr facile_variables rexpr)
          | C.NEq -> (translate_expr facile_variables lexpr) <>~ (translate_expr facile_variables rexpr) )

      | C.BinaryCstrOpCstr (op, lcstr, rcstr) ->
          (match op with
          | C.And         -> (translate_cstr facile_variables lcstr)  &&~~ (translate_cstr facile_variables rcstr)
          | C.Or          -> (translate_cstr facile_variables lcstr)  ||~~ (translate_cstr facile_variables rcstr)
          | C.Impl        -> (translate_cstr facile_variables lcstr)  =>~~ (translate_cstr facile_variables rcstr)
          | C.IfAndOnlyIf -> (translate_cstr facile_variables lcstr) <=>~~ (translate_cstr facile_variables rcstr)
          | C.Xor         -> xor (translate_cstr facile_variables lcstr) (translate_cstr facile_variables rcstr) )

      | C.UnaryCstrOpCstr (op, cstr) ->
          (match op with
          | C.Not -> not (translate_cstr facile_variables cstr) )


  end

open Facile_constraints

type generated_constraints = (string * (cstr list)) list

(* Posting *)

let post_constraints constraints = 
  List.iter post constraints

let post_translation_constraints translation_constraints =
  List.iter (fun (constraints_group_name, constraints) -> 
    post_constraints constraints) 
  translation_constraints

let string_of_constraint = Facile_constraints.string_of_cstr

(* Printing *)
let string_of_constraints constraints =
  let string_of_constraint_list constraints =
    let strings = List.map string_of_constraint constraints in
    lines_of_strings strings
  in
  let strings =
    List.map (fun (constraints_group_name, constraints) ->

      Printf.sprintf
        "+ %s constraints:\n%s\n"
        constraints_group_name
        (string_of_constraint_list constraints)
      
    ) constraints
  in
  Printf.sprintf
    "\n%s\n"
    (lines_of_strings strings)


(* Goals *)
let create_optimized_goal facile_variables cost_expr store_solution_here print_solutions =

    (* The goal is simple: all domain facile_variables have to be set. *)
    let goal =
      Facile.Goals.List.forall (Facile.Goals.indomain) (get_facile_variables facile_variables)
    in

    (* The cost variable to optimize (minimize): *)
    let cost_var =
      expr2var cost_expr
    in
    
    (* Function called when a solution is found. *)
    let solution_found cost = 
      let solution = solution_of_facile_variables facile_variables in
      store_solution_here := solution;
      
      if print_solutions then (
        Printf.printf
          "\n> Found a solution! Cost = %d %s" 
          cost 
          (string_of_solution solution);
        flush stdout;
      );

    in
    Facile.Goals.minimize goal cost_var solution_found
