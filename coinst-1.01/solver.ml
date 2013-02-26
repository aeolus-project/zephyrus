(* Solver *)
(* SAT solver by Jerôme Vouillon *)

let debug = ref false

module type S = sig
  type reason
end

module type SOLVER = sig
  type state

  type reason

  type var = int
  type lit
  val lit_of_var : var -> bool -> lit

  val initialize_problem :
    ?print_var:(Format.formatter -> int -> unit) -> int -> state
  val propagate : state -> unit

  val protect : state -> unit
  val reset : state -> unit

  type value = True | False | Unknown
  val assignment : state -> value array

  val add_rule : state -> lit array -> reason list -> unit
  val associate_vars : state -> lit -> var list -> unit

  val solve : state -> var -> bool
  val solve_lst : state -> var list -> bool

  val collect_reasons : state -> var -> reason list
  val collect_reasons_lst : state -> var list -> reason list
end

module F (X : S) = struct

(* Variables *)
type var = int

(* Literals *)
type lit = int

type reason = X.reason

(* A clause is an array of literals *)
type clause =
  { lits : lit array;
    all_lits : lit array;
    reasons : reason list }

type value = True | False | Unknown

module LitMap =
  Map.Make (struct type t = int let compare (x : int) y = compare x y end)

type state =
  { (* Indexed by var *)
    st_assign : value array;
    st_reason : clause option array;
    st_level : int array;
    st_seen_var : int array;
    st_refs : int array;
    st_pinned : bool array;
    (* Indexed by lit *)
    st_simpl_prop : clause LitMap.t array;
    st_watched : clause list array;
    st_associated_vars : var list array;
    (* Queues *)
    mutable st_trail : lit list;
    mutable st_trail_lim : lit list list;
    st_prop_queue : lit Queue.t;
    (* Misc *)
    mutable st_cur_level : int;
    mutable st_min_level : int;
    mutable st_seen : int;
    mutable st_var_queue_head : var list;
    st_var_queue : var Queue.t;
    mutable st_cost : int; (* Total computational cost so far *)
    st_print_var : Format.formatter -> int -> unit;
    mutable st_coherent : bool }

(****)

let charge st x = st.st_cost <- st.st_cost + x
let get_bill st = st.st_cost

(****)

let pin_var st x = st.st_pinned.(x) <- true

let unpin_var st x = st.st_pinned.(x) <- false

let enqueue_var st x =
  charge st 1;
  pin_var st x;
  Queue.push x st.st_var_queue

let requeue_var st x =
  pin_var st x;
  st.st_var_queue_head <- x :: st.st_var_queue_head

(* Returns -1 if no variable remains *)
let rec dequeue_var st =
  let x =
    match st.st_var_queue_head with
      x :: r -> st.st_var_queue_head <- r; x
    | []     -> try Queue.take st.st_var_queue with Queue.Empty -> -1
  in
  if x = -1 then x else begin
    unpin_var st x;
    if st.st_refs.(x) = 0 || st.st_assign.(x) <> Unknown then
      dequeue_var st
    else
      x
  end

(****)

let var_of_lit p = p lsr 1
let pol_of_lit p = p land 1 = 0
let lit_of_var v s = if s then v + v else v + v + 1
let lit_neg p = p lxor 1

let val_neg v =
  match v with
    True    -> False
  | False   -> True
  | Unknown -> Unknown

let val_of_bool b = if b then True else False

let val_of_lit st p =
  let v = st.st_assign.(var_of_lit p) in
  if pol_of_lit p then v else val_neg v

(****)

let print_val ch v =
  Format.fprintf ch "%s"
    (match v with True -> "True" | False -> "False" | Unknown -> "Unknown")

let print_lits st ch lits =
  Format.fprintf ch "{";
  Array.iter
    (fun p ->
       if pol_of_lit p then
         Format.fprintf ch " +%a" st.st_print_var (var_of_lit p)
       else
         Format.fprintf ch " -%a" st.st_print_var (var_of_lit p))
    lits;
  Format.fprintf ch " }"

let print_rule st ch r = print_lits st ch r.lits

(****)

exception Conflict of clause option

let enqueue st p reason =
  charge st 1;
  if !debug then begin
    match reason with
      Some r -> Format.eprintf "Applying rule %a@." (print_rule st) r
    | _ -> ()
  end;
  match val_of_lit st p with
    False ->
      if !debug then begin
        if pol_of_lit p then
          Format.eprintf "Cannot install %a@." st.st_print_var (var_of_lit p)
        else
          Format.eprintf "Already installed %a@."
            st.st_print_var (var_of_lit p)
      end;
      raise (Conflict reason)
  | True ->
      ()
  | Unknown ->
      if !debug then begin
        if pol_of_lit p then
          Format.eprintf "Installing %a@." st.st_print_var (var_of_lit p)
        else
          Format.eprintf "Should not install %a@."
            st.st_print_var (var_of_lit p);
      end;
      let x = var_of_lit p in
      st.st_assign.(x) <- val_of_bool (pol_of_lit p);
      st.st_reason.(x) <- reason;
      st.st_level.(x) <- st.st_cur_level;
      st.st_trail <- p :: st.st_trail;
      List.iter
        (fun x ->
           charge st 1;
           let refs = st.st_refs.(x) in
           if refs = 0 then enqueue_var st x;
           st.st_refs.(x) <- st.st_refs.(x) + 1)
        st.st_associated_vars.(p);
      Queue.push p st.st_prop_queue

let rec find_not_false st lits i l =
  if i = l then -1 else
  if val_of_lit st lits.(i) <> False then i else
  find_not_false st lits (i + 1) l

let propagate_in_clause st r p =
  charge st 1;
  let p' = lit_neg p in
  if r.lits.(0) = p' then begin
    r.lits.(0) <- r.lits.(1);
    r.lits.(1) <- p'
  end;
  if val_of_lit st r.lits.(0) = True then
    st.st_watched.(p) <- r :: st.st_watched.(p)
  else begin
    let i = find_not_false st r.lits 2 (Array.length r.lits) in
    if i = -1 then begin
      st.st_watched.(p) <- r :: st.st_watched.(p);
      enqueue st r.lits.(0) (Some r)
    end else begin
      r.lits.(1) <- r.lits.(i);
      r.lits.(i) <- p';
      let p = lit_neg r.lits.(1) in
      st.st_watched.(p) <- r :: st.st_watched.(p)
    end
  end

let propagate st =
  try
    while not (Queue.is_empty st.st_prop_queue) do
      charge st 1;
      let p = Queue.take st.st_prop_queue in
      LitMap.iter (fun p r -> enqueue st p (Some r)) st.st_simpl_prop.(p);
      let l = ref (st.st_watched.(p)) in
      st.st_watched.(p) <- [];
      begin try
        while
          match !l with
            r :: rem ->
              l := rem;
              propagate_in_clause st r p;
              true
          | [] ->
              false
        do () done
      with Conflict _ as e ->
        st.st_watched.(p) <- !l @ st.st_watched.(p);
        raise e
      end
    done
  with Conflict _ as e ->
    Queue.clear st.st_prop_queue;
    raise e

(****)

let raise_level st =
  st.st_cur_level <- st.st_cur_level + 1;
  st.st_trail_lim <- st.st_trail :: st.st_trail_lim;
  st.st_trail <- []

let assume st p =
  raise_level st;
  enqueue st p None

let protect st =
  propagate st;
  raise_level st;
  st.st_min_level <- st.st_cur_level

let undo_one st p =
  let x = var_of_lit p in
  if !debug then Format.eprintf "Cancelling %a@." st.st_print_var x;
  st.st_assign.(x) <- Unknown;
  st.st_reason.(x) <- None;
  st.st_level.(x) <- -1;
  List.iter
    (fun x -> charge st 1; st.st_refs.(x) <- st.st_refs.(x) - 1)
    st.st_associated_vars.(p);
  if st.st_refs.(x) > 0 && not st.st_pinned.(x) then enqueue_var st x

let cancel st =
  st.st_cur_level <- st.st_cur_level - 1;
  List.iter (fun p -> undo_one st p) st.st_trail;
  match st.st_trail_lim with
    []     -> assert false
  | l :: r -> st.st_trail <- l; st.st_trail_lim <- r

let reset st =
  if !debug then Format.eprintf "Reset@.";
  while st.st_trail_lim <> [] do cancel st done;
  for i = 0 to Array.length st.st_refs - 1 do
    st.st_refs.(i) <- 0;
    st.st_pinned.(i) <- false
  done;
  st.st_var_queue_head <- [];
  st.st_min_level <- 0;
  Queue.clear st.st_var_queue;
  st.st_coherent <- true

(****)

let rec find_next_lit st =
  match st.st_trail with
    [] ->
      assert false
  | p :: rem ->
      st.st_trail <- rem;
      if st.st_seen_var.(var_of_lit p) = st.st_seen then
        let reason = st.st_reason.(var_of_lit p) in
        undo_one st p;
        (p, reason)
      else begin
        undo_one st p;
        find_next_lit st
      end

let analyze st conflict =
  st.st_seen <- st.st_seen + 1;
  let counter = ref 0 in
  let learnt = ref [] in
  let bt_level  = ref 0 in
  let reasons = ref [] in
  let r = ref conflict in
  while
    if !debug then begin
      Array.iter
        (fun p ->
           Format.eprintf "%d:%a (%b/%d) "
             p print_val (val_of_lit st p)
             (st.st_reason.(var_of_lit p) <> None)
             st.st_level.(var_of_lit p))
        !r.lits;
      Format.eprintf "@."
    end;
    reasons := !r.reasons @ !reasons;

    for i = 0 to Array.length !r.lits - 1 do
      let p = !r.lits.(i) in
      let x = var_of_lit p in
      if  st.st_seen_var.(x) <> st.st_seen then begin
        assert (val_of_lit st p = False);
        st.st_seen_var.(x) <- st.st_seen;
        let level = st.st_level.(x) in
        if level = st.st_cur_level then begin
          incr counter
        end else (* if level > 0 then*) begin
          learnt := p :: !learnt;
          bt_level := max level !bt_level
        end
      end
    done;

    let (p, reason) = find_next_lit st in
    decr counter;
    if !counter  = 0 then
      learnt := lit_neg p :: !learnt
    else
      begin match reason with
        Some r' -> r := r'
      | None    -> assert false
      end;
    !counter > 0
  do () done;
  if !debug then begin
    List.iter
      (fun p ->
         Format.eprintf "%d:%a/%d "
           p print_val (val_of_lit st p) st.st_level.(var_of_lit p))
      !learnt;
    Format.eprintf "@."
  end;
  (Array.of_list !learnt, !reasons, !bt_level)

let find_highest_level st lits =
  let level = ref (-1) in
  let i = ref 0 in
  Array.iteri
    (fun j p ->
       if st.st_level.(var_of_lit p) > !level then begin
         level := st.st_level.(var_of_lit p);
         i := j
       end)
    lits;
  !i

let rec solve_rec st =
  match try propagate st; None with Conflict r -> Some r with
    None ->
      let x = dequeue_var st in
      x < 0 ||
      begin
        assume st (lit_of_var x false);
        solve_rec st
      end
  | Some r ->
      let r =
        match r with
          None   -> assert false
        | Some r -> r
      in
      let (learnt, reasons, level) = analyze st r in
      let level = max st.st_min_level level in
      while st.st_cur_level > level do cancel st done;
      assert (val_of_lit st learnt.(0) = Unknown);
      let rule = { lits = learnt; all_lits = learnt; reasons = reasons } in
      if !debug then Format.eprintf "Learning %a@." (print_rule st) rule;
      if Array.length learnt > 1 then begin
        let i = find_highest_level st learnt in
        assert (i > 0);
        let p' = learnt.(i) in
        learnt.(i) <- learnt.(1);
        learnt.(1) <- p';
        let p = lit_neg learnt.(0) in
        let p' = lit_neg p' in
        st.st_watched.(p) <- rule :: st.st_watched.(p);
        st.st_watched.(p') <- rule :: st.st_watched.(p')
      end;
      enqueue st learnt.(0) (Some rule);
      st.st_cur_level > st.st_min_level &&
      solve_rec st

let rec solve st x =
  assert (st.st_cur_level = st.st_min_level);
  propagate st;
  try
    let p = lit_of_var x true in
    assume st p;
    assert (st.st_cur_level = st.st_min_level + 1);
    if solve_rec st then begin
      protect st;
      true
    end else
      solve st x
  with Conflict _ ->
    st.st_coherent <- false;
    false

let rec solve_lst_rec st l0 l =
  match l with
    [] ->
      true
  | x :: r ->
      protect st;
      List.iter (fun x -> enqueue st (lit_of_var x true) None) l0;
      propagate st;
      if solve st x then begin
        if r <> [] then reset st;
        solve_lst_rec st (x :: l0) r
      end else
        false

let solve_lst st l = solve_lst_rec st [] l

let initialize_problem ?(print_var = (fun fmt -> Format.fprintf fmt "%d")) n =
  { st_assign = Array.make n Unknown;
    st_reason = Array.make n None;
    st_level = Array.make n (-1);
    st_seen_var = Array.make n (-1);
    st_refs = Array.make n 0;
    st_pinned = Array.make n false;
    st_simpl_prop = Array.make (2 * n) LitMap.empty;
    st_watched = Array.make (2 * n) [];
    st_associated_vars = Array.make (2 * n) [];
    st_trail = [];
    st_trail_lim = [];
    st_prop_queue = Queue.create ();
    st_cur_level = 0;
    st_min_level = 0;
    st_seen = 0;
    st_var_queue_head = [];
    st_var_queue = Queue.create ();
    st_cost = 0;
    st_print_var = print_var;
    st_coherent = true }

let insert_simpl_prop st r p p' =
  let p = lit_neg p in
  if not (LitMap.mem p' st.st_simpl_prop.(p)) then
    st.st_simpl_prop.(p) <- LitMap.add p' r st.st_simpl_prop.(p)

let add_bin_rule st lits p p' reasons =
  let r = { lits = [|p; p'|]; all_lits = lits; reasons = reasons } in
  insert_simpl_prop st r p p';
  insert_simpl_prop st r p' p

let add_un_rule st lits p reasons =
  let r = { lits = [|p|]; all_lits = lits; reasons = reasons } in
  enqueue st p (Some r)

let add_rule st lits reasons =
  let all_lits = Array.copy lits in
  let is_true = ref false in
  let j = ref 0 in
  for i = 0 to Array.length lits - 1 do
    match val_of_lit st lits.(i) with
      True    -> is_true := true
    | False   -> ()
    | Unknown -> lits.(!j) <- lits.(i); incr j
  done;
  let lits = Array.sub lits 0 !j in
  if not !is_true then
  match Array.length lits with
    0 -> assert false
  | 1 -> add_un_rule st all_lits lits.(0) reasons
  | 2 -> add_bin_rule st all_lits lits.(0) lits.(1) reasons
  | _ -> let rule = { lits = lits; all_lits = all_lits; reasons = reasons } in
         let p = lit_neg rule.lits.(0) in let p' = lit_neg rule.lits.(1) in
         assert (val_of_lit st p <> False);
         assert (val_of_lit st p' <> False);
         st.st_watched.(p) <- rule :: st.st_watched.(p);
         st.st_watched.(p') <- rule :: st.st_watched.(p')

let associate_vars st lit l =
  st.st_associated_vars.(lit) <- l @ st.st_associated_vars.(lit)

let rec collect_rec st x l =
  if st.st_seen_var.(x) = st.st_seen then l else begin
    st.st_seen_var.(x) <- st.st_seen;
    match st.st_reason.(x) with
      None ->
        l
    | Some r ->
        r.reasons @
        Array.fold_left
          (fun l p -> collect_rec st (var_of_lit p) l) l r.all_lits
  end

let collect_reasons st x =
  st.st_seen <- st.st_seen + 1;
  collect_rec st x []

let collect_reasons_lst st l =
  st.st_seen <- st.st_seen + 1;
  let x = List.find (fun x -> st.st_assign.(x) = False) l in
  collect_rec st x []

let assignment st = st.st_assign

end
