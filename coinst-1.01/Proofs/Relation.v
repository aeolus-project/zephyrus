
(**** Lattice of relations, of sets ****)

Set Implicit Arguments.
Unset Strict Implicit.

Require Import Lattice.

(****)

Inductive tuple : Type :=
  | t_empty : tuple
  | t_cons : Type -> tuple -> tuple.

Fixpoint tuple_elem (t : tuple) : Type :=
  match t with
  | t_empty => True
  | t_cons t r => prodT t (tuple_elem r)
  end.

Fixpoint nary_fun (A : Type) (t : tuple) {struct t} : Type :=
  match t with
  | t_empty => A
  | t_cons t r => t -> nary_fun A r
  end.

Fixpoint curry (A : Type) (t : tuple) {struct t} :
 nary_fun A t -> tuple_elem t -> A :=
  match t return (nary_fun A t -> tuple_elem t -> A) with
  | t_empty => fun f _ => f
  | t_cons t rem => fun f p => let (x, p') := p in curry (f x) p'
  end.

Definition uncurry (A : Type) (t : tuple) :
  (tuple_elem t -> A) -> nary_fun A t.
induction t;
 [ intros f; exact (f I)
 | intros f x; apply IHt; intro p; apply f; split; [ exact x | exact p ] ].
Defined.

Definition rel := nary_fun Prop.

Fixpoint rel_sub (t : tuple) : rel t -> rel t -> Prop :=
  match t return (rel t -> rel t -> Prop) with
  | t_empty => fun r r' => r -> r'
  | t_cons t rem => fun r r' => forall x : t, rel_sub (r x) (r' x)
  end.

Lemma rel_sub_transitive : forall t : tuple, is_transitive (rel_sub (t:=t)).
red in |- *; induction t; simpl in |- *; eauto.
Qed.

Lemma rel_sub_reflexive : forall t : tuple, is_reflexive (rel_sub (t:=t)).
red in |- *; induction t; simpl in |- *; auto.
Qed.

Definition rel_po (t : tuple) :=
  Build_po (rel_sub_transitive (t:=t)) (rel_sub_reflexive (t:=t)).

Definition rel_union (t : tuple) (s : rel_po t -> Prop) :
  rel_po t := uncurry (fun p => exists r : _, s r /\ curry r p).

Definition rel_inter (t : tuple) (s : rel_po t -> Prop) :
  rel_po t := uncurry (fun p => forall r : rel_po t, s r -> curry r p).

Lemma curry_monotone :
  forall t : tuple,
  @is_monotone (rel_po t) (rel_po (t_cons (tuple_elem t) t_empty))
     (@curry Prop t).
red in |- *; induction t; simpl in |- *;
  [ trivial | intros r r' H (x, p); apply IHt; exact (H x) ].
Qed.

Lemma uncurry_monotone :
  forall t : tuple,
  @is_monotone (rel_po (t_cons (tuple_elem t) t_empty)) (rel_po t)
     (@uncurry Prop t).
red in |- *; induction t; simpl in |- *;
  [ auto | intros r r' H x; apply IHt; intro p; apply H ].
Qed.

Lemma uncurry_curry_prop :
 forall (t : tuple) (r : rel_po t), equiv r (uncurry (curry r)).
intros t r; apply equiv_intro;
 [ induction t;
    [ simpl in |- *; trivial
    | intro x; apply rel_sub_transitive with (1 := IHt (r x)); simpl in |- *;
       apply uncurry_monotone; simpl in |- *; trivial ]
 | induction t;
    [ simpl in |- *; trivial
    | intro x; apply rel_sub_transitive with (2 := IHt (r x)); simpl in |- *;
       apply uncurry_monotone; simpl in |- *; trivial ] ].
Qed.

Lemma rel_union_sup : forall t : tuple, least_upper_bound (rel_po t).
intro t;
apply Build_least_upper_bound
with (A := rel_po t) (lub_def := rel_union (t:=t));
 [ intros P r H; simpl in |- *;
    apply rel_sub_transitive with (1 := proj1 (uncurry_curry_prop r));
    unfold rel_union in |- *; apply uncurry_monotone;
    intros x H'; exists r; split; trivial
 | intros P r H;
    apply
     (sub_transitive (p:=rel_po t)) with (2 := proj2 (uncurry_curry_prop r));
    unfold rel_union in |- *; apply uncurry_monotone;
    intros x (r', (H1, H2)); generalize x H2; clear x H2;
    apply curry_monotone; exact (H _ H1) ].
Defined.

Definition relation (t : tuple) := Build_lattice_2 (rel_union_sup t).

(****)

Definition set (A : Type) := relation (t_cons A t_empty).

Definition singleton (A : Type) (x : A) : set A := fun y => y = x.

Definition inter (A : Type) (t t' : set A) : set A := fun v => t v /\ t' v.

Lemma inter_lower_bound_1 :
  forall (A : Type) (t t' : set A), sub (inter t t') t.
intros A t t' x (H1, H2); trivial.
Qed.

Lemma inter_lower_bound_2 :
  forall (A : Type) (t t' : set A), sub (inter t t') t'.
intros A t t' x (H1, H2); trivial.
Qed.

Lemma inter_greatest :
  forall (A : Type) (t t' t'': set A),
  sub t'' t -> sub t'' t' -> sub t'' (inter t t').
intros A t t' t'' H1 H2 x H3; split;
  [ apply H1; trivial | apply H2; trivial ].
Qed.

Lemma sub_inter_split :
  forall A (t t' u u' : set A),
  sub t t' -> sub u u' -> sub (inter t u) (inter t' u').
intros A t t' u u' H1 H2; apply inter_greatest;
  [ apply (@sub_transitive (set A)) with (2 := H1);apply inter_lower_bound_1
  | apply (@sub_transitive (set A)) with (2 := H2);apply inter_lower_bound_2 ].
Qed.

Lemma monotony_and_inter :
  forall A B (f : monotone_fun (set A) (set B)) x x',
  sub (f (inter x x')) (inter (f x) (f x')).
intros A B f x x'; apply inter_greatest; apply monotone;
 [ apply inter_lower_bound_1
 | apply inter_lower_bound_2 ].
Qed.

Definition union (A : Type) (t t' : set A) : set A := fun v => t v \/ t' v.

Lemma union_upper_bound_1 :
  forall (A : Type) (t t' : set A), sub t (union t t').
intros A t t' x H1; left; trivial.
Qed.

Lemma union_upper_bound_2 :
  forall (A : Type) (t t' : set A), sub t' (union t t').
intros A t t' x H1; right; trivial.
Qed.

Lemma union_least :
  forall (A : Type) (t t' t'': set A),
  sub t t'' -> sub t' t'' -> sub (union t t') t''.
intros A t t' t'' H1 H2 x [H3 | H3];
  [ apply H1; trivial | apply H2; trivial ].
Qed.
