
Set Implicit Arguments.
Unset Strict Implicit.

(****)

Definition is_transitive (A : Type) (r : A -> A -> Prop) :=
  forall x y z : A, r x y -> r y z -> r x z.

Definition is_reflexive (A : Type) (r : A -> A -> Prop) :=
  forall x : A, r x x.

Definition is_symmetric (A : Type) (r : A -> A -> Prop) :=
  forall x y : A, r x y -> r y x.

(****)

Record po : Type :=
  { po_carrier :> Type;
    sub : po_carrier -> po_carrier -> Prop;
    sub_transitive : is_transitive sub;
    sub_reflexive : is_reflexive sub }.

Section equivalence.

Variable A : po.

Definition equiv (x y : A) := sub x y /\ sub y x.

(* Antisymmetry *)
Lemma equiv_intro : forall x y : A, sub x y -> sub y x -> equiv x y.
intros; split; trivial.
Qed.

Lemma equiv_elim_1 : forall x y : A, equiv x y -> sub x y.
intros x y (H, H'); trivial.
Qed.

Lemma equiv_elim_2 : forall x y : A, equiv x y -> sub y x.
intros x y (H, H'); trivial.
Qed.

Lemma equiv_transitive : is_transitive equiv.
intros x y z (H1, H2) (H3, H4); split;
  [ exact (sub_transitive H1 H3) | exact (sub_transitive H4 H2) ].
Qed.

Lemma equiv_reflexive : is_reflexive equiv.
intros x; split; apply sub_reflexive.
Qed.

Lemma equiv_symmetric : is_symmetric equiv.
intros x y (H1, H2); split; trivial.
Qed.

End equivalence.

Opaque equiv.

(****)

Lemma opposite_transitive :
  forall (A : Type) (r : A -> A -> Prop),
  is_transitive r -> is_transitive (fun x y => r y x).
unfold is_transitive in |- *; eauto.
Qed.

Lemma opposite_reflexive :
  forall (A : Type) (r : A -> A -> Prop),
  is_reflexive r -> is_reflexive (fun x y => r y x).
trivial.
Qed.

Section opposite.

Variable A : po.

Definition opposite_po :=
  Build_po (opposite_transitive (sub_transitive (p:=A)))
    (opposite_reflexive (sub_reflexive (p:=A))).

Lemma equiv_opp_intro :
 forall x y : A, equiv x y -> equiv (A:=opposite_po) x y.
intros x y H; apply equiv_intro;
  [ apply equiv_elim_2; trivial | apply equiv_elim_1; trivial ].
Qed.

Lemma equiv_opp_elim :
 forall x y : A, equiv (A:=opposite_po) x y -> equiv x y.
intros x y H; apply equiv_intro;
  [ apply equiv_elim_2; trivial | apply equiv_elim_1; trivial ].
Qed.

End opposite.

(****)

Definition is_monotone (A B : po) (f : A -> B) :=
  forall x y : A, sub x y -> sub (f x) (f y).

Record monotone_fun (A B : po) : Type :=
  { m_fun :> A -> B;
    monotone : is_monotone m_fun }.

Definition monotone_sub_def (A B : po) (f g : monotone_fun A B) :=
  forall x : A, sub (f x) (g x).

Lemma monotone_sub_refl :
  forall A B : po, is_reflexive (monotone_sub_def (A:=A) (B:=B)).
intros A B f x; apply sub_reflexive.
Qed.

Lemma monotone_sub_trans :
  forall A B : po, is_transitive (monotone_sub_def (A:=A) (B:=B)).
intros A B f g h H1 H2 x; exact (sub_transitive (H1 x) (H2 x)).
Qed.

Definition monotone_fun_po (A B : po) :=
  Build_po (monotone_sub_trans (A:=A) (B:=B))
    (monotone_sub_refl (A:=A) (B:=B)).

Lemma monotone_eq :
  forall (A B : po) (f : monotone_fun A B) (x y : A),
  equiv x y -> equiv (f x) (f y).
intros A B f x y H; apply equiv_intro;
  [ apply monotone; exact (equiv_elim_1 H)
  | apply monotone; exact (equiv_elim_2 H) ].
Qed.

Let compose_monotone :
  forall (A B C : po) (f : monotone_fun A B)
    (g : monotone_fun B C), is_monotone (fun x => g (f x)).
unfold is_monotone in |- *; intros; apply monotone; apply monotone; trivial.
Qed.

Definition compose (A B C : po) (f : monotone_fun A B)
  (g : monotone_fun B C) := Build_monotone_fun (compose_monotone f g).

Let opp_monotone :
  forall (A B : po) (f : A -> B),
  is_monotone f -> is_monotone (A:=opposite_po A) (B:=opposite_po B) f.
unfold is_monotone in |- *; simpl in |- *; eauto.
Qed.

Definition opposite_monotone (A B : po) (f : monotone_fun A B) :=
  Build_monotone_fun (opp_monotone (monotone f)).

Lemma constant_implies_monotone :
  forall (A B : po) (y : B), is_monotone (fun x : A => y).
intros A B y x x' H; apply sub_reflexive.
Qed.

Definition constant_monotone (A B : po) (y : B) :=
  Build_monotone_fun (@constant_implies_monotone A B y).

(****)

Definition is_fixpoint (A : po) (f : A -> A) (a : A) := equiv a (f a).
Definition is_upper_bound (A : po) (P : A -> Prop) (x : A) :=
  forall y : A, P y -> sub y x.

Definition postfixpoint (A : po) (f : A -> A) (x : A) := sub x (f x).
Definition prefixpoint (A : po) (f : A -> A) (x : A) := sub (f x) x.

(****)

Definition sub_product (A B : po) (x y : prodT A B) :=
  sub (fstT x) (fstT y) /\ sub (sndT x) (sndT y).

Lemma sub_product_transitive :
 forall A B : po, is_transitive (sub_product (A:=A) (B:=B)).
intros A B x y z (H1, H1') (H2, H2'); split;
 [ exact (sub_transitive H1 H2) | exact (sub_transitive H1' H2') ].
Qed.

Lemma sub_product_reflexive :
 forall A B : po, is_reflexive (sub_product (A:=A) (B:=B)).
intros A B x; split; apply sub_reflexive.
Qed.

Definition product_po (A B : po) :=
  Build_po (sub_product_transitive (A:=A) (B:=B))
    (sub_product_reflexive (A:=A) (B:=B)).

(****)

Definition pwe_lift (A : Type) (B : po) (r : B -> B -> Prop) (f g : A -> B) :=
  forall x : A, r (f x) (g x).

Definition sub_pwe (A : Type) (B : po) (f g : A -> B) :=
  forall x : A, sub (f x) (g x).

Lemma sub_pwe_transitive :
  forall (A : Type) (B : po), is_transitive (sub_pwe (A:=A) (B:=B)).
intros A B f g h H1 H2 x; apply sub_transitive with (1 := H1 x); apply H2.
Qed.

Lemma sub_pwe_reflexive :
  forall (A : Type) (B : po), is_reflexive (sub_pwe (A:=A) (B:=B)).
intros A B f x; apply sub_reflexive.
Qed.

Definition pwe_po (A : Type) (B : po) :=
  Build_po (sub_pwe_transitive (A:=A) (B:=B))
           (sub_pwe_reflexive (A:=A) (B:=B)).
