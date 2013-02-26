
Set Implicit Arguments.
Unset Strict Implicit.

Require Export Po.

(****)

Record least_upper_bound (A : po) : Type :=
  { lub_def :> (A -> Prop) -> A;
    lub_upper_bound : forall P : A -> Prop, is_upper_bound P (lub_def P);
    lub_least :
      forall (P : A -> Prop) (x : A),
      is_upper_bound P x -> sub (lub_def P) x }.

Record lattice : Type :=
  { l_carrier :> po;
    sup : least_upper_bound l_carrier;
    inf : least_upper_bound (opposite_po l_carrier) }.

Definition sup_upper_bound (A : lattice) := lub_upper_bound (sup A).
Definition sup_least (A : lattice) := lub_least (sup A).

Definition inf_lower_bound (A : lattice) := lub_upper_bound (inf A).
Definition inf_greatest (A : lattice) := lub_least (inf A).

Implicit Arguments sup [l].
Implicit Arguments inf [l].

Definition opp_opp (A : po) (lub : least_upper_bound A) :=
  Build_least_upper_bound (A:=opposite_po (opposite_po A))
    (lub_upper_bound lub) (lub_least lub).

Definition opposite_lattice (A : lattice) :=
  Build_lattice (inf (l:=A)) (opp_opp (sup (l:=A))).

(****)

Definition bottom (A : lattice) := sup (fun x : A => False).

Lemma bottom_prop : forall (A : lattice) (x : A), sub (bottom A) x.
intros A x; unfold bottom in |- *; apply sup_least; red in |- *;
 contradiction.
Qed.

Definition top (A : lattice) : A := bottom (opposite_lattice A).

Lemma top_prop : forall (A : lattice) (x : A), sub x (top A).
exact (fun A => bottom_prop (A:=opposite_lattice A)).
Qed.

Section binary_sup.

Variable A : lattice.

Definition sup2 (x y : A) := sup (fun z => z = x \/ z = y).

Lemma sup2_upper_bound_1 : forall x y : A, sub x (sup2 x y).
intros x y; unfold sup2 in |- *; apply sup_upper_bound; auto.
Qed.

Lemma sup2_upper_bound_2 : forall x y : A, sub y (sup2 x y).
intros x y; unfold sup2 in |- *; apply sup_upper_bound; auto.
Qed.

Lemma sup2_least : forall x y z : A, sub x z -> sub y z -> sub (sup2 x y) z.
intros x y z H1 H2; unfold sup2 in |- *; apply sup_least; intros t [E| E];
 rewrite E; clear t E; trivial.
Qed.

End binary_sup.

Section binary_inf.

Variable A : lattice.

Definition inf2 (x y : A) : A := sup2 (A:=opposite_lattice A) x y.

Lemma inf2_lower_bound_1 : forall x y : A, sub (inf2 x y) x.
exact (sup2_upper_bound_1 (A:=opposite_lattice A)).
Qed.

Lemma inf2_lower_bound_2 : forall x y : A, sub (inf2 x y) y.
exact (sup2_upper_bound_2 (A:=opposite_lattice A)).
Qed.

Lemma inf2_greatest :
 forall x y z : A, sub z x -> sub z y -> sub z (inf2 x y).
exact (sup2_least (A:=opposite_lattice A)).
Qed.

End binary_inf.

(****)

Definition inf_of_sup (A : po) (sup : (A -> Prop) -> A)
  (P : A -> Prop) := sup (fun x => forall y : A, P y -> sub x y).

Definition inf_of_sup_inf :
  forall A : po,
  least_upper_bound A -> least_upper_bound (opposite_po A).
intros A H; case H; clear H; intros sp prop1 prop2;
 apply
  Build_least_upper_bound
   with (A := opposite_po A) (lub_def := inf_of_sup sp);
 [ intros P y H; simpl in |- *; unfold inf_of_sup in |- *; apply prop2;
    red in |- *; auto
 | simpl in |- *; unfold inf_of_sup in |- *; intros P x H; apply prop1; auto ].
Defined.

Definition Build_lattice_2 (A : po) (sup : least_upper_bound A) :=
  Build_lattice sup (inf_of_sup_inf sup).

(****)

Definition lfp (A : lattice) (f : monotone_fun A A) : A :=
  inf (fun x => sub (f x) x).

Theorem induction :
  forall (A : lattice) (f : monotone_fun A A) (x : A),
  sub (f x) x -> sub (lfp f) x.
intros A f x H; unfold lfp in |- *; apply inf_lower_bound; trivial.
Qed.

Theorem lfp_fold :
  forall (A : lattice) (f : monotone_fun A A), sub (f (lfp f)) (lfp f).
intros A f; unfold lfp in |- *; apply inf_greatest; simpl in |- *; intros x H;
 simpl in |- *; apply sub_transitive with (y := f x);
 [ apply monotone; apply inf_lower_bound; trivial | trivial ].
Qed.

Theorem lfp_unfold :
  forall (A : lattice) (f : monotone_fun A A), sub (lfp f) (f (lfp f)).
intros A f; apply induction; apply monotone; apply lfp_fold.
Qed.

Theorem strong_induction :
  forall (A : lattice) (f : monotone_fun A A) (x : A),
  sub (f (inf2 x (lfp f))) x -> sub (lfp f) x.
intros A f x H; assert (H' : sub (lfp f) (inf2 x (lfp f)));
 [ apply induction; apply inf2_greatest;
    [ trivial
    | apply sub_transitive with (2 := lfp_fold f); apply monotone;
       apply inf2_lower_bound_2 ]
 | apply sub_transitive with (1 := H'); apply inf2_lower_bound_1 ].
Qed.

(****)

Definition gfp (A : lattice) (f : monotone_fun A A) : A :=
  lfp (A:=opposite_lattice A) (opposite_monotone f).

Theorem coinduction :
 forall (A : lattice) (f : monotone_fun A A) (x : A),
 sub x (f x) -> sub x (gfp f).
intros A f;
 exact (induction (A:=opposite_lattice A) (f:=opposite_monotone f)).
Qed.

Theorem gfp_fold :
 forall (A : lattice) (f : monotone_fun A A), sub (f (gfp f)) (gfp f).
intros A f; exact (lfp_unfold (A:=opposite_lattice A) (opposite_monotone f)).
Qed.

Theorem gfp_unfold :
 forall (A : lattice) (f : monotone_fun A A), sub (gfp f) (f (gfp f)).
intros A f; exact (lfp_fold (A:=opposite_lattice A) (opposite_monotone f)).
Qed.

Theorem strong_coinduction :
 forall (A : lattice) (f : monotone_fun A A) (x : A),
 sub x (f (sup2 x (gfp f))) -> sub x (gfp f).
intros A f;
 exact (strong_induction (A:=opposite_lattice A) (f:=opposite_monotone f)).
Qed.

(****)

Definition sup_product (A B : lattice) (P : product_po A B -> Prop) :=
  pairT (sup (fun x => exists y : _, P (pairT x y)))
    (sup (fun y => exists x : _, P (pairT x y))).

Definition sup_product_prop :
  forall A B : lattice, least_upper_bound (product_po A B).
intros A B;
 apply
  Build_least_upper_bound
   with (A := product_po A B) (lub_def := sup_product (A:=A) (B:=B));
 [ red in |- *; simpl in |- *; intros P y H; split;
    [ simpl in |- *; apply sup_upper_bound; exists (sndT y); induction y;
       trivial
    | simpl in |- *; apply sup_upper_bound; exists (fstT y); induction y;
       trivial ]
 | simpl in |- *; intros P x H; split;
    [ simpl in |- *; apply sup_least; intros y (y', H');
       exact (proj1 (H _ H'))
    | simpl in |- *; apply sup_least; intros y (y', H');
       exact (proj2 (H _ H')) ] ].
Defined.

Definition prod_opp (A B : po)
  (lub : least_upper_bound
           (product_po (opposite_po A) (opposite_po B))) :=
  Build_least_upper_bound (A:=opposite_po (product_po A B))
    (lub_upper_bound lub) (lub_least lub).

Definition product_lattice (A B : lattice) :=
  Build_lattice (sup_product_prop A B)
    (prod_opp (sup_product_prop (opposite_lattice A) (opposite_lattice B))).

Lemma fstT_monotone :
 forall A B : lattice,
 is_monotone (A:=product_lattice A B) (B:=A) (fstT (A:=A) (B:=B)).
intros A B x y (H, H'); trivial.
Qed.

Lemma sndT_monotone :
 forall A B : lattice,
 is_monotone (A:=product_lattice A B) (B:=B) (sndT (A:=A) (B:=B)).
intros A1 B1 x y (H, H'); trivial.
Qed.

(****)

Definition sup_pwe (A : Type) (B : lattice) (P : pwe_po A B -> Prop) x :=
  sup (fun y => exists f : _, P f /\ y = f x).

Definition sup_pwe_prop :
  forall (A : Type) (B : lattice), least_upper_bound (pwe_po A B).
intros A B;
 apply
  Build_least_upper_bound
   with (A := pwe_po A B) (lub_def := sup_pwe (A:=A) (B:=B));
 [ intros P f H x; unfold sup_pwe in |- *; apply sup_upper_bound; exists f;
    split; trivial
 | intros P f H x; unfold sup_pwe in |- *; apply sup_least;
    intros y (g, (H', H'')); rewrite H''; clear y H'';
    apply (H _ H') ].
Defined.

Definition pwe_opp (A : Type) (B : po)
  (lub : least_upper_bound (pwe_po A (opposite_po B))) :
  least_upper_bound (opposite_po (pwe_po A B)) :=
  match lub with
  | Build_least_upper_bound sp prop1 prop2 =>
      Build_least_upper_bound (A:=opposite_po (pwe_po A B)) prop1 prop2
  end.

Definition pwe_lattice (A : Type) (B : lattice) :=
  Build_lattice (sup_pwe_prop A B)
    (pwe_opp (sup_pwe_prop A (opposite_lattice B))).

Lemma pwe_bottom_prop :
 forall (A : Type) (B : lattice) (x : A),
 equiv (bottom (pwe_lattice A B) x) (bottom B).
intros A B x; apply equiv_intro;
 [ exact (bottom_prop (A:=pwe_lattice A B) (fun x => bottom B) x)
 | apply bottom_prop ].
Qed.

Lemma pwe_top_prop :
 forall (A : Type) (B : lattice) (x : A),
 equiv (top (pwe_lattice A B) x) (top B).
intros A B x; apply equiv_intro;
 [ apply top_prop
 | exact (top_prop (A:=pwe_lattice A B) (fun x => top B) x) ].
Qed.

(****)

Definition image (A B : Type) (f : A -> B) (P : A -> Prop) y :=
  exists x : A, P x /\ y = f x.

Lemma image_prop_1 :
  forall (A B : Type) (f : A -> B) (P : A -> Prop) x, P x -> image f P (f x).
intros A B f P x H1; exists x; split; trivial.
Qed.

Lemma monotony_and_sup :
  forall (A B : lattice) (f : monotone_fun A B) (P : A -> Prop),
  sub (sup (image f P)) (f (sup P)).
intros A B f P; apply sup_least; intros y (x, (H1, E)); subst;
apply monotone; apply sup_upper_bound; trivial.
Qed.

Lemma monotony_and_inf :
  forall (A B : lattice) (f : monotone_fun A B) (P : A -> Prop),
  sub (f (inf P)) (inf (image f P)).
intros A B f P; apply inf_greatest; intros y (x, (H1, E)); subst;
change (sub (f (inf P)) (f x)); apply monotone;
apply inf_lower_bound; trivial.
Qed.
