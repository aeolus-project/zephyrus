
Set Implicit Arguments.
Unset Strict Implicit.

Require Import Po.
Require Import Relation.
Require Import Lattice.
Require Import List.

Require Import Classical.

Axiom choice :
 forall (A : Type) (B : A -> Type) (R : forall x : A, B x -> Prop),
   (forall x:A,  exists y : B x, R x y) ->
    exists f : forall x : A, B x, (forall x:A, R x (f x)).

(****)

Definition is_injective A B (r : A -> B -> Prop) :=
  forall x y z, r x y -> r x z -> y = z.

Definition reverse A B (r : A -> B -> Prop) x y := r y x.

Definition minimal (A : po) (P : A -> Prop) (x : A) :=
  P x /\ forall y, P y -> sub y x -> sub x y.

Definition least (A : po) (P : A -> Prop) (x : A) :=
  P x /\ forall y, P y -> sub x y.

Lemma least_implies_minimal :
  forall (A : po) (P : A -> Prop) x, least P x -> minimal P x.
intros A P X (H1, H2); split; auto.
Qed.

Definition maximal (A : po) (P : A -> Prop) (x : A) :=
  P x /\ forall y, P y -> sub x y -> sub y x.

(* f(s) = {f(q) | q in s} *)

Definition img (A B : Type) (f : A -> B) (s : set A) : set B :=
  fun p => exists q, s q /\ p = f q.

(*
   {p | forall q, p = f(q) => q in s},
   complement of the image of the complement
*)

Definition complimg (A B : Type) (f : A -> B) (s : set A) : set B :=
  fun p => forall q, p = f q -> s q.

(* f^-1(s) = {p | exists q in s, q = f(p)} *)

Definition preimg (A B : Type) (f : A -> B) (s : set B) : set A :=
  fun p => exists q, s q /\ q = f p.

(*************************************************************)

(*
   We first define repositories (Section 2.1)
   We assume given a fixed set of packages.
*)

Variable package : Set.

(*
  The two axioms corresponds to the assumption that the set of
  packages is finite.
*)

Axiom package_set_maximality :
  forall (P : set package -> Prop) s,
  P s -> exists s', maximal P s' /\ sub s s'.

Axiom package_set_minimality :
  forall (P : set package -> Prop) s,
  P s -> exists s', minimal P s' /\ sub s' s.

(*
  The dependency function and the conflict relation.
  We do not assume here that the conflict relation is symmetric and
  irreflexive. Instead, we systematically use the [in_conflict]
  function define below to symmetrise the relation, and we explicitly
  assume that the relation is irreflexive ([no_self_conflict]
  predicate) when needed.
*)

Definition confl_rel := package -> set package.
Definition dep_fun := package -> set (set package).

(*
   A repository is composed of the set of packages defined above
   together with the dependency and conflict relations below.
*)

Record constraints : Type :=
  { depends : dep_fun;
    conflicts : confl_rel }.

(*
   We define healthy installations (Section 2.1), and then installable
   packages and co-installable set of packages (Section 2.2).
*)

Section Installation.

Variable c : constraints.

Definition in_conflict p q := conflicts c p q \/ conflicts c q p.

Definition has_conflict p := exists q, in_conflict p q.

Definition no_self_conflict c := forall p, ~ conflicts c p p.

Definition dep_satisfied (i d : set package) := exists p, i p /\ d p.

Record healthy (i : set package) : Prop :=
  { abundance : forall p d, i p -> depends c p d -> dep_satisfied i d;
    peace : forall p p', i p -> i p' -> conflicts c p p' -> False }.

Definition installable p := exists i, healthy i /\ i p.

Definition co_installable s := exists i, healthy i /\ sub s i.

End Installation.

(*************************************************************)

(*
   We define a preorder on repositories (Section 4)
*)

Definition dsub (ds ds' : dep_fun) :=
  forall p, forall d, ds p d -> exists d', sub d' d /\ ds' p d'.

Remark dsub_trans : is_transitive dsub.
intros ds ds' ds'' H1 H2 p s H3;
generalize (H1 _ _ H3); intros (s', (H4, H5));
generalize (H2 _ _ H5); intros (s'', (H6, H7));
exists s''; split; trivial;
exact (sub_transitive H6 H4).
Qed.

Remark dsub_refl : is_reflexive dsub.
intros ds p s H1; exists s; split; trivial; apply sub_reflexive.
Qed.

Definition dep_fun_po := Build_po dsub_trans dsub_refl.

Canonical Structure dep_fun_po.

Definition csub c c' :=
  sub (depends c) (depends c')
    /\
  (forall p, sub (conflicts c p) (conflicts c' p)).

Remark csub_trans : is_transitive csub.
intros c c' c'' (H1, H2) (H3, H4); split;
  [ exact (sub_transitive H1 H3)
  | intro p; exact (sub_transitive (H2 p) (H4 p)) ].
Qed.

Remark csub_refl : is_reflexive csub.
intros c; split; [ apply sub_reflexive | intro p; apply sub_reflexive ].
Qed.

Definition constraints_po := Build_po csub_trans csub_refl.

Canonical Structure constraints_po.

(* The preorder is coarser than point-wise inclusion (Remark 23). *)

Remark dsub_incl :
  forall ds ds' : dep_fun, (forall p, sub (ds p) (ds' p)) -> sub ds ds'.
intros ds ds' H1 p d H2; exists d; split;
  [ apply sub_reflexive
  | apply H1; trivial ].
Qed.

(* Theorem 2 and an immediate corollary (remark 22). *)

Theorem constraint_weakening :
  forall c c' i, sub c c' -> healthy c' i -> healthy c i.
intros c c' i (H1, H2) (H3, H4); split; trivial;
  [ intros p d H5 H6; generalize (H1 _ _ H6); intros (d', (H7, H8));
    generalize (H3 _ _ H5 H8); intros (q, (G1, G2)); exists q;
    split; trivial; apply H7; trivial
  | intros p p' H5 H6 H7; generalize (H2 _ _ H7); eauto ].
Qed.

Remark constraint_weakening_and_co_installability :
  forall c c' s, sub c c' -> co_installable c' s -> co_installable c s.
intros c c' s H1 (i, (H2, H3)); exists i; split; trivial;
exact (constraint_weakening H1 H2); trivial.
Qed.

(*************************************************************)

(* We now define the flattening operation (Section 5). *)

Section Flattening.

Variable c : constraints.

(*
  These three definitions implement rules Refl and Trans.

  We first define derivation trees build from these rules.
  As we must be able to reason by induction on these derivations,
  they are of sort [Type].

  Then, given such a derivation, we can read a dependency.

  Finally, the resulting dependency function is build by collecting
  all these dependencies.
*)

Inductive below_conflicts p : Type :=
    immediate_conflict :
      has_conflict c p -> below_conflicts p
  | higher_conflicts :
      forall d,
      depends c p d -> (forall q, d q -> below_conflicts q) ->
      below_conflicts p.

Fixpoint flattened_dep p (H : below_conflicts p) : set package :=
  fun q =>
  match H with
    immediate_conflict _ =>
      q = p
  | higher_conflicts d H1 H2 =>
      exists p, exists H3 : d p, flattened_dep (H2 _ H3) q
  end.

Definition flattened_dependencies : package -> set (set package) :=
  fun p => fun s => exists H : below_conflicts p, s = flattened_dep H.

(*
   This is the repository after flattening.
   Only the dependency function is changed.
*)

Definition flattened :=
  Build_constraints flattened_dependencies (conflicts c).

End Flattening.

(*************************************************************)

(* Strongly flat repositories (Section 6). *)

(* Definition of \Delta_C. *)

Definition confl_deps c : dep_fun :=
  fun p d => exists p', in_conflict c p p' /\ d = singleton p.

(* Definition of the dependency function composition D;D. *)

Definition dep_fun_compose (ds ds' : dep_fun) : dep_fun :=
  fun p d' =>
  exists d,
  ds p d /\
  exists f, (forall q (H : d q), ds' q (f q H)) /\
  d' = (fun p => exists q, exists H : d q, f q H p).

(* Definition of strongly flat repositories *)

Record strongly_flat c :=
  { sf_refl : sub (confl_deps c) (depends c);
    sf_trans : sub (dep_fun_compose (depends c) (depends c)) (depends c) }.

(*
   This lemma shows that we can normalize the dependencies (see
   Section 4) without breaking the property.
*)

Lemma strongly_flat_preserved :
   forall c c', equiv c c' -> strongly_flat c -> strongly_flat c'.
intros c c' ((H1, H2), (H3, H4)) (H5, H6); split;
  [ refine (sub_transitive _ (sub_transitive H5 H1));
    intros p d H7; exists d; generalize H7; clear H7;
    intros (q ,(H7, H8));
    split; try apply sub_reflexive;
    exists q; split; trivial;
    case H7; intro H9; [ left | right]; apply H4; trivial
  | intros p d (d', (H7, (f, (H8, E)))); subst d;
    generalize (H3 _ _ H7); intros (d, (G1, G2));
    generalize (choice (fun qH => H3 _ _ (H8 (proj1_sig qH) (proj2_sig qH))));
    intros (f', G3);
    pose
      (d'' := fun p' => exists q, exists H : d q, f' (exist d' q (G1 q H)) p');
    case (H6 p d'');
      [ exists d; split; trivial;
        exists (fun q H => f' (exist _ q (G1 _ H))); split;
          [ intros q G4;
            generalize (G3 (exist  _ q (G1 _ G4))); intros (G5, G6); trivial
          | trivial ]
      | subst d''; intros d'' (G4, G5);
        generalize (H1 _ _ G5); intros (d''', (G6, G7));
        exists d'''; split; trivial;
        refine (sub_transitive G6 _);
        refine (sub_transitive G4 _);
        intros p' (q, (G8, G9)); exists q; exists (G1 _ G8);
        apply (proj1 (G3 (exist _ q (G1 _ G8)))); trivial ] ].
Qed.

(* We know prove Theorem 3.  We use two intermediate lemmas for that. *)

Remark flattened_reflexive :
   forall c, sub (confl_deps (flattened c)) (depends (flattened c)).
intros c p d (p', (H2, H3)); subst d;
assert (H1 : has_conflict c p);
  [ exists p'; trivial
  | exists (flattened_dep (immediate_conflict H1)); simpl; split; trivial;
    exists (immediate_conflict H1); trivial ].
Qed.

Remark flattened_and_composition :
   forall c,
   sub (dep_fun_compose (depends (flattened c)) (depends (flattened c)))
       (depends (flattened c)).
intros c p s (d, ((H1, E1), (f, (H2, E)))); subst d s;
cut (exists H : below_conflicts c p,
       forall q, flattened_dep H q -> exists q', exists H', f q' H' q);
  [ intros (H3, H4); exists (flattened_dep H3); split;
      [ intros q H5; exact (H4 _ H5)
      | exists H3; trivial ]
  | induction H1 as [p H1 | p d H1 H3 H4];
      [ generalize  (H2 _ (refl_equal _)); clear H2; intros (H2, E);
        exists H2; intros q H3; exists p; exists (refl_equal p);
        rewrite E; trivial
      | assert (H5 :
                forall q H q', flattened_dep (H3 q H) q' ->
                flattened_dep (higher_conflicts H1 H3) q');
          [ simpl; intros q H q' H5; exists q; exists H; trivial
          | generalize
              (choice (fun qH => H4 (proj1_sig qH) (proj2_sig qH) _
                                   (fun p H => H2 _ (H5 _ _ _ H))));
            intros (g, H6);
            pose (H7 := higher_conflicts H1 (fun q H => g (exist _ q H)));
            exists H7;
            simpl; intros q (q', (H8, H9));
            generalize (H6 (exist d q' H8) _ H9);
            intros (q'', (G1, G2));
            exists q''; exists (H5 _ H8 _ G1);
            trivial ] ] ].
Qed.

Theorem flattened_strongly_flat : forall c, strongly_flat (flattened c).
intro c; split;
  [ apply flattened_reflexive
  | apply flattened_and_composition ].
Qed.

(*
   A configuration is a pair of two sets of packages [i] and [i'].
   We define healthy configurations and weakly co-installable set of
   packages.
   (Section 5.)
*)

Section Configuration.

Variable c : constraints.

Record healthy_config (i i' : set package) : Prop :=
  { abundance_config : forall p d, i p -> depends c p d -> dep_satisfied i' d;
    peace_config : forall p p', i' p -> i' p' -> conflicts c p p' -> False }.

Definition weakly_co_installable s := exists s', healthy_config s s'.

End Configuration.

(*
   This is the counterpart of Theorem 2 for configurations, and an
   immediate corollary (Lemma 32).
*)

Lemma constraint_weakening_config :
  forall c c' i i', sub c c' -> healthy_config c' i i' -> healthy_config c i i'.
intros c c' i i' (H1, H2) (H3, H4); split; trivial;
  [ intros p d H5 H6; generalize (H1 _ _ H6); intros (d', (H7, H8));
    generalize (H3 _ _ H5 H8); intros (q, (G1, G2)); exists q;
    split; trivial; apply H7; trivial
  | intros p p' H5 H6 H7; generalize (H2 _ _ H7); eauto ].
Qed.

Lemma constraint_weakening_config_and_co_installability :
  forall c c' s,
  sub c c' -> weakly_co_installable c' s -> weakly_co_installable c s.
intros c c' s H1 (i, H2); exists i; exact (constraint_weakening_config H1 H2).
Qed.

(* Remark 24 corresponds to these remarks.  *)

Remark healthy_implies_healthy_config :
  forall c i, healthy c i -> healthy_config c i i.
intros c i (H1, H2); split; tauto.
Qed.

Remark healthy_config_implies_healthy :
  forall c i, healthy_config c i i -> healthy_config c i i.
intros c i (H1, H2); split; tauto.
Qed.

Remark healthy_config_antimonotony :
  forall c i1 i2 i, sub i2 i1 -> healthy_config c i1 i -> healthy_config c i2 i.
intros c i1 i2 i H1 (H2, H3); split; trivial;
intros p d H4; apply H2; apply H1; trivial.
Qed.

(* Lemma 25. *)

Lemma co_installable_implies_weakly_co_installable :
  forall c i, co_installable c i -> weakly_co_installable c i.
intros c i (s, (H1, H2)); exists s;
apply healthy_config_antimonotony with (1 := H2);
apply healthy_implies_healthy_config; trivial.
Qed.

(* Technical lemma 26 (used in the proof of Theorems 4 and 10). *)

Lemma dep_on_conflicts_prop :
  forall c p i s,
  sub (confl_deps c) (depends c) -> healthy_config c i s ->
  has_conflict c p -> i p -> s p.
intros c p i s H1 H2 (q, H3) H4;
case (H1 p (singleton p));
  [ exists q; split; trivial
  | clear q H3; intros d (H5, H6);
    generalize (abundance_config H2 H4 H6);
    intros (q, (H7, H8)); rewrite <- (H5 _ H8); trivial ].
Qed.

(* Another small technical lemma. *)

Remark maximal_healthy_config :
  forall c i i'',
  healthy_config c i i'' ->
  exists i', maximal (fun i => healthy_config c i i'') i' /\ sub i i'.
intros c i i'' H1; apply package_set_maximality; trivial.
Qed.

(*
   Lemma 27.
   This lemma specify a condition in which a weakly co-installable set
   of packages in a repository [c'] is co-installable in repository [c].
   It is crucial to prove Theorems 4 and 6.
*)

Lemma weakly_co_installable_implies_co_installable_1 :
  forall c c' i,
  sub (confl_deps c') (depends c') ->
  sub (dep_fun_compose (depends c) (depends c')) (depends c') ->
  (forall p p', conflicts c p p' -> conflicts c' p p') ->
  weakly_co_installable c' i -> co_installable c i.
intros c c' i' H1 H2 H3 (s, H4);
generalize (maximal_healthy_config H4); clear H4;
intros (i, (H4, H5)); exists i; split; trivial;
clear i' H5; split;
  [ intros p d H5 H6;
    assert (H7 : exists q, d q /\
      forall d', depends c' q d' -> exists q', s q' /\ d' q');
      [ assert (H7 : forall d', depends c' p d' ->
                     exists q', s q' /\ d' q');
          [ intros d' H7; exact (abundance_config (proj1 H4) H5 H7)
          | apply NNPP; intro H8;
            generalize
               (choice (fun pH => not_all_ex_not _ _
                           (fun H' => not_ex_all_not _ _ H8
                                    (proj1_sig pH) (conj (proj2_sig pH) H'))));
            clear H8; intros (f, H8);
            assert (H9 := fun q H => imply_to_and _ _ (H8 (exist _ q H)));
            clear H8;
            pose
              (d'' := fun p' => exists q, exists H : d q, f (exist d q H) p');
            case (H2 p d'');
              [ exists d; split; trivial;
                exists (fun q H => f (exist _ q H)); split; trivial;
                intros q H; exact (proj1 (H9 q H))
              | intros d' (G2, G1);
                generalize (H7 _ G1); intros (q', (G3, G4));
                generalize (G2 _ G4); intros (q, (G5, G6));
                apply (proj2 (H9 _ G5)); exists q'; split; trivial ] ]
      | generalize H7; clear H7; intros (q, (H7, H8));
        exists q; split; trivial; apply NNPP; intro H9;
        apply H9; apply (proj2 H4 (fun p => i p \/ p = q)); simpl; auto;
        clear p d H5 H6 H7; split;
          [ intros p d [G1 | G1] G2;
              [ generalize (abundance_config (proj1 H4) G1 G2);
                intros (p', (G3, G4)); exists p'; auto
              | subst p; exact (H8 _ G2) ]
          | exact (peace_config (proj1 H4)) ] ]
  | intros p p' G1 G2 G3;
    refine
      (peace_config (proj1 H4)
         (dep_on_conflicts_prop H1 (proj1 H4) _ G1)
         (dep_on_conflicts_prop H1 (proj1 H4) _ G2) (H3 _ _ G3));
      [ exists p' | exists p ];
    red; auto ].
Qed.

(* Theorem 4. *)

Lemma weakly_co_installable_implies_co_installable_2 :
  forall c i,
  strongly_flat c ->
  weakly_co_installable c i -> co_installable c i.
intros c i (H1, H2);
apply weakly_co_installable_implies_co_installable_1; trivial.
Qed.

(* Lemma 5. *)

Lemma flattened_least :
  forall c c',
  (forall p, sub (conflicts (flattened c) p) (conflicts c' p)) ->
  sub (confl_deps c') (depends c') ->
  sub (dep_fun_compose (depends c) (depends c')) (depends c') ->
  sub (flattened c) c'.
intros c c' H0 H1 H2; split; trivial;
intros p d (H, E); subst d; induction H as [p (p', H3)| p d H3 H4 H5];
  [ lapply (H1 p (singleton p));
      [ intros (d, (H4, H5)); exists d; auto
      | exists p'; split; trivial;
        case H3; [left | right]; apply H0; trivial ]
  | generalize (choice (fun pH => H5 (proj1_sig pH) (proj2_sig pH)));
    intros (f, H6);
    lapply (H2 p (fun p => exists q, exists H : d q, f (exist _ _ H) p));
      [ intros (d', (H7, H8)); exists d'; split; trivial;
        refine (sub_transitive H7 _);
        simpl; intros q (q', (G1, G2)); exists q'; exists G1;
        exact (proj1 (H6 (exist _ _ G1)) _ G2)
      | exists d; split; trivial;
        exists (fun p H => f (exist _ _ H)); split; trivial;
        intros q H; exact (proj2 (H6 (exist  _ _ H))) ] ].
Qed.

(* A technical lemma used to prove Theorem 6 (case: 2 implies 1). *)

Lemma flattened_and_composition_2 :
   forall c,
   sub (dep_fun_compose (depends c) (depends (flattened c)))
       (depends (flattened c)).
intros c p s (d, (H1, (f, (H2, E)))); subst s;
generalize (choice (fun qH => H2 (proj1_sig qH) (proj2_sig qH)));
intros (g, H3);
pose (H4 := higher_conflicts H1 (fun q H => g (exist _ q H)));
exists (flattened_dep H4);
simpl; split;
  [ intros q (p', (H5, H6)); exists p'; exists H5;
    generalize (H3 (exist _ _ H5));simpl; intro E; rewrite E; trivial
  | exists H4; trivial ].
Qed.

(*
  We prove theorem 6.
  More precisely, we prove that:
  - 1 implies 3 (easy, by induction on each derivation involving rules
    Refl and Trans);
  - 1 implies 2 (redundant);
  - 2 implies 1 (difficult case, by Lemmas 27 and 5);
  - 2 implies 3 (redundant);
  - 3 implies 2 (immediate, by Lemma 25).
*)

Lemma flatten_co_inst_prop_1 :
  forall c s, co_installable c s -> co_installable (flattened c) s.
intros c s (i, ((H1, H2), H3)); exists i; split; trivial; split; trivial;
intros p s' H4 (H5, H6); subst s'; clear s H3;
induction H5;
  [ exists p; simpl; split; trivial
  | generalize (H1 _ _ H4 d0); intros (q, (H7, H8));
    generalize (H q H8 H7); intros (q', (G1, G2));
    exists q'; split; trivial; exists q; exists H8; trivial ].
Qed.

Lemma flatten_co_inst_prop_2 :
  forall c s, co_installable c s -> weakly_co_installable (flattened c) s.
intros c s (i, ((H1, H2), H3)); exists i; split; trivial;
intros p s' H4 (H5, H6); subst s'; assert (H6 := H3 _ H4); clear H4;
induction H5;
  [ exists p; simpl; split; trivial
  | generalize (H1 _ _ H6 d0); intros (q, (H7, H8));
    generalize (H q H8 H7); intros (q', (G1, G2));
    exists q'; split; trivial; exists q; exists H8; trivial ].
Qed.

Lemma flatten_co_inst_prop_3 :
  forall c s, weakly_co_installable (flattened c) s -> co_installable c s.
intros c s; apply weakly_co_installable_implies_co_installable_1; trivial;
  [ apply flattened_reflexive
  | apply flattened_and_composition_2 ].
Qed.

Lemma flatten_co_inst_prop_4 :
  forall c s,
  weakly_co_installable (flattened c) s -> co_installable (flattened c) s.
intros c s;
apply weakly_co_installable_implies_co_installable_2;
apply flattened_strongly_flat.
Qed.

Lemma flatten_co_inst_prop_5 :
  forall c s,
  co_installable (flattened c) s -> weakly_co_installable (flattened c) s.
intros c s; apply co_installable_implies_weakly_co_installable.
Qed.

(*************************************************************)

(* Flat repositories (Section 7). *)

(* Definition 7: \nabla_C. *)

Definition always_sat (c : confl_rel) :=
  fun d =>
    forall i,
    forall c' : confl_rel, (forall p, sub (c' p) (c p)) ->
    maximal
      (fun i : set package =>
         forall p p', i p -> i p' -> c' p p' -> False) i ->
    dep_satisfied i d.

(*
  Dependencies containing a package with no conflict are in
  \nabla_C.
*)

Lemma always_sat_when_no_conflict :
  forall c (d : set package) q,
  d q -> ~ has_conflict c q -> always_sat (conflicts c) d.
intros c d q H1 H2 i c' H3 H4;
pose (i' := fun p' => i p' \/ p' = q);
assert (H6 : forall p p' : package, i' p -> i' p' -> c' p p' -> False);
  [ intros p1 p2 [H6 | H6] [H7 | H7];
      [ apply (proj1 H4 _ _ H6 H7)
      | subst p2; intro H7; apply H2; exists p1; right; apply H3; trivial
      | subst p1; intro H6; apply H2; exists p2; left; apply H3; trivial
      | subst p1 p2; intro H6; apply H2; exists q; left; apply H3; trivial ]
  | lapply (proj2 H4 i' H6);
       [ intro H7; exists q; split; trivial; apply H7; right; trivial
       | intros p H7; red; auto ] ].
Qed.

(*
  We define the dependencies containing a package with only internal
  conflicts.
*)

Definition internal_conflicts c (d : set package) :=
  exists p, d p /\ forall q, in_conflict c p q -> d q.

(* Then, we prove Theorem 8. We prove each direction in turn. *)

Theorem always_sat_implies_internal_conflicts :
  forall c d,
  always_sat (conflicts c) d -> internal_conflicts c d.
intros c d H1; apply NNPP; intro H0;
case
  (H1 (fun p => ~ d p)
      (fun p q => conflicts c p q /\ ((d p /\ ~ d q) \/ (d q /\ ~ d p))));
  [ intros p1 p2 (H3, _); trivial
  | split;
      [ intros p p' H3 H4 (H5, [(H6, H7) | (H6, H7)]);
          [ case (H3 H6)
          | case (H4 H6) ]
      | intros i H3 H4 p H5 H6;
        apply H0; exists p; split; trivial;
        intros q [H8 | H8]; apply NNPP; intro H7;
          [ apply (H3 _ _ H5 (H4 _ H7)); auto
          | apply (H3 _ _ (H4 _ H7) H5); auto ] ]
  | intros q (H3, H4); case (H3 H4) ].
Qed.

Theorem always_sat_when_internal_conflicts :
  forall c d,
  no_self_conflict c -> internal_conflicts c d -> always_sat (conflicts c) d.
intros c d H0 (p, (H1, H2)) i c' H3 H4;
case (classic (forall q, in_conflict c p q -> ~i q));
  [ intro H5; pose (i' := fun p' => i p' \/ p' = p);
    assert (H6 : forall p p' : package, i' p -> i' p' -> c' p p' -> False);
      [ intros p1 p2 [H6 | H6] [H7 | H7];
          [ apply (proj1 H4 _ _ H6 H7)
          | subst p2; intro H7; apply (H5 p1); trivial;
            right; apply (H3 _ _ H7)
          | subst p1; intro H6; apply (H5 p2); trivial;
            left; apply (H3 _ _ H6)
          | subst p1 p2; intro H6; apply (H0 p); apply (H3 _ _ H6) ]
      | lapply (proj2 H4 i' H6);
           [ intro H7; exists p; split; trivial; apply H7; right; trivial
           | intros q H7; red; auto ] ]
  | intros H5; generalize (not_all_ex_not _ _ H5); clear H5;
    intros (q, H5); generalize (imply_to_and _ _ H5); clear H5;
    intros (H5, H6); exists q; generalize (NNPP _ H6); auto ].
Qed.

(* Remark 28. *)

Remark always_sat_upward_closed :
  forall c s s', sub s s' -> always_sat c s -> always_sat c s'.
intros c s s' H1 H2 i c' H3 H4; generalize (H2 _ _ H3 H4);
intros (q, (H5, H6)); exists q; split; trivial;
apply H1; trivial.
Qed.

(* Another simple properties of \nabla_C. *)

Remark always_sat_antimonotone :
  forall c c' s, (forall p, sub (c' p) (c p)) ->
  always_sat c s -> always_sat c' s.
intros c c' s H1 H2 i c'' H3; apply H2; intro p;
exact (sub_transitive (H3 p) (H1 p)).
Qed.

(*
  We define a coarser preorder on dependency functions that ignores
  dependencies in \nabla_C (Section 7).
*)

Definition dsubc (c : confl_rel) (ds ds' : dep_fun) :=
  forall p,
  forall d, ds p d -> always_sat c d \/ exists d', sub d' d /\ ds' p d'.

Lemma dsubc_trans : forall c, is_transitive (dsubc c).
intros c ds ds' ds'' H1 H2 p s H3;
generalize (H1 _ _ H3); intros [H4 | (s', (H4, H5))]; auto;
generalize (H2 _ _ H5); intros [H6 | (s'', (H6, H7))];
  [ left; exact (always_sat_upward_closed H4 H6)
  | right; exists s''; split; trivial;
    exact (sub_transitive H6 H4) ].
Qed.

Lemma dsubc_refl : forall c, is_reflexive (dsubc c).
intros c ds p s H1; right; exists s; split; trivial; apply sub_reflexive.
Qed.

Definition dep_fun_alt_po c := Build_po (@dsubc_trans c) (@dsubc_refl c).

(*
  Remark 29, propsition 1: if we remove conflicts, the dependency
  functions remain related.
*)

Remark dsubc_weaken :
  forall c c' d d',
  (forall p, sub (c' p) (c p)) ->
  @sub (dep_fun_alt_po c) d d' -> @sub (dep_fun_alt_po c') d d'.
intros c c' d d' H1 H2 p d'' H3;
generalize (H2 _ _ H3); intros [H4 | H4]; auto; left;
intros i c'' H5; apply (H4 i c'');
intro q; exact (sub_transitive (H5 q) (H1 q)).
Qed.

(* Remark 29, propsition 2: it is indeed a coarser preorder. *)

Remark dsubc_coarser :
  forall c (d d' : dep_fun_po), sub d d' -> @sub (dep_fun_alt_po c) d d'.
intros c d d' H1 p d'' H2; right; exact (H1 _ _ H2).
Qed.

(* We lift the preorder to repositories. *)

Definition csubc c c' :=
  @sub (dep_fun_alt_po (conflicts c)) (depends c) (depends c')
    /\
  (forall p, sub (conflicts c p) (conflicts c' p)).

Lemma csubc_trans : is_transitive csubc.
intros c c' c'' (H1, H2) (H3, H4); split;
  [ refine (sub_transitive H1 _);
    intros p d H5; generalize (H3 _ _ H5); intros [H6 | H6]; auto; left;
    apply always_sat_antimonotone with (2 := H6); auto
  | intro p; exact (sub_transitive (H2 p) (H4 p)) ].
Qed.

Lemma csubc_refl : is_reflexive csubc.
intros c; split; [ apply sub_reflexive | intro p; apply sub_reflexive ].
Qed.

Definition constraints_alt_po := Build_po csubc_trans csubc_refl.

(* Remark 29, propsition 3. *)

Remark alt_sub_coarser :
  forall c c' : constraints_po, sub c c' -> @sub constraints_alt_po c c'.
intros c c' (H1, H2); split; trivial;
apply dsubc_coarser; trivial.
Qed.

(* We now define flat repositories (Section 7). *)

Record flat c :=
  { f_refl : @sub (dep_fun_alt_po (conflicts c)) (confl_deps c) (depends c);
    f_trans : @sub (dep_fun_alt_po (conflicts c))
                (dep_fun_compose (depends c) (depends c)) (depends c) }.

(* Lemma 9. *)

Lemma strongly_flat_implies_flat : forall c, strongly_flat c -> flat c.
intros c (H1, H2); split;
apply dsubc_coarser; trivial.
Qed.

(* Technical remark 30. *)

Remark confl_dep_sub_confl :
  forall c q,
  sub (confl_deps c) (depends c) -> has_conflict c q ->
  exists d, sub d (singleton q) /\ depends c q d.
intros c q H1 (q', H2); apply H1; exists q'; split; trivial.
Qed.

(* Lemma 31. *)

Lemma flat_preservation :
   forall c c',
   @sub constraints_alt_po c c' ->
   @sub constraints_po c' c ->
   flat c -> flat c'.
intros c c' (H1, H2) (H3, H4) (H5, H6); split;
  [ refine (sub_transitive _ (dsubc_weaken H4 (sub_transitive H5 H1)));
    apply dsubc_coarser; apply dsub_incl;
    intros p d (q, (H7, H8)); exists q; split; trivial;
    case H7; [left | right]; apply H4; trivial
  | intros p d (d', (H7, (f, (H8, E)))); subst d;
    generalize (H3 _ _ H7); intros (d, (G1, G2));
    generalize (choice (fun qH => H3 _ _ (H8 (proj1_sig qH) (proj2_sig qH))));
    intros (f', G3);
    pose
      (d'' := fun p' => exists q, exists H : d q, f' (exist d' q (G1 q H)) p');
    case (H6 p d'');
      [ exists d; split; trivial;
        exists (fun q H => f' (exist _ q (G1 _ H))); split;
          [ intros q G4;
            generalize (G3 (exist  _ q (G1 _ G4))); intros (G5, G6); trivial
          | trivial ]
      | intro G4; left; subst d'';
        apply always_sat_antimonotone with (1 := H4);
        intros i c'' G5 G6;generalize (G4 _ _ G5 G6);
        intros (q, (G7, (q', (G8, G9))));
        exists q; split; trivial; exists q'; exists (G1 _ G8);
        apply (proj1 (G3 (exist _ _ (G1 _ G8)))); trivial
      | subst d''; intros (d'', (G4, G5));
        generalize (H1 _ _ G5); intros [G6 | (d''', (G6, G7))];
          [ left; apply always_sat_antimonotone with (1 := H4);
            intros i c'' G7 G8; generalize (G6 _ _ G7 G8);
            intros (q, (F1, F2)); generalize (G4 _ F2);
            intros (q', (F3, F4));
            exists q; split; trivial;
            exists q'; exists (G1 _ F3);
            apply (proj1 (G3 (exist _ _ (G1 _ F3)))); trivial
          | right; exists d'''; split; trivial;
            refine (sub_transitive G6 _);
            refine (sub_transitive G4 _);
            intros p' (q, (G8, G9)); exists q; exists (G1 _ G8);
            apply (proj1 (G3 (exist _ q (G1 _ G8)))); trivial ] ] ].
Qed.

(* Two technical lemma used to prove Theorem 10. *)

Lemma feature_maximality :
  forall c s i,
  maximal (fun i => healthy_config c s i) i ->
  maximal
    (fun i : set package =>
       forall p p' : package, i p -> i p' -> conflicts c p p' -> False)
    i.
intros c s i (H1, H2); split;
  [ exact (peace_config H1)
  | intros i' H3 H4; apply H2; trivial;
    split; trivial;
    intros p d H5 H6; generalize (abundance_config H1 H5 H6);
    intros (q, (H7, H8)); exists q; split; trivial;
    apply H4; trivial ].
Qed.

Lemma confl_dep_sub :
  forall c,
  sub (p:=dep_fun_alt_po (conflicts c)) (confl_deps c) (depends c) ->
  sub (confl_deps c) (depends c).
intros c H1 p d H2; generalize (H1 _ _ H2); intros [H3 | H3]; trivial;
generalize H2; clear H2; intros (q, (H2, E)); subst d;
case (classic (conflicts c p p));
  [ intro H4;
    case (H3 (fun q => q <> p)
             (fun p' q' => conflicts c p' q' /\ (p' = p /\ q' = p)));
      [ intros p1 p2 (H5, H6); trivial
      | split;
          [ intros p1 p2 H5 H6 (H7, (H8, H9)); case (H5 H8)
          | intros s H5 H6 p' H7 H8; subst p';
            apply (H5 _ _ H7 H7); auto ]
      | intros p' (H5, H6); case (H5 H6) ]
  | intro H0;
    case (H3 (fun q => q <> p)
             (fun p' q' => conflicts c p' q' /\
                           ((p' = p /\ q' = q) \/ (p' = q /\ q' = p))));
      [ intros p1 p2 (H4, H5); trivial
      | split;
          [ intros p1 p2 H4 H5 (H6, [(H7, H8) | (H7, H8)]);
              [ case (H4 H7)
              | case (H5 H8) ]
          | intros i H4 H5 p1 H6 H7; subst p1;
            assert (H7 : i q);
              [ apply H5; intro E; subst q; apply H0; case H2; trivial
              | case H2; clear H2; intro H2;
                  [ apply (H4 p q); auto
                  | apply (H4 q p); auto ] ] ]
      | intros p' (H4, H5); case (H4 H5) ] ].
Qed.

(* Theorem 10. *)

Theorem weakly_co_installable_implies_co_installable_3 :
  forall c i,
  flat c -> weakly_co_installable c i -> co_installable c i.
intros c i (H1, H2) (s', H3);
generalize (package_set_maximality H3); clear H3;
intros (s, (H4, H4')); clear s' H4';
assert (H3' := feature_maximality H4);
assert (H3'' := abundance_config (proj1 H4));
pattern i in H3''; generalize (package_set_maximality H3''); clear H4 H3'';
intros (i', (H3, H4)); exists i'; split; trivial;
clear i H4; split;
  [ intros p d H5 H6;
    assert (H7 : exists q, d q /\
      forall d', depends c q d' -> exists q', s q' /\ d' q');
      [ assert (H7 : forall d', depends c p d' ->
                     exists q', s q' /\ d' q');
          [ intros d' H7; exact (proj1 H3 _ _ H5 H7)
          | apply NNPP; intro H8;
            generalize
               (choice (fun pH => not_all_ex_not _ _
                           (fun H' => not_ex_all_not _ _ H8
                                    (proj1_sig pH) (conj (proj2_sig pH) H'))));
            clear H8; intros (f, H8);
            assert (H9 := fun q H => imply_to_and _ _ (H8 (exist _ q H)));
            clear H8;
            pose
              (d'' := fun p' => exists q, exists H : d q, f (exist d q H) p');
            case (H2 p d'');
              [ exists d; split; trivial;
                exists (fun q H => f (exist _ q H)); split; trivial;
                intros q H; exact (proj1 (H9 q H))
              | intro G1;
                generalize (G1 _ _ (fun p => sub_reflexive _) H3');
                intros (q, (G2, (q', (G3, G4))));
                apply (proj2 (H9 _ G3));
                exists q; auto
              | intros (d', (G2, G1));
                generalize (H7 _ G1); intros (q', (G3, G4));
                generalize (G2 _ G4); intros (q, (G5, G6));
                apply (proj2 (H9 _ G5)); exists q'; split; trivial ] ]
      | generalize H7; clear H7; intros (q, (H7, H8));
        exists q; split; trivial; apply NNPP; intro H9;
        apply H9; apply (proj2 H3 (fun p => i' p \/ p = q)); simpl; auto;
        clear p d H5 H6 H7;
        intros p d [G1 | G1] G2;
          [ generalize (proj1 H3 _ _ G1 G2);
            intros (p', (G3, G4)); exists p'; auto
          | subst p; exact (H8 _ G2) ] ]
  | intros p p' G1 G2 G3;
    assert (G4 := Build_healthy_config (proj1 H3) (proj1 H3'));
    refine
      (peace_config G4
         (dep_on_conflicts_prop (confl_dep_sub H1) G4 _ G1)
         (dep_on_conflicts_prop (confl_dep_sub H1) G4 _ G2) G3);
      [ exists p' | exists p ];
    red; auto ].
Qed.

(*
  We specify what it means to remove all dependencies in \nabla_C from
  a repository.
*)

Definition simplified c :=
  Build_constraints
    (fun p d => depends c p d /\ ~ always_sat (conflicts c) d) (conflicts c).

(* Lemma 33, proposition 1: we get less dependencies *)

Lemma simplified_smaller : forall c, sub (simplified c) c.
intros c; split;
  [ apply dsub_incl; intros p d (H1, H2); trivial
  | intro p; apply sub_reflexive ].
Qed.

(*
  Lemma 33, proposition 2: we remain equivalent wrt the coarser
  relation.
*)

Lemma simplified_alt_larger :
  forall c, @sub constraints_alt_po c (simplified c).
intros c; split;
  [ intros p d H1; case (classic (always_sat (conflicts c) d)); auto;
    intro H2; right; exists d; split;
      [ apply sub_reflexive
      | simpl; auto ]
  | intro p; apply sub_reflexive ].
Qed.

(* We need this small lemma to complete the proof of theorem 11. *)
(*
  XXX Missing in the paper proof!
*)

Lemma constraint_alt_weakening :
  forall c c' i,
  @sub constraints_alt_po c c' ->
  weakly_co_installable c' i -> weakly_co_installable c i.
intros c c' i (H1, H2) (i', (H3, H4));
pose
  (P := fun i' : set package =>
          forall p p' : package, i' p -> i' p' -> conflicts c p p' -> False);
lapply (@package_set_maximality P i');
  [ intros (i'', (H5, H6)); exists i''; split;
      [ intros p d H7 H8; generalize (H1 _ _ H8); intros [G1 | (d', (G1, G2))];
          [ apply (G1 i'' (conflicts c)); trivial;
            intro q; apply sub_reflexive
          | generalize (H3 _ _ H7 G2); intros (q, (G3, G4));
            exists q; split;
              [ apply H6; trivial
              | apply G1; trivial ] ]
      | exact (proj1 H5) ]
  | intros p p' H5 H6 H7; apply (H4 _ _ H5 H6); apply H2; trivial ].
Qed.

(* Theorem 11. *)

Theorem removing_clearly_irrelevant_dependencies_preserves_flatness :
  forall c, flat c -> flat (simplified c).
intro c; apply flat_preservation;
  [ apply simplified_alt_larger
  | apply simplified_smaller ].
Qed.

Theorem removing_clearly_irrelevant_dependencies_and_weak_co_installability :
  forall c i,
  weakly_co_installable c i <-> weakly_co_installable (simplified c) i.
intros c i; split;
  [ apply constraint_weakening_config_and_co_installability;
    apply simplified_smaller
  | apply constraint_alt_weakening; apply simplified_alt_larger ].
Qed.

(*
  Lemma 34.
  Crucial lemma for reasoning in flat repositories: dependency
  composition (see remark at the end of Section 7.)
*)

Lemma dep_comp :
  forall c p d,
  flat c ->
  depends c p d ->
  forall s, sub s d ->
  forall f : (forall p', s p' -> set package),
  (forall p' (H : s p'), depends c p' (f p' H)) ->
  exists d'', (depends c p d'' \/ always_sat (conflicts c) d'') /\
              sub d'' (fun p => (d p /\ ~s p) \/
                       exists p', exists H : s p', f p' H p).
intros c p d H1 H2 s H3 f H4;
case (classic (forall q', d q' -> ~ s q' -> has_conflict c q'));
  [ intro H5;
    assert
      (H6 : forall q, d q ->
            exists d'',
            depends c q d'' /\
            sub d'' (fun p => (d p /\ ~s p) \/
                              exists p', exists H : s p', f p' H p));
      [ intros q H6; case (classic (s q));
          [ intro H7; exists (f _ H7); split; auto;
            intros q' H8; right; exists q; exists H7; trivial
          | intro H7;
            generalize
              (confl_dep_sub_confl
                 (confl_dep_sub (f_refl H1)) (H5 _ H6 H7));
            intros (d', (H8, H9)); exists d'; split; trivial;
            intros p' G1; rewrite (H8 _ G1); auto ]
      | generalize (choice (fun qH => H6 (proj1_sig qH) (proj2_sig qH)));
        clear H6; intros (g, H6);
        case (@f_trans _ H1 p
               (fun p => exists q, exists H : d q, g (exist _ _ H) p));
          [ exists d; split; trivial;
            exists (fun p H => g (exist _ _ H)); split; trivial;
            intros q H7; exact (proj1 (H6 (exist _ _ H7)))
          | intro H7;
            exists (fun p => exists q, exists H : d q, g (exist _ _ H) p);
            split; auto;
            intros q (q', (H8, H9));
            apply (proj2 (H6 (exist _ _ H8))); trivial
          | intros (d', (H7, H8)); exists d'; split; auto;
            refine (sub_transitive H7 _);
            intros q (q', (G1, G2));
            apply (proj2 (H6 (exist _ _ G1))); trivial ] ]
  | intro H5; generalize (not_all_ex_not _ _ H5); clear H5; intros (q, H5);
    generalize (imply_to_and _ _ H5); clear H5; intros (H5, H6);
    generalize (imply_to_and _ _ H6); clear H6; intros (H6, H7);
    exists (singleton q);
    split;
      [ right; apply always_sat_when_no_conflict with (2 := H7); red; trivial
      | intros q' H8; rewrite H8; auto ] ].
Qed.

(*XXX Remove Lemma 35? *)

(*************************************************************)

(* Conflict covered dependencies (Section 8.2). *)

(* Definition of conflict covered dependencies. *)

Definition conflict_covered c (d : set package) q :=
  d q /\ forall q', in_conflict c q' q ->
         exists d', depends c q' d' /\ sub d' d /\ ~ d' q.

(* What it means to remove a single dependency. *)

Definition remove_dep c p d :=
  Build_constraints
    (fun p' d' => depends c p' d' /\ ~(p' = p /\ d' = d)) (conflicts c).

(*
  Lemma 12: a conflict covered dependency can be removed in a flat
  repository, while leaving (weak) co-instability unchanged.
  (We first prove an intermediate lemma.)
*)

Lemma remove_conflict_covered_0 :
  forall c p d s i q,
  flat c ->
  conflict_covered c d q -> ~dep_satisfied i d ->
  let c' := remove_dep c p d in
  let i' := fun p => i p /\ forall p', in_conflict c p' q -> p <> p' in
  maximal (fun i => healthy_config c' s i) i ->
  forall p' d', s p' -> depends c' p' d' -> ~d' q -> dep_satisfied i' d'.
intros c p d s i q H1 H2 H3 c' i' H4;
intros p' d' H5 H6 H0;
apply NNPP; intro H7;
case (classic (forall q'', i q'' -> d' q'' -> in_conflict c q'' q));
  [ intro G1;
    generalize
      (choice
         (fun qP : sig (fun p => i p /\ d' p) =>
            let P := proj2_sig qP in
            (proj2 H2 _ (G1 (proj1_sig qP) (proj1 P) (proj2 P)))));
    intros (f, G2);
    assert (G3 : @sub (set package) (fun p => i p /\ d' p) d');
      [ intros p0 (G3, G4); trivial
      | generalize
          (dep_comp H1 (proj1 H6) G3
             (fun p s => proj1 (G2 (exist _ p s))));
        intros (d'', (G5, G6));
        assert (G7 : ~dep_satisfied i d'');
          [ intros (q', (G7, G8));
            generalize (G6 _ G8); intros [(F1, F2) | (p'', (F1, F2))];
              [ auto
              | generalize (G2 (exist _ _ F1)); intros (F3, (F4, F5));
                assert (F6 := F4 _ F2);
                apply H3; exists q'; auto ]
          | apply G7; generalize G5; clear G5; intros [G5 | G5];
              [ apply (fun H => abundance_config (proj1 H4) H5 H);
                split; trivial;
                intros (G8, G9); subst p' d'';
                generalize (G6 _ (proj1 H2));
                intros [(F1, F2) | (p'', (F1, F2))];
                  [ case (H0 F1)
                  | generalize (G2 (exist _ _ F1)); intros (F3, (F4, F5));
                    case (F5 F2) ]
              | exact
                  (G5 _ _ (fun p => sub_reflexive _)
                     (feature_maximality H4)) ] ] ]
  | intro G3; generalize (not_all_ex_not _ _ G3); clear G3;
    intros (q'', G3); generalize (imply_to_and _ _ G3); clear G3;
    intros (G3, G4); generalize (imply_to_and _ _ G4); clear G4;
    intros (G4, G5); apply H7;
    exists q''; split; trivial; split; trivial;
    intros q1 G6 G7; subst q1; case (G5 G6) ].
Qed.

Lemma remove_conflict_covered :
  forall c p d s q, no_self_conflict c ->
  flat c -> conflict_covered c d q ->
  weakly_co_installable (remove_dep c p d) s -> weakly_co_installable c s.
intros c p d s q H0 H1 H2 (i', H4);
generalize (package_set_maximality H4); intros (i, (H3, _)); clear i' H4;
case (classic (dep_satisfied i d)); intro H4;
  [ exists i; split;
      [ intros p' d' H5 H6;
        case (classic (p' = p /\ d' = d));
          [ intros (E1, E2); subst d'; trivial
          | intro H7; apply (abundance_config (proj1 H3) H5); split; trivial ]
      | exact (peace_config (proj1 H3)) ]
  | exists
      (fun p => (i p /\ forall p', in_conflict c p' q -> p <> p') \/ p = q);
    split;
      [ intros p' d' H5 H6;
        case (classic (d' q)); intro H7;
          [ exists q; auto
          | lapply (fun H => remove_conflict_covered_0 H1 H2 H4 H3 H5 H H7);
              [ intros (q', ((G1, G2), G3)); exists q'; auto
              | split; trivial;
                intros (E1, E2); subst p' d'; exact (H7 (proj1 H2)) ] ]
      | intros p1 p2 [(H5, H6) | H5] [(H7, H8) | H7];
          [ apply (peace_config (proj1 H3)); trivial
          | subst p2; intro H7; apply (H6 p1); unfold in_conflict; auto
          | subst p1; intro H5; apply (H8 p2); unfold in_conflict; auto
          | subst p1 p2; apply H0 ] ] ].
Qed.

(* Lemma 13. *)

Lemma removal_preserves_flatness :
  forall c p d,
  let c' := remove_dep c p d in
  ~ sub d (singleton p) ->
  (forall d' : set package,
   dep_fun_compose (depends c') (depends c') p d' -> ~ sub d d') ->
  flat c -> flat c'.
intros c p d c' A1 A2 (H1, H2); split;
  [ intros p' d' H3; generalize (H1 _ _ H3); generalize H3; clear H3;
    intros (q, (H3, H4)); subst d'; intros [H4 | (d', (H4, H5))]; auto;
    right; exists d'; split; trivial; split; trivial;
    intros (E1, E2); subst p' d'; exact (A1 H4)
  | intros p' d'' (d', (H4, (f, (H5, E)))); subst d'';
    case (H2 p' (fun p => exists q, exists H : d' q, f q H p));
      [ exists d'; split;
          [ exact (proj1 H4)
          | exists f; split; trivial;
            intros q H; exact (proj1 (H5 q H)) ]
      | auto
      | intros (d'', (H6, H7)); right;
        exists d''; split; trivial; split; trivial;
        intros (E1, E2); subst p' d'';
        apply A2 with (2 := H6);
        exists d'; split; trivial;
        exists f; auto ] ].
Qed.

(*************************************************************)

(* Section 8.3: redundant conflicts *)

(* We specify what it means to remove a single conflict. *)

Definition remove_confl c p1 p2 :=
  Build_constraints
    (depends c)
    (fun q1 q2 =>
       conflicts c q1 q2 /\
       ~(q1 = p1 /\ q2 = p2) /\ ~(q1 = p2 /\ q2 = p1)).

(* Definition of a redundant conflict. *)

Definition redundant_conflict c p1 p2 :=
  exists d1,
  depends c p1 d1 /\
  forall q1, d1 q1 ->
  exists q2,
  exists d2, sub d2 (singleton q2) /\ depends c p2 d2 /\
             conflicts (remove_confl c p1 p2) q1 q2.

(*
  Lemma 14: healthiness is preserved when a redundant conflict is removed.
*)

Lemma redundant_conflict_removal_and_healthiness :
  forall c p1 p2 i,
  redundant_conflict c p1 p2 ->
  healthy (remove_confl c p1 p2) i ->
  healthy c i.
intros c p1 p2 i H1 H2; split;
  [ exact (abundance H2)
  | assert (G1 : i p1 -> i p2 -> False);
      [ intros H3 H4;
        generalize H1; clear H1; intros (d, (H1, H6));
        generalize (abundance H2 H3 H1);
        intros (q1, (H7, H8));
        generalize (H6 _ H8);
        intros (q2, (d', (G1, (G2, G3))));
        generalize (abundance H2 H4 G2);
        intros (q, (G4, G5));
        generalize (G1 _ G5); unfold singleton; intro E; subst q;
        exact (peace H2 H7 G4 G3)
      | intros p p' H3 H4 H5; apply (peace H2 H3 H4); split; trivial; split;
          [ intros (H6, H7); subst p p'; auto
          | intros (H6, H7); subst p p'; auto ] ] ].
Qed.

(*
  We can actually prove a lot more regarding redundant conflict removal:
  - the repository remains flat;
  - weak co-installability is preserved.
*)

Lemma conflict_removal_flat :
  forall c (confl : confl_rel),
  (forall p, sub (confl p) (conflicts c p)) ->
  flat c -> flat (Build_constraints (depends c) confl).
intros c confl H1 (H2, H3); split;
  [ refine (sub_transitive _ (dsubc_weaken H1 H2));
    simpl; intros p d (p', (H5, H6));
    right; exists d; split;
      [ apply sub_reflexive
      | exists p'; split; trivial;
        generalize H1 H5; unfold in_conflict; simpl; intuition ]
  | exact (dsubc_weaken H1 H3) ].
Qed.

Lemma redundant_conflict_removal_and_weak_co_installability :
  forall c p1 p2 i,
  flat c ->
  redundant_conflict c p1 p2 ->
  weakly_co_installable (remove_confl c p1 p2) i ->
  weakly_co_installable c i.
intros c p1 p2 i A2 H0; generalize H0; intros (d1, (H1, H2)) (s', H4);
generalize (package_set_maximality H4); intros (s, (H3, _)); clear s' H4;
case (classic (dep_satisfied s d1));
  [ intros (q1, (H4, H5));
    generalize (H2 _ H5); intros (q2, (d2, (H6, (H7, H7'))));
    exists (fun p => s p /\ ~ p = p2); split;
      [ intros p d G1 G2; generalize (abundance_config (proj1 H3) G1 G2);
        intros (q, (G3, G4));
        case (classic (q = p2));
          [ intro E; subst q;
            case
              (fun H H' => @dep_comp _ _ _
                          (conflict_removal_flat
                             (confl := conflicts (remove_confl c p1 p2)) H' A2)
                          G2 (singleton p2) H (fun _ _ => d2));
              [ simpl; tauto
              | intros q E; rewrite E; trivial
              | intros q G5; rewrite G5; trivial
              | intros d' (G6, G7);
                cut (dep_satisfied s d');
                  [ intros (q, (G8, G9)); generalize (G7 _ G9);
                    intros [(F1, F2) | (q', (F1, F2))];
                      [ exists q; auto
                      | rewrite (H6 _ F2) in G8;
                        case (peace_config (proj1 H3) H4 G8);
                        trivial ]
                  | case G6; clear G6; intro G6;
                      [ exact (abundance_config (proj1 H3) G1 G6)
                      | exact (G6 _ _ (fun p => sub_reflexive _)
                                (feature_maximality H3)) ] ] ]
          | intros G5; exists q; auto ]
      | intros q1' q2' (G1, G2) (G3, G4) G5;
        apply (peace_config (proj1 H3) G1 G3); split; trivial; split;
          [ intros (E1, E2); exact (G4 E2)
          | intros (E1, E2); exact (G2 E1) ] ]
  | intro H4; exists (fun p => s p /\ ~ p = p1); split;
      [ intros p d G1 G2; generalize (abundance_config (proj1 H3) G1 G2);
        intros (q, (G3, G4));
        case (classic (q = p1));
          [ intro E; subst q;
            case
              (fun H H' => @dep_comp _ _ _
                          (conflict_removal_flat
                             (confl := conflicts (remove_confl c p1 p2)) H' A2)
                          G2 (singleton p1) H (fun _ _ => d1));
              [ simpl; tauto
              | intros q E; rewrite E; trivial
              | intros q G5; rewrite G5; trivial
              | intros d' (G6, G7);
                cut (dep_satisfied s d');
                  [ intros (q, (G8, G9)); generalize (G7 _ G9);
                    intros [(F1, F2) | (q', (F1, F2))];
                      [ exists q; auto
                      | case H4; exists q; auto ]
                  | case G6; clear G6; intro G6;
                      [ exact (abundance_config (proj1 H3) G1 G6)
                      | exact (G6 _ _ (fun p => sub_reflexive _)
                                (feature_maximality H3)) ] ] ]
          | intros G5; exists q; auto ]
      | intros q1' q2' (G1, G2) (G3, G4) G5;
        apply (peace_config (proj1 H3) G1 G3); split; trivial; split;
          [ intros (E1, E2); exact (G2 E1)
          | intros (E1, E2); exact (G4 E2) ] ] ].
Qed.

(*************************************************************)

(* Section 8.4: dependence on conflicting packages. *)

Definition strength_dep c p :=
  Build_constraints
    (fun p' d => (p' <> p /\ depends c p' d) \/ (p' = p /\ d = fun _ => False))
    (conflicts c).

(* The class of packages that we consider as clearly broken. *)

Definition clearly_broken c p :=
  exists d, exists q,
  depends c p d /\ sub d (singleton q) /\ in_conflict c p q.

(* Such packages cannot be installed. *)

Lemma clearly_broken_not_installable :
  forall c p, clearly_broken c p -> ~ installable c p.
intros c p (d, (q, (H1, (H2, H3)))) (i, ((H4, H5), H6));
generalize (H4 _ _ H6 H1); intros (p', (H7, H8));
generalize (H2 _ H8); unfold singleton; intro E; subst p';
case H3; eauto.
Qed.

(* The [strength_dep] is rightly named. *)

Lemma strength_dep_prop : forall c p, sub c (strength_dep c p).
intros c p; split;
  [ intros p' d H1; case (classic (p' = p));
      [ intro E; exists (fun _ => False); split;
          [ simpl; contradiction
          | right; auto ]
      | intro H2; exists d; split;
          [ apply sub_reflexive
          | left; auto ] ]
  | intro p'; apply sub_reflexive ].
Qed.

(* Lemma 15. *)

Lemma strenghten_dependence_of_conflicting_package :
  forall c p i,
  ~ installable c p -> (healthy c i <-> healthy (strength_dep c p) i).
intros c p i H1; split;
  [ intros (H2, H3); split; trivial;
    intros p' d H4 [(H5, H6)|(H5, H6)];
      [ eauto
      | case H1; exists i; subst p'; split; trivial;
        split; trivial ]
  | apply constraint_weakening; apply strength_dep_prop ].
Qed.

(*************************************************************)

(* Quotienting (Section 9). *)

(*
  A quotient is defined by a representative function, that maps each
  package to its representative.  The representative of a package
  should have the same dependencies.
*)

Definition representative c (f : package -> package) :=
  forall p, equiv (depends c p) (depends c (f p)).

(*
  Given a representative function, we can define the quotient of a
  repository.
*)

Definition quotient c f :=
  Build_constraints
    (fun p d => exists d', depends c p d' /\ d = img f d')
    (fun p p' => exists q,
       exists q', p = f q /\ p' = f q' /\ conflicts c q q').

(*
   Lemma 16.
   We show that the quotient repository is indeed a repository, as
   long as no package depends on a package it conflicts with.
   (That is, there is no clearly broken packages.)
*)

Definition no_dep_on_conflict c :=
  forall p d q,
  depends c p d -> sub d (singleton q) -> ~ in_conflict c p q.

Lemma quotient_and_self_conflicts :
  forall c f,
  flat c -> representative c f -> no_dep_on_conflict c ->
  no_self_conflict (quotient c f).
intros c f H2 H3 H4 p (q, (q', (E1, (E2, H6))));
assert (H7 : has_conflict c q');
  [ exists q; right; trivial
  | generalize (confl_dep_sub_confl (confl_dep_sub (@f_refl _ H2)) H7);
    intros (d, (H8, H9));
    generalize (proj1 (H3 q') _ H9); rewrite <- E2; rewrite E1;
    intro G1; assert (G2 := proj2 (H3 q) _ G1);
    apply (H4 _ _ _ G2 H8); left; trivial ].
Qed.

(*
  Note that is a package has an empty dependency, all the conflicts it
  is involved in are redundant and can be removed.
  The [no_dep_on_conflict] assumption is thus not too strong.
*)

Lemma conflict_and_empty_dep :
  forall c p p' d,
  depends c p d -> (forall q, ~ d q) -> redundant_conflict c p p'.
intros c p p' d H1 H2; exists d; split; trivial;
intros q H3; case (H2 _ H3).
Qed.

(* Technical lemma: Remark 36. *)

Remark missing_feature_implies_conflict :
  forall c s i,
  no_self_conflict c -> maximal (fun i => healthy_config c s i) i ->
  forall p, ~ i p -> has_conflict c p.
intros c s i A0 H1 p H2;
assert (H3 : ~always_sat (conflicts c) (singleton p));
  [ intro H3; apply H2; red in H3;
    generalize (H3 _ _ (fun p => sub_reflexive _) (feature_maximality H1));
    intros (q, (H4, E)); rewrite <- E; trivial
  | assert (H4 := fun H => H3 (always_sat_when_internal_conflicts A0 H));
    generalize (not_ex_all_not _ _ H4 p); clear H4; intro H4;
    apply NNPP; intro H5; apply H4; split;
      [ red; trivial
      | intros q H6; case H5; exists q; trivial ] ].
Qed.

(* Technical lemma: Lemma 37. *)

Lemma quotient_and_conflicts :
  forall c f s i p p',
  no_self_conflict c -> flat c -> representative c f ->
  maximal (fun i => healthy_config c s i) i ->
  f p = f p' -> ~ i p' ->
  exists d, sub d (singleton p') /\ depends c p d.
intros c f s i p p' A0 A1 A2 H1 E H3;
assert (H4 := missing_feature_implies_conflict A0 H1 H3);
generalize (confl_dep_sub_confl (confl_dep_sub (@f_refl _ A1)) H4);
intros (d, (H5, H6)); exists d; split; trivial;
apply (proj2 (A2 p)); rewrite E; apply (proj1 (A2 p')); trivial.
Qed.

(*
  Lemma 38: we can map maximal healthy configurations in the original
  repository to healthy configurations in the quotiented repository.
*)

Lemma quotient_healthy_1 :
  forall c f s i,
  no_self_conflict c -> flat c -> representative c f ->
  maximal (fun i => healthy_config c s i) i ->
  healthy_config (quotient c f) (img f s) (complimg f i).
intros c f s i A0 A1 H1 H2; split;
  [ intros p d (q, (H3, H4)) (d', (H5, H6)); subst p d;
    generalize (abundance_config (proj1 H2) H3 (proj2 (H1 q) _ H5));
    intros (p, (H6, H7));
    case (classic (forall p, i p -> d' p -> exists p', f p = f p' /\ ~ i p'));
      [ intro G1;
        assert (G2 :
          forall p, i p -> d' p ->
          exists d, exists p', ~ i p' /\ sub d (singleton p') /\ depends c p d);
          [ intros p' G2 G3; generalize (G1 _ G2 G3);
            intros (p'', (E, G4));
            generalize (quotient_and_conflicts A0 A1 H1 H2 E G4);
            intros (d, (G5, G6)); exists d; exists p''; auto
          | generalize
              (choice (fun pH : {p | i p /\ d' p} =>
                 G2 (proj1_sig pH)
                    (proj1 (proj2_sig pH)) (proj2 (proj2_sig pH))));
            intros (g, G3);
            lapply (fun H => @dep_comp _ _ _ A1 (proj2 (H1 _) _ H5) _ H
                                (fun p H => g (exist _ p H)));
              [ intro G4; lapply G4; clear G4;
                  [ intros (d, (G4, G5));
                    cut (dep_satisfied i d);
                      [ intros (p', (G6, G7)); generalize (G5 _ G7);
                        intros [(G8, G9) | (p'', (F1, F2))];
                          [ tauto
                          | generalize (G3 (exist _ _ F1));
                            intros (q', (F3, (F4, F5)));
                            generalize (F4 _ F2); intro E; red in E; subst q';
                            case (F3 G6) ]
                      | case G4; clear G4; intro G4;
                          [ exact (abundance_config (proj1 H2) H3 G4)
                          | exact (G4 _ _ (fun p => sub_reflexive _)
                                    (feature_maximality H2)) ] ]
                  | intros p' G4; generalize (G3 (exist _ _ G4));
                    intros (p'', (_, (_, G5))); trivial ]
              | simpl; tauto ] ]
      | intro G1; generalize (not_all_ex_not _ _ G1); clear G1;
        intros (p', G1); generalize (imply_to_and _ _ G1); clear G1;
        intros (G1, G2); generalize (imply_to_and _ _ G2); clear G2;
        intros (G2, G3); generalize (not_ex_all_not _ _ G3); clear G3;
        intro G3;
        exists (f p'); split;
          [ intros p'' G4; generalize (G3 p''); intro G5;
            apply NNPP; intro G6; apply G5; auto
          | exists p'; auto ] ]
  | intros p1 p2 H3 H4 (q1, (q2, (H5, (H6, H7))));
    exact (peace_config (proj1 H2) (H3 _ H5) (H4 _ H6) H7) ].
Qed.

(*
  Lemma 39: one can associate an healthy configuration in the original
  repository to any healthy configuration in the quotiented
  repository.
*)

Lemma quotient_healthy_2 :
  forall c f i s,
  representative c f ->
  healthy_config (quotient c f) i s ->
  healthy_config c (preimg f i) (preimg f s).
intros c f i s H0 (H1, H2); split;
  [ intros p d (q, (H3, E)) H4; subst q;
    lapply (H1 (f p) (img f d) H3);
      [ intros (q, (H5, (q', (H6, H7)))); subst q;
        exists q'; split; trivial;
        exists (f q'); auto
      | exists d; split; trivial;
        exact (proj1 (H0 p) _ H4) ]
  | intros p p' (q, (H3, E1)) (q', (H4, E2)) H5; subst q q';
    apply (H2 _ _ H3 H4);exists p;  exists p'; auto ].
Qed.

(* Theorem 17: co-installability is left invariant by quotienting. *)

Theorem quotient_and_co_installability :
  forall c f s, no_self_conflict c -> flat c -> representative c f ->
  (weakly_co_installable c s <->
   weakly_co_installable (quotient c f) (img f s)).
intros c f s A1 A2 A3; split;
  [ intros (s', H1);
    generalize (package_set_maximality H1);
    intros (s'', (H2, H3));
    exists (complimg f s'');
    exact (quotient_healthy_1 A1 A2 A3 H2)
  | intros (s', H1);
    exists (preimg f s');
    apply healthy_config_antimonotony with (2 := quotient_healthy_2 A3 H1);
    intros p H2; exists (f p); split; trivial;
    exists p; auto ].
Qed.

(* Technical lemma used to prove Theorem 18. *)

Lemma quotient_in_conflict :
  forall c f p p',
  in_conflict (quotient c f) p p' ->
  exists q, exists q', p = f q /\ p' = f q' /\ in_conflict c q q'.
intros c f p p' [(q, (q', H1)) | (q', (q, H1))];
exists q; exists q'; unfold in_conflict; tauto.
Qed.

(*
  Theorem 18: the quotiented repository is flat when the original
  repository is.
*)

Lemma quotient_flat :
  forall c f,
  no_self_conflict c -> representative c f -> no_dep_on_conflict c ->
  flat c -> flat (quotient c f).
intros c f A0 A1 A2 A3; case A3; intros H1 H2; split;
  [ intros p d (p', (H3, E)); subst d; right;
    generalize (quotient_in_conflict H3);
    intros (q, (q', (E1, (E2, H4)))); subst p p';
    lapply (@confl_dep_sub _ H1 q (singleton q));
      [ intros (d, (H5, H6)); exists (img f d); split;
          [ intros p (q'', (H7, E)); subst p;
            generalize (H5 _ H7); intro E; rewrite E; red; trivial
          | exists d; split; trivial;
            apply (proj1 (A1 q)); trivial ]
      | exists q'; auto ]
  | intros p d'' (d', ((d, (H3, E1)), (g, (H4, E2)))); subst d' d'';
    case
      (classic (always_sat (conflicts (quotient c f))
         (fun p => exists q, exists H : img f d q, g q H p)));
    auto;
    intro H5; right;
    generalize
      (fun H =>
         H5 (@always_sat_when_internal_conflicts _ _
               (quotient_and_self_conflicts A3 A1 A2) H));
    clear H5; intro H5;
    generalize
      (choice (fun pH : {p | img f d p} => H4 _ (proj2_sig pH)));
    clear H4; intros (dg, H4);
    assert (H6 :
      forall p, (exists H, dg H p) ->
      exists p'',
      f p = f p'' /\
      exists q'', in_conflict c p'' q'' /\
                  forall H : {q' | img f d q'}, ~ img f (dg H) (f q''));
      [ intros p1 (H6, H7);
        generalize (not_ex_all_not _ _ H5);
        clear H5; intro H5; generalize (not_and_or _ _ (H5 (f p1)));
        clear H5; intro H5; generalize (or_to_imply _ _ H5);
        clear H5; intro H5; lapply H5;
          [ clear H5; intro H5; generalize (not_all_ex_not _ _ H5);
            clear H5; intros (q1, H5); generalize (imply_to_and _ _ H5);
            clear H5; intros (G1, G2);red in G1;
            assert (G3 : exists p3, exists q3,
                         f p1 = f p3 /\ q1 = f q3 /\ in_conflict c p3 q3);
              [ case G1;
                  [ intros (p3, (q3, (G3, (G4, G5)))); exists p3; exists q3;
                    unfold in_conflict; auto
                  | intros (q3, (p3, (G3, (G4, G5)))); exists p3; exists q3;
                    unfold in_conflict; auto ]
              | revert G3; intros (p3, (q3, (G3, (G4, G5))));
                exists p3; split; trivial; exists q3; split; trivial;
                intros H; rewrite <- (proj2 (H4 H));
                revert H; intros (q2, G6);
                generalize (not_ex_all_not _ _ G2 q2);
                clear G1 G2; intro G2; subst q1;
                exact (not_ex_all_not _ _ G2 G6) ]
          | exists (proj1_sig H6); exists (proj2_sig H6);
            rewrite (proj2 (H4 H6)); exists p1; auto ]
      | clear H5;
        generalize
          (choice (fun pH : {p | exists H, dg H p} =>
             H6 _ (proj2_sig pH)));
        clear H6; intros (h, H6);
        assert (G1 :
          forall d, @sub (set package) d (fun p => exists H, p = h H) ->
          ~ always_sat (conflicts c) d);
          [ intros d' G1 G2;
            generalize (always_sat_implies_internal_conflicts G2);
            clear G2; intros (q, (G2, G3));
            generalize (G1 _ G2); clear G2; intros (H, E); subst q;
            generalize (H6 H); intros (G4, (q'', (G5, G6)));
            generalize (G1 _ (G3 _ G5));
            intros ((p', (G7, G8)), G9);
            subst q'';
            apply (G6 G7);
            exists p'; split; trivial;
            symmetry;
            apply
              (proj1 (H6 (exist (fun p' => _) _ (ex_intro (fun H => _) _ G8))))
          | assert (G2 : forall p (H : d p),
                         exists d' : set package,
                         sub d' (fun p => exists H, p = h H) /\
                         depends c p d');
              [ intros p' H5;
                assert (G2 : forall H p'' (H' : dg H p''),
                             exists d' : set package,
                             sub d' (fun p => exists H, p = h H) /\
                             depends c p'' d');
                  [ intros H7 p'' H8;
                    pose (G3 := exist (fun p => exists H, dg H p) _
                                 (ex_intro (fun H => _) _ H8));
                    assert (G2 : has_conflict c (h G3));
                      [ generalize (proj2 (H6 G3));
                        intros (q, (G4, _)); exists q; trivial
                      | generalize
                          (confl_dep_sub_confl (confl_dep_sub (f_refl A3)) G2);
                        intros (d', (G4, G5)); exists d'; split;
                          [ refine (sub_transitive G4 _);
                            intros q E; rewrite E; exists G3; trivial
                          | apply (proj2 (A1 p''));
                            generalize (proj1 (H6 G3)); simpl;
                            intro E; rewrite E;
                            apply (proj1 (A1 (h G3)));
                            trivial ] ]
                  | assert (H7 : img f d (f p'));
                      [ exists p'; auto
                      | generalize
                          (choice
                             (fun pH => G2 (exist _ _ H7)
                                           (proj1_sig pH) (proj2_sig pH)));
                        intros (i, G3);
                        generalize
                          (dep_comp A3 (proj1 (H4 (exist _ _ H7)))
                             (sub_reflexive _)
                             (fun p H => proj2 (G3 (exist _ _ H))));
                        intros (d'', ([G4 | G4], G5));
                          [ exists d''; split;
                              [ refine (sub_transitive G5 _); clear G5;
                                 intros q [(G5, G6) | (q', (H, G5))];
                                   [ case (G6 G5)
                                   | exact (proj1 (G3 (exist _ _ H)) _ G5) ]
                              | exact (proj2 (A1 p') _ G4) ]
                          | generalize
                              (always_sat_implies_internal_conflicts G4);
                            intros (q, (G6, G7));
                            generalize (G5 _ G6); clear G6;
                            intros [(G6, G8) | (q', (H, G6))];
                              [ case (G8 G6)
                              | generalize (proj1 (G3 _) _ G6);
                                clear G6; intros (H', E); subst q;
                                generalize (proj2 (H6 H'));
                                intros (q'', (G8, G9));
                                generalize (G7 _ G8); clear G7 G8; intro G7;
                                generalize (G5 _ G7); clear G7;
                                intros [(I1, I2) | (q1, (I1, I2))];
                                  [ case (I2 I1)
                                  | generalize (proj1 (G3 _) _ I2);
                                    clear I2; intros (I3, E); subst q'';
                                    rewrite <- (proj1 (H6 I3)) in G9;
                                    revert I3 G9; simpl;
                                    intros (q2, (H'', I4)) G9;
                                    case (G9 H'');
                                    exists q2; auto ] ] ] ] ]
              | assert (G2' : exists f : (forall p, d p -> set package),
                             forall p (H : d p),
                             sub (f _ H) (fun p => exists H, p = h H) /\
                             depends c p (f _ H));
                  [ generalize
                      (choice (fun pH => G2 (proj1_sig pH) (proj2_sig pH)));
                    intros (i, G3);
                    exists (fun p H => i (exist _ _ H));
                    intros q H; exact (G3 (exist _ _ H))
                  | clear G2; revert G2'; intros (i, G2);
                    generalize
                      (dep_comp A3 H3 (@sub_reflexive _ _)
                         (fun p H => proj2 (G2 p H)));
                    intros (d'', ([G3 | G3], G4));
                      [ exists (img f d''); split;
                          [ intros q' (q, (G5, G6)); subst q';
                            generalize (G4 _ G5);
                            intros [(G6, G7) | (q', (H, G6))];
                              [ case (G7 G6)
                              | generalize (proj1 (G2 _ H) _ G6); clear G6;
                                intros (G6, G7); subst q;
                                rewrite <- (proj1 (H6 G6));
                                generalize G6; clear G5 G6;
                                intros (q1, ((q2, G5), G6));
                                exists q2; exists G5;
                                generalize
                                  (proj2 (H4 (exist (fun p => img f d p) q2 G5)));
                                simpl; intro E; rewrite E; clear E;
                                exists q1; auto ]
                          | exists d''; auto ]
                      | case (fun H => G1 _ H G3);
                        intros q G5; generalize (G4 _ G5); clear G5;
                        intros [(G5, G6) | (q', (H, G5))];
                          [ case (G6 G5)
                          | exact (proj1 (G2 _ H) _ G5) ] ] ] ] ] ] ].
Qed.

(*************************************************************)

(* Reflexive transitive reduction (Section 10). *)

(* Lemma 19, and an immediate corollary. *)

Lemma remove_trans_healthy :
  forall c p d i,
  let c' := remove_dep c p d in
  (dep_fun_compose (depends c') (depends c') p d) ->
  healthy c' i -> healthy c i.
intros c p d i c' (d'', (H1, (f, (H2, E)))) (H3, H4); split; trivial;
intros p' d' H5 H6;
case (classic (p' = p /\ d' = d));
  [ intros (E1, E2); subst p' d' c';
    generalize (H3 _ _ H5 H1); intros (p', (H7, H8));
    generalize (H3 _ _ H7 (H2 _ H8)); intros (q, (G1, G2));
    exists q; split; trivial;
    rewrite E; exists p'; exists H8; trivial
  | intro H7; exact (H3 _ _ H5 (conj H6 H7)) ].
Qed.

Lemma remove_trans_co_installable :
  forall c p d i,
  let c' := remove_dep c p d in
  (dep_fun_compose (depends c') (depends c') p d) ->
  co_installable c' i -> co_installable c i.
intros c p d i c' H1 (s, (H2, H3)); exists s; split; trivial;
exact (remove_trans_healthy H1 H2).
Qed.

(* We define what it means to remove self-dependencies. *)

Definition no_self_dep c :=
  Build_constraints (fun p d => depends c p d /\ ~ d p) (conflicts c).

(* Lemma 20. *)

Lemma no_self_dep_healthy :
  forall c i, healthy (no_self_dep c) i -> healthy c i.
intros c i (H1, H2); split; trivial;
intros p d H3 H4; case (classic (d p)); intro H5;
  [ exists p; split; trivial
  | apply (H1 p); trivial;
    split; trivial ].
Qed.

(*
  Co-installability is left invariant when removing self-dependencies
  (not explicitly in the paper).
*)

Lemma no_self_dep_smaller : forall c, sub (no_self_dep c) c.
intros c; split;
  [ apply dsub_incl; intros p d (H1, H2); trivial
  | intro p; apply sub_reflexive ].
Qed.

Lemma no_self_dep_co_installable :
  forall c s, co_installable (no_self_dep c) s <-> co_installable c s.
intros c s; split;
  [ intros (s', (H1, H2)); exists s'; split; trivial;
    exact (no_self_dep_healthy H1)
  | apply constraint_weakening_and_co_installability;
    apply no_self_dep_smaller ].
Qed.

(*************************************************************)

(* Putting all together (Section 11). *)

(* First, some definition useful to specify an algorithm. *)

Definition seq A step1 step2 (c c'' : A) :=
  exists c' : A, step1 c c' /\ step2 c' c''.
Inductive repeat A (step : A -> A -> Prop) : A -> A -> Prop :=
  | rep_zero :
      forall c, repeat step c c
  | rep_succ :
      forall c c' c'', repeat step c c' -> step c' c'' -> repeat step c c''.
Definition assert A P (c c' : A) := c' = c /\ P c.
Infix "++" := seq.

Lemma seq_elim :
  forall A (P : Prop) (step1 step2 : A -> A -> Prop) c c',
  seq step1 step2 c c' -> (forall c'', step1 c c'' -> step2 c'' c' -> P) -> P.
intros A P step1 step2 c c' (c'', (H1, H2)); eauto.
Qed.

Lemma repeat_elim :
  forall A (P : A -> Prop) (step : A -> A -> Prop) c c',
  repeat step c c' -> (forall c c', step c c' -> (P c <-> P c')) ->
  (P c <-> P c').
intros A P step c c' H1 H2; induction H1;
  [ reflexivity
  | rewrite IHrepeat; auto ].
Qed.

(*
  We now specify the algorithm. Note that we do not prove formally any
  termination result when operations are repeated, but there are
  simple arguments for that in each case. Thus, the specification just
  states that some numbers of repetitions have been performed.
*)

Definition flatten c c' := c' = flattened c.
Definition canonise (c c' : constraints) := equiv c c'.
Definition remove_clearly_irrelevant_deps c c' := c' = simplified c.
Definition remove_clearly_broken c c' :=
  exists p, clearly_broken c p /\ c' = strength_dep c p.
Definition remove_redundant_conflict c c' :=
  exists p1, exists p2,
  redundant_conflict c p1 p2 /\ c' = remove_confl c p1 p2.
Definition remove_conflict_covered_deps c c' :=
  exists d, exists p, exists q,
  conflict_covered c d q /\
  let c'' := remove_dep c p d in
  ~ sub d (singleton p) /\
  (forall d' : set package,
   dep_fun_compose (depends c') (depends c') p d' -> ~ sub d d') /\
  c' = c''.
Definition perform_quotient f c c' :=
  representative c f /\ c' = quotient c f.

Definition phase1 :=
  flatten ++ canonise ++ remove_clearly_irrelevant_deps ++
  repeat remove_clearly_broken ++ repeat remove_redundant_conflict.

Definition simplification f :=
  repeat phase1
    ++
  flatten
    ++
  assert no_dep_on_conflict
    ++
  repeat remove_conflict_covered_deps
    ++
  perform_quotient f.

(* We have two technical lemmas regarding phase 1. *)

Lemma phase1_and_co_installability :
  forall c c' i, phase1 c c' -> (co_installable c i <-> co_installable c' i).
intros c1 c4 i H1;
apply (seq_elim H1); clear H1; unfold flatten; intros c2 E H2; subst c2;
apply (seq_elim H2); clear H2; unfold canonise; intros c2 H1 H2;
apply (seq_elim H2); clear H2; unfold remove_clearly_irrelevant_deps;
intros c3 E H2; subst c3;
apply (seq_elim H2); clear H2; intros c3 H2 H3;
transitivity (co_installable c2 i);
  [ transitivity (co_installable (flattened c1) i);
      [ split;
          [ apply flatten_co_inst_prop_1
          | intro H4; apply flatten_co_inst_prop_3;
            apply flatten_co_inst_prop_5; trivial ]
      | split; apply constraint_weakening_and_co_installability;
        case H1;trivial ]
  | transitivity (co_installable (simplified c2) i);
       [ assert (H4 : flat c2);
           [ apply strongly_flat_implies_flat;
             apply (strongly_flat_preserved H1);
             apply flattened_strongly_flat
           | split; intro H5;
               [ apply weakly_co_installable_implies_co_installable_3;
                   [ apply
           removing_clearly_irrelevant_dependencies_preserves_flatness;
                     trivial
                   | rewrite <-
           removing_clearly_irrelevant_dependencies_and_weak_co_installability;
                     apply co_installable_implies_weakly_co_installable;
                     trivial ]
               | apply weakly_co_installable_implies_co_installable_3; trivial;
                 rewrite
           removing_clearly_irrelevant_dependencies_and_weak_co_installability;
                 apply co_installable_implies_weakly_co_installable;
                 trivial ] ]
       | transitivity (co_installable c3 i);
           [ apply (repeat_elim (P := fun c => co_installable c i) H2);
             intros c c' (p, (H4, E)); subst c';
             assert (H5 := clearly_broken_not_installable H4);
             split; intros (i', (H6, H7)); exists i'; split; trivial;
             apply (strenghten_dependence_of_conflicting_package _ H5); trivial
           | apply (repeat_elim (P := fun c => co_installable c i) H3);
             intros c c' (p1, (p2, (H4, E))); subst c';
             split; intros (i', (H6, H7)); exists i'; split; trivial;
               [ apply constraint_weakening with (2 := H6); split;
                   [ apply sub_reflexive
                   | intros p p' (H8, H9); trivial ]
               | apply
                   (redundant_conflict_removal_and_healthiness H4 H6) ] ] ] ].
Qed.

Lemma phase1_preserves_no_self_conflict :
  forall c c', phase1 c c' -> no_self_conflict c -> no_self_conflict c'.
intros c1 c4 H1;
apply (seq_elim H1); clear H1; unfold flatten; intros c2 E H2; subst c2;
apply (seq_elim H2); clear H2; unfold canonise; intros c2 H1 H2;
apply (seq_elim H2); clear H2; unfold remove_clearly_irrelevant_deps;
intros c3 E H2; subst c3;
apply (seq_elim H2); clear H2; intros c3 H2 H3 H4;
assert (H5 : no_self_conflict c2);
  [ assert (H5 : no_self_conflict (flattened c1));
      [ exact H4
      | intros p H6; apply (H5 p); apply (proj2 (proj2 H1)); trivial ]
  | assert (H6 : no_self_conflict (simplified c2));
      [ exact H5
      | assert (H7 : no_self_conflict c3);
          [ clear H3; induction H2; trivial;
            revert H; intros (p, (H7, E)); subst c''; apply IHrepeat; auto
          | induction H3; trivial;
            revert H; intros (p1, (p2, (H8, E))); subst c'';
            intros p (H9, _); revert p H9; apply IHrepeat; trivial ] ] ].
Qed.

(* We can finally prove Theorem 21. *)

Lemma simplification_and_co_installability :
  forall f c c' i, no_self_conflict c -> simplification f c c' ->
  (co_installable c i <-> co_installable c' (img f i)) /\ flat c'.
intros f c1 c4 i A1 H1;
apply (seq_elim H1); clear H1; intros c2 H1 H2;
apply (seq_elim H2); clear H2; unfold flatten; intros c' E H2; subst c';
apply (seq_elim H2); clear H2; intros c' (E, H2) H3; subst c';
apply (seq_elim H3); clear H3; intros c3 H3 (H4, E); subst c4;
assert (A2 : (co_installable c1 i <-> co_installable (flattened c2) i)
             /\ flat (flattened c2) /\ no_self_conflict (flattened c2)
             /\ no_dep_on_conflict (flattened c2));
  [ assert (A2 : (co_installable c1 i <-> co_installable c2 i)
             /\ no_self_conflict c2);
      [ split;
          [ apply (repeat_elim H1 (P := fun c => co_installable c i));
            intros c c'; apply phase1_and_co_installability
          | clear H2 H3; induction H1; trivial;
            generalize (phase1_preserves_no_self_conflict H); auto ]
      | rewrite (proj1 A2); split;
          [ split;
              [ apply flatten_co_inst_prop_1
              | intro H5; apply flatten_co_inst_prop_3;
                apply flatten_co_inst_prop_5; trivial ]
          | split;
              [ apply strongly_flat_implies_flat; apply flattened_strongly_flat
              | split; trivial;
                exact (proj2 A2) ] ] ]
  | rewrite (proj1 A2); clear A1 H1;
    assert (A1 : (co_installable (flattened c2) i <-> co_installable c3 i)
                 /\ flat c3 /\ no_self_conflict c3 /\ no_dep_on_conflict c3);
      [ clear H2 H4;
        induction H3;
          [ tauto
          | revert H; intros (d, (p, (q, (G1, (G2, (G3, G4))))));
            subst c''; assert (G4 : flat (remove_dep c' p d));
              [ apply (removal_preserves_flatness G2 G3); tauto
              | split;
                  [ rewrite (proj1 (IHrepeat A2)); split;
                      [ intro G5;
                        apply weakly_co_installable_implies_co_installable_3;
                        trivial;
                        apply
                          constraint_weakening_config_and_co_installability
                        with
                          (2 :=
                            co_installable_implies_weakly_co_installable
                              G5);
                        split;
                          [ intros p' d' (G6, G7);
                            exists d'; split; trivial;
                            apply sub_reflexive
                          | intro p'; apply sub_reflexive ]
                      | intro G5;
                        assert (G6 := proj1 (proj2 (proj2 (IHrepeat A2))));
                        assert (G7 := proj1 (proj2 (IHrepeat A2)));
                        apply
                          weakly_co_installable_implies_co_installable_3;
                        trivial;
                        apply (remove_conflict_covered (p := p) G6 G7 G1);
                        apply co_installable_implies_weakly_co_installable;
                        trivial ]
                  | split; trivial;
                    split;
                      [ exact (proj1 (proj2 (proj2 (IHrepeat A2))))
                      | intros p' d' q' (G5, _); revert p' d' q' G5;
                        exact (proj2 (proj2 (proj2 (IHrepeat A2)))) ] ] ] ]
      | clear A2; revert A1; intros (A1, (A2, (A3, A4))); rewrite A1; split;
          [ split;
              [ intro G1;
                apply (weakly_co_installable_implies_co_installable_3
                         (quotient_flat A3 H4 A4 A2));
                apply (quotient_and_co_installability i A3 A2 H4);
                apply co_installable_implies_weakly_co_installable; trivial
              | intro G1;
                apply (weakly_co_installable_implies_co_installable_3 A2);
                apply (quotient_and_co_installability i A3 A2 H4);
                apply co_installable_implies_weakly_co_installable; trivial ] 
           | exact (quotient_flat A3 H4 A4 A2) ] ] ].
Qed.

(*
  The repetition of phase 1 should terminate.  The argument is that
  the "remove clearly broken" phase either do nothing or stricly
  increases the number of packages with empty dependencies. Other
  steps in phase 1 preserves or increases this number of packages.
  We prove below that this is the case for flattening.
*)

Lemma flatten_and_empty_dep :
  forall c p d,
  depends c p d -> sub d (fun _ => False) ->
  exists d', depends (flattened c) p d' /\ sub d' (fun _ => False).
intros c p d H1 H2;
assert (f : forall q : package, d q -> below_conflicts c q);
  [ intros H3 H4; case (H2 _ H4)
  | pose (dep := higher_conflicts H1 f);
    exists (flattened_dep dep); split;
      [ exists dep; trivial
      | simpl; intros q (q', (H3, H4)); exact (H2 _ H3) ] ].
Qed.

(*************************************************************)

(* Shapes of installations *)

(*
  To prove that a package is weakly installable, one only needs to
  consider the packages it depends on.
*)
Lemma installability_shape :
  forall c p,
  weakly_co_installable c (singleton p) ->
  exists s, healthy_config c (singleton p) s /\
            forall q, s q -> exists d, depends c p d /\ d q.
intros c p (s, (H1, H2));
exists (fun q => s q /\ exists d : set package, depends c p d /\ d q); split;
  [ split;
      [ intros p' d H3 H4; generalize (H1 _ _ H3 H4);
        intros (q, (H5, H6)); exists q; repeat split; trivial;
        rewrite <- H3; exists d; split; trivial
      | intros p' p'' (H3, _) (H4, _); apply H2; trivial ]
  | tauto ].
Qed.

(*
  The following lemma can be used to prune down the number of pairs of
  packages to consider when computing strong conflicts.

  If two weakly installable packages p and p' are not weakly
  co-installable, one can find two conflicting packages p' and q'
  such that p depends on p' and q depends on q'
*)
Lemma strong_conflict_shape :
  forall c p p',
  weakly_co_installable c (singleton p) ->
  weakly_co_installable c (singleton p') ->
  ~ weakly_co_installable c (fun q => q = p \/ q = p') ->
  exists d, exists d', exists q, exists q',
  depends c p d /\ depends c p' d' /\ d q /\ d' q' /\ in_conflict c q q'.
intros c p p' H1 H2 H3;
generalize (installability_shape H1); intros (s, (H4, H5));
generalize (installability_shape H2); intros (s', (H6, H7));
apply NNPP; intro H8; apply H3;
exists (fun q => s q \/ s' q); split;
  [ intros q d [G1 | G1] G2;
      [ generalize (abundance_config H4 G1 G2); subst q; intros (q, (G3, G4));
        exists q; auto
      | generalize (abundance_config H6 G1 G2); subst q; intros (q, (G3, G4));
        exists q; auto ]
  | intros q q' [G1 | G1] [G2 | G2] G3;
      [ exact (peace_config H4 G1 G2 G3)
      | apply H8;
        generalize (H5 _ G1); intros (d, (G4, G5));
        generalize (H7 _ G2); intros (d', (G6, G7));
        exists d; exists d'; exists q; exists q'; repeat (split; trivial);
        red; auto
      | apply H8;
        generalize (H5 _ G2); intros (d, (G4, G5));
        generalize (H7 _ G1); intros (d', (G6, G7));
        exists d; exists d'; exists q'; exists q; repeat (split; trivial);
        red; auto
      | exact (peace_config H6 G1 G2 G3) ] ].
Qed.
