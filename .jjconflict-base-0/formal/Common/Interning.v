(* Habu.Common.Interning — a model of the interning invariant that the three
   compiler tables share: IR-SYM (src/compiler/ir/symbol.f), IR-TYPE
   (src/compiler/ir/type.f) and IR-ATTR (src/compiler/ir/attr.f).

   All three do the same thing.  A table is an append-only list of distinct
   rows under a committed ceiling.  Interning a key scans the live rows for a
   row that matches the key, answers that row's module-local ordinal if one
   matches, and otherwise appends a new row and answers the ordinal it was
   appended at.  The identity a caller receives is that ordinal packed under
   the module key by IR-ID (src/compiler/ir/id.f), which is already proved in
   Habu.Common.IdLaws; this file is about the ordinal, not about the packing.

   The three tables differ only in what a key is and how two keys are compared:

     - IR-SYM compares symbol bytes.  The scan predicate is
       IR-SYM:ROW-MATCH? (symbol.f:294-298): stored 16-bit content filter
       against the filter of the presented bytes, stored length against the
       presented length, then IR-SYM:BYTES-EQ (symbol.f:262-269) cell by cell.
     - IR-TYPE compares four fixed cells.  The scan predicate is
       IR-TYPE:ROW4-MATCH? (type.f:498-503).
     - IR-ATTR compares five fixed cells.  The scan predicate is
       IR-ATTR:ROW5-MATCH? (attr.f:641-647).

   So the model is written once over an abstract key type with a decidable
   equality, and instantiated twice at the end of the file: once for the
   symbol case (keys are byte strings, and the content filter is modelled
   explicitly so that its non-role in identity is a theorem rather than a
   comment) and once for the structural case (keys may reference ordinals
   strictly below their own, which is what makes reference walks terminate).

   Everything here is proved from the global context alone.  The two axioms
   that Habu.Common.IdAllocator declares belong to the atomic compare-and-swap
   boundary of the module-serial allocator; nothing in this file needs them,
   and every `Print Assumptions` at the bottom reports "Closed under the
   global context".

   ------------------------------------------------------------------------
   MODEL GAPS

   Places where this model and the Habu code deliberately differ.  None of
   them is idealised away silently; each is named so a reader knows exactly
   what the theorems below do and do not cover.

   1. The content filter is not identity-bearing, and that is modelled, not
      assumed.  IR-SYM:FILTER (symbol.f:287-289) is the low sixteen bits of
      the bytes' SHA-256 word 0.  This file treats the filter as an ARBITRARY
      function of the bytes — no injectivity, no collision resistance, not
      even that it is interesting — and proves two separate facts.
      `sym_row_match_sound` says that whatever value sits in the stored filter
      cell, a match still implies byte equality, because the byte verify runs
      behind it; this is the half that survives a bypass-written or corrupted
      filter cell.  `sym_row_match_is_byte_equality` says that when the stored
      cell really is the filter of the stored bytes (which is what
      IR-SYM:ROW-ADD writes, symbol.f:333-339), the whole three-part predicate
      is EXTENSIONALLY EQUAL to plain byte equality, so the filter changes the
      cost of the scan and nothing else.  `sym_filter_without_verify_merges`
      exhibits a filter under which dropping the byte verify would merge two
      different symbols.  Consequence for the rest of the file: the generic
      model instantiates the symbol case at plain byte equality, and that is
      justified by `sym_row_match_is_byte_equality` rather than assumed.
      What is NOT covered: a corrupted stored filter cell can still cause a
      FALSE NEGATIVE — the scan misses an existing row and appends a duplicate
      row for the same bytes.  Soundness (distinct keys stay distinct) does
      not depend on the filter; completeness (no duplicate rows) does depend
      on the stored cell being honest, and the Habu code gets that from
      IR-ARENA ownership rather than from a recheck.

   2. Storage layout is abstracted to a list of keys.  The real tables are two
      IR-ARENA arenas per module (a payload pool and a row table) with a
      three-cell header each, and every read revalidates shape, magic, owner
      serial and span (IR-SYM:PHDR-CK/RHDR-CK/SPAN-CK-N, symbol.f:120-138 and
      219-222; the IR-TYPE and IR-ATTR equivalents at type.f:344-375/472-475
      and attr.f:424-455/545-548).  Those rechecks are what make a
      bypass-written row fail closed instead of reading foreign cells.  This
      model has no cells, so it says nothing about them.

   3. Ownership is not modelled.  IR-SYM:KEY-CK (symbol.f:174-177) and its
      IR-TYPE/IR-ATTR twins reject a foreign module key or a cross-module
      store pairing before any row is touched, and IR-SYM:ID-CK-N
      (symbol.f:184-188) rejects a foreign module's identity.  The model is
      one table under one owner; cross-module separation is Habu.Common.Ids
      and Habu.Common.IdLaws (`distinct_valid_owners_separate`).

   4. Ordinals are not packed.  `intern` here answers a plain `nat`; the Habu
      words answer `key hit IR-ID:PACK-SYMBOL` (symbol.f:372) and equivalents.
      Packing, projection and their round trips are Habu.Common.IdLaws.

   5. One ceiling, not two.  IR-SYM:ROOM-CK (symbol.f:320-323) checks the row
      count against the row table's committed capacity AND the pool cells
      against the pool's committed capacity, and IR-TYPE:FN-END does the same
      pair (type.f:621-622).  This model has a single ceiling.  The atomicity
      argument has the same shape for both — every check precedes the first
      IR-ARENA:PUSH — and the model proves it for one ceiling.

   6. Arena growth, freezing and rollback are not modelled.  IR-ARENA doubles
      its capacity up to the committed ceiling (arena.f:217-231) and throws
      E-IR-ARENA-FULL before any mutation at the ceiling (arena.f:31, 220).
      The model's ceiling is a bound on the row count and the geometric
      growth underneath it is invisible, which is sound precisely because the
      growth never changes what a read returns.  Frozen views (the `F*`
      readers) read the same rows and are not separately modelled.

   7. Only the fixed-field intern paths are modelled.  IR-SYM:INTERN
      (symbol.f:366-376), IR-TYPE:INTERN4 (type.f:516-527) and
      IR-ATTR:INTERN5 (attr.f:678-685) are covered.  IR-TYPE:FN-END
      (type.f:613-632) and the IR-ATTR list/record/string paths
      (attr.f:688-699 and the staged-list words) compare a window of the
      payload pool as well as the fixed cells; the model's abstract key
      equality covers "compare the whole canonical content" but the model does
      not describe the pool window itself.

   8. Order independence at the level of STORED ROWS holds only for keys that
      contain no ordinals.  This is a real distinction, not a modelling
      convenience, and it is proved both ways below: `order_independent`
      proves the permutation statement from equal key sets, and
      `structural_rows_not_permutation` exhibits two admissible insertion
      orders of the same three types whose stored rows are NOT a permutation
      of each other, because a pointer row stores its pointee's ORDINAL and
      that ordinal moves when the order moves.  See the FINDING note above
      `structural_rows_not_permutation`.

   9. Single task, no interleaving.  The tables are built by one compilation
      task holding the context; there is no concurrency in this model.  The
      concurrent part of the substrate is the module-serial allocator, which
      is Habu.Common.IdAllocator.

  10. Machine integers are modelled as `nat`.  The real cells are 64-bit and
      the capacities are bounded by IR-SYM:CAP-MAX / IR-TYPE:CAP-MAX
      (symbol.f:90, type.f:163); overflow of an ordinal is out of scope here
      and is bounded by IdLaws' `local_validb` range instead.

  11. Determinism is outside the model.  IR-SYM:INTERN's real claim is that it
      reads only (a, r, key, p, u) and nothing else — no clock, no allocation
      counter, no pointer identity (symbol.f:26-37).  A model whose `intern`
      takes only a table and a key cannot state that: every Rocq definition is
      already a function of its arguments, so "interning is deterministic"
      would be provable here of an implementation that reads a clock.  Stating
      it would need the ambient state as a parameter.  Nothing below publishes
      it.

  12. Canonicalisation is modelled for the type table only, and its order
      independence is an INSTANCE.  `Types.canonize` is the selection rule a
      canonical type encoder needs - repeatedly take the smallest canonical
      key among the rows whose references are already numbered - and
      `Types.ty_canonize_needs_renumbering` proves that dropping the
      renumbering costs the module its canonical form.  (The shipped
      canonical codec stage was deleted 2026-08-05 for having no consumer;
      the design constraint the model pins is type.f:88's surviving claim
      that any canonical encoder must sort structurally AND renumber.)
      What is proved about agreement is `Types.ty_canonize_orders_agree`,
      which is the counterexample instance and not the general statement
      "any two topological orders of any reference graph canonicalise
      alike"; that general statement is unproved here.
   ------------------------------------------------------------------------
   BINDING GAPS

   Measured by mutating src/compiler/ir/{symbol,type}.f and rerunning
   test/compiler/ir-intern-proof.f.  The scan, the byte verify, the row
   ceiling and the reference bound are all caught by a vector row.  What is
   NOT separately exercised is the committed ceiling's immutability: no row
   observes the header capacity after an intern, so nothing would notice a
   ROW-ADD that rewrote it.  That is why `intern_preserves_ceiling` is an
   internal lemma below rather than a published result.
   ------------------------------------------------------------------------ *)

From Stdlib Require Import Bool List Lia Arith Permutation Wf_nat.
Import ListNotations.

(* ------------------------------------------------------------------ *)
(* A table.                                                            *)
(*                                                                     *)
(* `rows` is the live rows in insertion order, so a row's position IS   *)
(* its module-local ordinal — exactly IR-SYM:CNT (symbol.f:143-144),    *)
(* IR-TYPE:CNT (type.f:380-381) and IR-ATTR:CNT (attr.f:460-461), each  *)
(* of which is the arena's used cells minus the header divided by the   *)
(* row width.  `ceiling` is the committed capacity cell HC-CAP that     *)
(* IR-SYM:NEW / IR-TYPE:NEW / IR-ATTR:NEW write into the header         *)
(* (symbol.f:359, type.f:662).                                          *)
(* ------------------------------------------------------------------ *)

Record table (K : Type) : Type := MkTable {
  rows : list K;
  ceiling : nat
}.

Arguments MkTable {K}.
Arguments rows {K}.
Arguments ceiling {K}.

Definition count {K : Type} (t : table K) : nat := length (rows t).

(* Reading a key back out of a table by its ordinal.  For IR-SYM this is
   what IR-SYM:COPY observes (symbol.f:398-408); for IR-TYPE it is the
   family of accessors behind IR-TYPE:ID-CK (type.f:437-440). *)
Definition key_at {K : Type} (t : table K) (i : nat) : option K :=
  nth_error (rows t) i.

(* The structural invariant the tables maintain: rows are pairwise distinct
   (that is what interning is for) and the live count never passes the
   committed ceiling. *)
Definition table_wf {K : Type} (t : table K) : Prop :=
  NoDup (rows t) /\ count t <= ceiling t.

(* "u is reachable from t by further interning".  Append-only means the old
   rows are a prefix of the new ones. *)
Definition extends {K : Type} (t u : table K) : Prop :=
  exists suffix, rows u = rows t ++ suffix.

Lemma extends_refl : forall {K : Type} (t : table K), extends t t.
Proof.
  intros K t.
  exists [].
  symmetry.
  apply app_nil_r.
Qed.

Lemma extends_trans :
  forall {K : Type} (t u v : table K),
    extends t u -> extends u v -> extends t v.
Proof.
  intros K t u v [s1 H1] [s2 H2].
  exists (s1 ++ s2).
  rewrite H2, H1.
  symmetry.
  apply app_assoc.
Qed.

Lemma key_at_stable :
  forall {K : Type} (t u : table K) i k,
    extends t u -> key_at t i = Some k -> key_at u i = Some k.
Proof.
  intros K t u i k [suffix Hsuffix] Hkey.
  unfold key_at in *.
  rewrite Hsuffix.
  rewrite nth_error_app1.
  - exact Hkey.
  - apply nth_error_Some.
    rewrite Hkey.
    discriminate.
Qed.

Lemma count_stable :
  forall {K : Type} (t u : table K),
    extends t u -> count t <= count u.
Proof.
  intros K t u [suffix Hsuffix].
  unfold count.
  rewrite Hsuffix, length_app.
  lia.
Qed.

Lemma NoDup_snoc :
  forall {A : Type} (l : list A) (x : A),
    NoDup l -> ~ In x l -> NoDup (l ++ [x]).
Proof.
  intros A l.
  induction l as [|y ys IH]; simpl; intros x Hnodup Hfresh.
  - constructor; [intros [] | constructor].
  - inversion Hnodup as [| ? ? Hnotin Hrest]; subst.
    constructor.
    + rewrite in_app_iff.
      simpl.
      intros [Hin | [Heq | Hfalse]].
      * contradiction.
      * apply Hfresh.
        left.
        symmetry.
        exact Heq.
      * contradiction.
    + apply IH.
      * exact Hrest.
      * intros Hin.
        apply Hfresh.
        right.
        exact Hin.
Qed.

(* ------------------------------------------------------------------ *)
(* The interning table, over any key type with a decidable equality.   *)
(* ------------------------------------------------------------------ *)

Section Interning.

Context {K : Type}.

(* The scan predicate.  IR-SYM:ROW-MATCH? (symbol.f:294-298),
   IR-TYPE:ROW4-MATCH? (type.f:498-503), IR-ATTR:ROW5-MATCH?
   (attr.f:641-647).  The two hypotheses are exactly what those three
   predicates provide: a match implies the compared content is the same, and
   a row always matches itself. *)
Variable eqb : K -> K -> bool.
Hypothesis eqb_true : forall a b, eqb a b = true -> a = b.
Hypothesis eqb_refl : forall a, eqb a a = true.

(* The linear scan from ordinal 0 upward, answering the FIRST match.
   IR-SYM:SCAN (symbol.f:300-305), IR-TYPE:SCAN4 (type.f:505-510),
   IR-ATTR:SCAN5 (attr.f:649-654).  Those words answer -1 for "no match";
   `None` is the same fail-closed answer without a sentinel integer. *)
Fixpoint find (ks : list K) (k : K) : option nat :=
  match ks with
  | [] => None
  | k' :: rest =>
      if eqb k' k then Some 0 else option_map S (find rest k)
  end.

Lemma find_sound :
  forall ks k i, find ks k = Some i -> nth_error ks i = Some k.
Proof.
  induction ks as [|k' rest IH]; simpl; intros k i Hfind.
  - discriminate.
  - destruct (eqb k' k) eqn:Hhead.
    + inversion Hfind; subst i.
      simpl.
      f_equal.
      apply eqb_true.
      exact Hhead.
    + destruct (find rest k) as [j |] eqn:Hrest; simpl in Hfind;
        [| discriminate].
      inversion Hfind; subst i.
      simpl.
      apply IH.
      exact Hrest.
Qed.

Lemma find_lt :
  forall ks k i, find ks k = Some i -> i < length ks.
Proof.
  intros ks k i Hfind.
  apply nth_error_Some.
  rewrite (find_sound ks k i Hfind).
  discriminate.
Qed.

Lemma find_in :
  forall ks k, In k ks -> exists i, find ks k = Some i.
Proof.
  induction ks as [|k' rest IH]; simpl; intros k Hin.
  - contradiction.
  - destruct (eqb k' k) eqn:Hhead.
    + exists 0.
      reflexivity.
    + destruct Hin as [Heq | Hin].
      * subst k'.
        rewrite eqb_refl in Hhead.
        discriminate.
      * destruct (IH k Hin) as [i Hi].
        exists (S i).
        rewrite Hi.
        reflexivity.
Qed.

Lemma find_none_not_in :
  forall ks k, find ks k = None -> ~ In k ks.
Proof.
  intros ks k Hnone Hin.
  destruct (find_in ks k Hin) as [i Hi].
  rewrite Hnone in Hi.
  discriminate.
Qed.

(* With distinct rows the scan is not merely SOME index of the key: it is
   THE index.  This is where NoDup earns its place. *)
Lemma find_complete :
  forall ks i k,
    NoDup ks -> nth_error ks i = Some k -> find ks k = Some i.
Proof.
  intros ks i k Hnodup Hnth.
  assert (Hin : In k ks) by (eapply nth_error_In; exact Hnth).
  destruct (find_in ks k Hin) as [j Hj].
  pose proof (find_sound ks k j Hj) as Hnthj.
  pose proof (find_lt ks k j Hj) as Hlt.
  assert (Hsame : j = i).
  { apply (proj1 (NoDup_nth_error ks) Hnodup j i Hlt).
    rewrite Hnthj, Hnth.
    reflexivity. }
  subst j.
  exact Hj.
Qed.

Lemma find_app_left :
  forall ks more k i,
    find ks k = Some i -> find (ks ++ more) k = Some i.
Proof.
  induction ks as [|k' rest IH]; simpl; intros more k i Hfind.
  - discriminate.
  - destruct (eqb k' k) eqn:Hhead.
    + exact Hfind.
    + destruct (find rest k) as [j |] eqn:Hrest; simpl in Hfind;
        [| discriminate].
      inversion Hfind; subst i.
      rewrite (IH more k j Hrest).
      reflexivity.
Qed.

Lemma find_app_new :
  forall ks k,
    find ks k = None -> find (ks ++ [k]) k = Some (length ks).
Proof.
  induction ks as [|k' rest IH]; simpl; intros k Hnone.
  - rewrite eqb_refl.
    reflexivity.
  - destruct (eqb k' k) eqn:Hhead; [discriminate |].
    destruct (find rest k) as [j |] eqn:Hrest; simpl in Hnone;
      [discriminate |].
    rewrite (IH k Hrest).
    reflexivity.
Qed.

(* ------------------------------------------------------------------ *)
(* Interning.                                                          *)
(*                                                                     *)
(* IR-SYM:INTERN (symbol.f:366-376):                                   *)
(*     ... SCAN -> hit ; hit >= 0 -> PACK-SYMBOL and exit              *)
(*     ROOM-CK ; POOL-ADD ; ROW-ADD ; PACK-SYMBOL                      *)
(* IR-TYPE:INTERN4 (type.f:516-527) and IR-ATTR:INTERN5                *)
(* (attr.f:678-685) have the identical shape.  `None` is the throw:    *)
(* E-IR-SYM-CAP (symbol.f:322), E-IR-TYPE-CAP (type.f:521),            *)
(* E-IR-ATTR-CAP (attr.f:658).                                          *)
(*                                                                     *)
(* The capacity test precedes the construction of the new table, which *)
(* is how "no partial row" is expressed in a pure model.  In the Habu  *)
(* code the same ordering is literal: ROOM-CK runs before the first    *)
(* IR-ARENA:PUSH.  `ceiling_write_order_counterexample` below shows    *)
(* that the ordering is load-bearing.                                  *)
(* ------------------------------------------------------------------ *)

Definition intern (t : table K) (k : K) : option (table K * nat) :=
  match find (rows t) k with
  | Some i => Some (t, i)
  | None =>
      if Nat.ltb (count t) (ceiling t)
      then Some (MkTable (rows t ++ [k]) (ceiling t), count t)
      else None
  end.

(* The state-passing form, so that "the table is unchanged on failure" is a
   statement about a table and not about the absence of one. *)
Definition intern_step (t : table K) (k : K) : table K * option nat :=
  match intern t k with
  | Some (t', i) => (t', Some i)
  | None => (t, None)
  end.

Lemma intern_cases :
  forall t k t' i,
    intern t k = Some (t', i) ->
    (find (rows t) k = Some i /\ t' = t)
    \/ (find (rows t) k = None
        /\ count t < ceiling t
        /\ i = count t
        /\ t' = MkTable (rows t ++ [k]) (ceiling t)).
Proof.
  intros t k t' i Hintern.
  unfold intern in Hintern.
  destruct (find (rows t) k) as [j |] eqn:Hfind.
  - inversion Hintern; subst.
    left.
    split; reflexivity.
  - destruct (Nat.ltb (count t) (ceiling t)) eqn:Hroom; [| discriminate].
    inversion Hintern; subst.
    right.
    apply Nat.ltb_lt in Hroom.
    repeat split; assumption.
Qed.

(* ---- 1. FUNCTIONALITY ------------------------------------------------ *)

(* "Interning is a function of the table and the key" is NOT published here.
   Every Rocq definition is a function, so that statement is provable of any
   `intern` whatever — including one that reads a clock — and says nothing
   about symbol.f:26-37.  The determinism the Habu code really has is that
   IR-SYM:INTERN reads only (a, r, key, p, u); expressing it would need a
   model with the state it might have read, which this one does not have.
   MODEL GAP 11 records that. *)

Lemma intern_finds :
  forall t k t' i,
    intern t k = Some (t', i) -> find (rows t') k = Some i.
Proof.
  intros t k t' i Hintern.
  destruct (intern_cases t k t' i Hintern)
    as [[Hfind Heq] | [Hfind [_ [Hi Heq]]]].
  - subst t'.
    exact Hfind.
  - subst t' i.
    simpl.
    apply find_app_new.
    exact Hfind.
Qed.

(* Interning the same key a second time in the resulting state answers the
   same ordinal AND returns the very same table: a hit allocates nothing.
   IR-SYM:INTERN takes the `hit 0 < 0=` branch and exits before ROOM-CK,
   POOL-ADD or ROW-ADD (symbol.f:372); the header comment at symbol.f:35-37
   states this as "a duplicate intern allocates nothing and therefore does
   not consult the context". *)
Theorem intern_idempotent :
  forall t k t' i,
    intern t k = Some (t', i) -> intern t' k = Some (t', i).
Proof.
  intros t k t' i Hintern.
  unfold intern at 1.
  rewrite (intern_finds t k t' i Hintern).
  reflexivity.
Qed.

(* ---- well-formedness is preserved ------------------------------------ *)

Theorem intern_wf :
  forall t k t' i,
    table_wf t -> intern t k = Some (t', i) -> table_wf t'.
Proof.
  intros t k t' i [Hnodup Hbound] Hintern.
  destruct (intern_cases t k t' i Hintern)
    as [[_ Heq] | [Hfind [Hroom [_ Heq]]]].
  - subst t'.
    split; assumption.
  - subst t'.
    unfold table_wf, count in *.
    simpl in *.
    split.
    + apply NoDup_snoc.
      * exact Hnodup.
      * apply find_none_not_in.
        exact Hfind.
    + rewrite length_app.
      simpl.
      lia.
Qed.

(* The committed ceiling carries through an intern unchanged.  Not published:
   `intern` copies the field verbatim in both branches, so this is the
   definition read back.  What a ceiling is FOR is published as
   `ceiling_fail_closed`. *)
Lemma intern_preserves_ceiling :
  forall t k t' i,
    intern t k = Some (t', i) -> ceiling t' = ceiling t.
Proof.
  intros t k t' i Hintern.
  destruct (intern_cases t k t' i Hintern)
    as [[_ Heq] | [_ [_ [_ Heq]]]]; subst t'; reflexivity.
Qed.

(* ---- 3. ROUND TRIP --------------------------------------------------- *)

(* The stored key reads back as the presented key.  For IR-SYM this is the
   invariant IR-SYM:EQ? observes (symbol.f:389-394): the symbol's stored
   bytes compare equal to the bytes that were interned. *)
Theorem intern_roundtrip :
  forall t k t' i,
    intern t k = Some (t', i) -> key_at t' i = Some k.
Proof.
  intros t k t' i Hintern.
  destruct (intern_cases t k t' i Hintern)
    as [[Hfind Heq] | [_ [_ [Hi Heq]]]].
  - subst t'.
    unfold key_at.
    apply find_sound.
    exact Hfind.
  - subst t' i.
    unfold key_at, count.
    simpl.
    rewrite nth_error_app2 by lia.
    rewrite Nat.sub_diag.
    reflexivity.
Qed.

Theorem intern_ordinal_in_range :
  forall t k t' i,
    intern t k = Some (t', i) -> i < count t'.
Proof.
  intros t k t' i Hintern.
  apply nth_error_Some.
  pose proof (intern_roundtrip t k t' i Hintern) as Hkey.
  unfold key_at in Hkey.
  rewrite Hkey.
  discriminate.
Qed.

(* ---- 2. INJECTIVITY -------------------------------------------------- *)

(* Distinct ordinals denote distinct keys.  This is the whole point of the
   scan: IR-SYM:INTERN only appends after SCAN answered -1, so no two rows
   can hold the same content. *)
Theorem key_injective :
  forall (t : table K) i j (k : K),
    table_wf t ->
    key_at t i = Some k -> key_at t j = Some k -> i = j.
Proof.
  intros t i j k [Hnodup _] Hi Hj.
  unfold key_at in *.
  apply (proj1 (NoDup_nth_error (rows t)) Hnodup i j).
  - apply nth_error_Some.
    rewrite Hi.
    discriminate.
  - rewrite Hi, Hj.
    reflexivity.
Qed.

Lemma intern_extends :
  forall t k t' i,
    intern t k = Some (t', i) -> extends t t'.
Proof.
  intros t k t' i Hintern.
  destruct (intern_cases t k t' i Hintern)
    as [[_ Heq] | [_ [_ [_ Heq]]]].
  - subst t'.
    apply extends_refl.
  - subst t'.
    exists [k].
    reflexivity.
Qed.

(* Equal identities exactly when equal keys, for two keys interned into the
   same starting table one after the other.  The forward direction is
   functionality of `key_at`; the backward direction is the scan. *)
Theorem intern_id_iff_key :
  forall t k1 k2 t1 t2 i1 i2,
    table_wf t ->
    intern t k1 = Some (t1, i1) ->
    intern t1 k2 = Some (t2, i2) ->
    (i1 = i2 <-> k1 = k2).
Proof.
  intros t k1 k2 t1 t2 i1 i2 Hwf H1 H2.
  pose proof (intern_wf t k1 t1 i1 Hwf H1) as Hwf1.
  pose proof (intern_wf t1 k2 t2 i2 Hwf1 H2) as Hwf2.
  pose proof (intern_roundtrip t k1 t1 i1 H1) as Hkey1.
  pose proof (intern_roundtrip t1 k2 t2 i2 H2) as Hkey2.
  pose proof (intern_extends t1 k2 t2 i2 H2) as Hext.
  pose proof (key_at_stable t1 t2 i1 k1 Hext Hkey1) as Hkey1'.
  split.
  - intros Hsame.
    subst i2.
    rewrite Hkey1' in Hkey2.
    inversion Hkey2.
    reflexivity.
  - intros Hsame.
    subst k2.
    apply (key_injective t2 i1 i2 k1 Hwf2 Hkey1' Hkey2).
Qed.

(* ---- 4. STABILITY UNDER GROWTH --------------------------------------- *)

(* An ordinal minted in t still denotes the same key in every state reachable
   from t by further interning.  Append-only: IR-SYM:ROW-ADD only pushes
   (symbol.f:333-339), and IR-ARENA has no remove. *)
Theorem intern_key_stable :
  forall t k t' u i,
    intern t k = Some (t', i) ->
    extends t' u ->
    key_at u i = Some k.
Proof.
  intros t k t' u i Hintern Hext.
  apply (key_at_stable t' u i k Hext).
  apply intern_roundtrip with (t := t) (k := k).
  exact Hintern.
Qed.

(* And the ordinal is still THE answer the scan gives: interning the same key
   again in the grown state answers the same identity and allocates nothing.
   This is what makes an identity usable as a long-lived handle. *)
Theorem intern_answer_stable :
  forall t k t' u i,
    intern t k = Some (t', i) ->
    extends t' u ->
    intern u k = Some (u, i).
Proof.
  intros t k t' u i Hintern [suffix Hsuffix].
  unfold intern.
  rewrite Hsuffix.
  rewrite (find_app_left (rows t') suffix k i
             (intern_finds t k t' i Hintern)).
  reflexivity.
Qed.

(* "An ordinal in range stays in range" is not published: it is `count_stable`
   plus arithmetic, and it is strictly weaker than `intern_key_stable` just
   above, which says the ordinal still denotes the SAME key. *)
Lemma intern_ordinal_stays_valid :
  forall (t u : table K) i,
    extends t u -> i < count t -> i < count u.
Proof.
  intros t u i Hext Hlt.
  pose proof (count_stable t u Hext).
  lia.
Qed.

(* ---- ceiling behaviour ----------------------------------------------- *)

(* Interning past the committed ceiling fails and leaves the table exactly as
   it was: no row, no partial row, no bytes.  IR-SYM:ROOM-CK throws
   E-IR-SYM-CAP / E-IR-SYM-BYTES (symbol.f:320-323) BEFORE IR-SYM:POOL-ADD
   and IR-SYM:ROW-ADD run; IR-TYPE:INTERN4 checks at type.f:521 before the
   four pushes at 523-526; IR-ATTR:INTERN5 calls ROOM-CK at attr.f:683
   before ROW-ADD5.  IR-TYPE:FN-END checks BOTH ceilings (type.f:621-622)
   before the first pool push at 625. *)
Lemma ceiling_rejects :
  forall t k,
    find (rows t) k = None ->
    ceiling t <= count t ->
    intern t k = None.
Proof.
  intros t k Hfind Hfull.
  unfold intern.
  rewrite Hfind.
  replace (Nat.ltb (count t) (ceiling t)) with false.
  - reflexivity.
  - symmetry.
    apply Nat.ltb_ge.
    exact Hfull.
Qed.

Theorem ceiling_fail_closed :
  forall t k,
    find (rows t) k = None ->
    ceiling t <= count t ->
    intern_step t k = (t, None).
Proof.
  intros t k Hfind Hfull.
  unfold intern_step.
  rewrite (ceiling_rejects t k Hfind Hfull).
  reflexivity.
Qed.

(* Two statements that used to be published here are not, because neither can
   be false of any `intern` at all:
     - "whenever interning fails the table that survives is the table that
       went in" is the definition of `intern_step`, which answers `(t, None)`
       on a `None` however badly `intern` behaved before refusing.  The claim
       with content is `ceiling_fail_closed` above, which names the table the
       caller started with under the ceiling hypothesis, and
       `ceiling_write_order_counterexample` below, which shows a write-first
       intern failing it.
     - "a full table still answers duplicates" (type.f:514-515) is the first
       branch of `intern` read back.  What makes it true of the Habu code is
       that SCAN runs before ROOM-CK, and that ordering is published as
       `intern_idempotent`, which is derived rather than definitional. *)

(* The write-before-check mutation, to show the ordering in IR-SYM:ROOM-CK
   and IR-TYPE:FN-END is load-bearing rather than stylistic: this variant
   appends first and tests the ceiling afterwards, so a rejected intern
   leaves a row behind. *)
Definition intern_write_first (t : table K) (k : K) : table K * option nat :=
  match find (rows t) k with
  | Some i => (t, Some i)
  | None =>
      let t' := MkTable (rows t ++ [k]) (ceiling t) in
      if Nat.ltb (count t) (ceiling t)
      then (t', Some (count t))
      else (t', None)
  end.

(* ---- 5. ORDER INDEPENDENCE ------------------------------------------- *)

(* Interning a whole list of keys, left to right. *)
Fixpoint intern_all (t : table K) (ks : list K) : option (table K) :=
  match ks with
  | [] => Some t
  | k :: rest =>
      match intern t k with
      | Some (t', _) => intern_all t' rest
      | None => None
      end
  end.

Lemma intern_in_rows :
  forall t k t' i x,
    intern t k = Some (t', i) ->
    (In x (rows t') <-> In x (rows t) \/ x = k).
Proof.
  intros t k t' i x Hintern.
  destruct (intern_cases t k t' i Hintern)
    as [[Hfind Heq] | [_ [_ [_ Heq]]]].
  - subst t'.
    split.
    + intros Hin.
      left.
      exact Hin.
    + intros [Hin | Heqx].
      * exact Hin.
      * subst x.
        eapply nth_error_In.
        apply find_sound.
        exact Hfind.
  - subst t'.
    simpl.
    rewrite in_app_iff.
    simpl.
    split.
    + intros [Hin | [Heqx | Hfalse]].
      * left; exact Hin.
      * right; symmetry; exact Heqx.
      * contradiction.
    + intros [Hin | Heqx].
      * left; exact Hin.
      * right; left; symmetry; exact Heqx.
Qed.

Lemma intern_all_extends :
  forall ks t u, intern_all t ks = Some u -> extends t u.
Proof.
  induction ks as [|k rest IH]; simpl; intros t u Hall.
  - inversion Hall; subst u.
    apply extends_refl.
  - destruct (intern t k) as [[t' i] |] eqn:Hintern; [| discriminate].
    apply extends_trans with (u := t').
    + apply intern_extends with (k := k) (i := i).
      exact Hintern.
    + apply IH.
      exact Hall.
Qed.

Lemma intern_all_wf :
  forall ks t u, table_wf t -> intern_all t ks = Some u -> table_wf u.
Proof.
  induction ks as [|k rest IH]; simpl; intros t u Hwf Hall.
  - inversion Hall; subst u.
    exact Hwf.
  - destruct (intern t k) as [[t' i] |] eqn:Hintern; [| discriminate].
    apply IH with (t := t').
    + apply intern_wf with (t := t) (k := k) (i := i); assumption.
    + exact Hall.
Qed.

Lemma intern_all_in :
  forall ks t u x,
    intern_all t ks = Some u ->
    (In x (rows u) <-> In x (rows t) \/ In x ks).
Proof.
  induction ks as [|k rest IH]; simpl; intros t u x Hall.
  - inversion Hall; subst u.
    split.
    + intros Hin; left; exact Hin.
    + intros [Hin | Hfalse]; [exact Hin | contradiction].
  - destruct (intern t k) as [[t' i] |] eqn:Hintern; [| discriminate].
    rewrite (IH t' u x Hall).
    rewrite (intern_in_rows t k t' i x Hintern).
    split.
    + intros [[Hin | Heqx] | Hin].
      * left; exact Hin.
      * right; left; symmetry; exact Heqx.
      * right; right; exact Hin.
    + intros [Hin | [Heqx | Hin]].
      * left; left; exact Hin.
      * left; right; symmetry; exact Heqx.
      * right; exact Hin.
Qed.

(* Interning a set of keys in any two orders yields tables whose stored keys
   are a permutation of each other.  Stated from the weaker hypothesis that
   the two key lists have the same elements, so a repeated key or a
   reordering both qualify.

   IR-SYM's header comment (symbol.f:48-54) is exactly this: local ordinals
   are insertion-ordered, and what the canonical encoder needs is that the
   SET of symbols does not depend on the order, so it can emit its own
   permutation.  Note MODEL GAP 8: this is sound for keys that contain no
   ordinals; for reference-bearing keys see `structural_rows_not_permutation`
   below. *)
Theorem order_independent :
  forall t0 ks1 ks2 u1 u2,
    table_wf t0 ->
    rows t0 = [] ->
    intern_all t0 ks1 = Some u1 ->
    intern_all t0 ks2 = Some u2 ->
    (forall x, In x ks1 <-> In x ks2) ->
    Permutation (rows u1) (rows u2).
Proof.
  intros t0 ks1 ks2 u1 u2 Hwf Hempty H1 H2 Hsame.
  pose proof (intern_all_wf ks1 t0 u1 Hwf H1) as [Hnd1 _].
  pose proof (intern_all_wf ks2 t0 u2 Hwf H2) as [Hnd2 _].
  apply NoDup_Permutation; [exact Hnd1 | exact Hnd2 |].
  intros x.
  rewrite (intern_all_in ks1 t0 u1 x H1).
  rewrite (intern_all_in ks2 t0 u2 x H2).
  rewrite Hempty.
  simpl.
  split.
  - intros [Hfalse | Hin].
    + contradiction.
    + right.
      apply Hsame.
      exact Hin.
  - intros [Hfalse | Hin].
    + contradiction.
    + right.
      apply Hsame.
      exact Hin.
Qed.

Corollary order_independent_permuted_input :
  forall t0 ks1 ks2 u1 u2,
    table_wf t0 ->
    rows t0 = [] ->
    Permutation ks1 ks2 ->
    intern_all t0 ks1 = Some u1 ->
    intern_all t0 ks2 = Some u2 ->
    Permutation (rows u1) (rows u2).
Proof.
  intros t0 ks1 ks2 u1 u2 Hwf Hempty Hperm H1 H2.
  apply (order_independent t0 ks1 ks2 u1 u2 Hwf Hempty H1 H2).
  intros x.
  split.
  - apply Permutation_in.
    exact Hperm.
  - apply Permutation_in.
    apply Permutation_sym.
    exact Hperm.
Qed.

(* "Equal up to a permutation of ordinals", made explicit: the renumbering is
   a computable function with a computable inverse, and it takes each ordinal
   of one table to the ordinal of the same key in the other. *)
Definition reindex (u t : table K) (i : nat) : nat :=
  match key_at t i with
  | Some k =>
      match find (rows u) k with
      | Some j => j
      | None => count u
      end
  | None => count u
  end.

(* The three `reindex` results are machinery, not published coverage.  No word
   in IR-SYM or IR-TYPE computes a renumbering; and given their hypotheses —
   distinct rows, and two tables with the same elements — they are facts about
   lists that hold whatever the Habu scan does.  They are kept because they
   are the positive counterpart of `structural_rows_not_permutation` below:
   they say what a canonical encoder would have to compute, and MODEL GAP 8
   says why a type encoder cannot skip it. *)
Lemma reindex_preserves_key :
  forall (t u : table K) i,
    (forall x, In x (rows t) <-> In x (rows u)) ->
    i < count t ->
    key_at u (reindex u t i) = key_at t i.
Proof.
  intros t u i Hsame Hlt.
  unfold reindex.
  destruct (key_at t i) as [k |] eqn:Hkey.
  2: { exfalso.
       apply (proj2 (nth_error_Some (rows t) i)) in Hlt.
       unfold key_at in Hkey.
       contradiction. }
  assert (Hin : In k (rows u)).
  { apply Hsame.
    eapply nth_error_In.
    unfold key_at in Hkey.
    exact Hkey. }
  destruct (find_in (rows u) k Hin) as [j Hj].
  rewrite Hj.
  unfold key_at.
  apply find_sound.
  exact Hj.
Qed.

Lemma reindex_in_range :
  forall (t u : table K) i,
    (forall x, In x (rows t) <-> In x (rows u)) ->
    i < count t ->
    reindex u t i < count u.
Proof.
  intros t u i Hsame Hlt.
  unfold reindex.
  destruct (key_at t i) as [k |] eqn:Hkey.
  2: { exfalso.
       apply (proj2 (nth_error_Some (rows t) i)) in Hlt.
       unfold key_at in Hkey.
       contradiction. }
  assert (Hin : In k (rows u)).
  { apply Hsame.
    eapply nth_error_In.
    unfold key_at in Hkey.
    exact Hkey. }
  destruct (find_in (rows u) k Hin) as [j Hj].
  rewrite Hj.
  unfold count.
  apply find_lt with (k := k).
  exact Hj.
Qed.

Lemma reindex_inverse :
  forall (t u : table K) i,
    table_wf t ->
    (forall x, In x (rows t) <-> In x (rows u)) ->
    i < count t ->
    reindex t u (reindex u t i) = i.
Proof.
  intros t u i [Hnodup _] Hsame Hlt.
  pose proof (reindex_preserves_key t u i Hsame Hlt) as Hkey.
  unfold reindex at 1.
  rewrite Hkey.
  destruct (key_at t i) as [k |] eqn:Hkeyi.
  2: { exfalso.
       apply (proj2 (nth_error_Some (rows t) i)) in Hlt.
       unfold key_at in Hkeyi.
       contradiction. }
  rewrite (find_complete (rows t) i k Hnodup Hkeyi).
  reflexivity.
Qed.

(* Lookup is a bijection between the live ordinals and the stored keys.  Both
   halves used to be published and neither is now:
     - totality ("every ordinal below the count reads back") is
       `nth_error_Some`, true of every list there is, and it is already the
       thing `intern_ordinal_in_range` establishes for a minted ordinal.
     - uniqueness is `key_injective` above with the quantifiers rearranged.
   Uniqueness is kept as a lemma because it is the shape a reader wants. *)
Lemma lookup_total :
  forall (t : table K) i, i < count t -> exists k, key_at t i = Some k.
Proof.
  intros t i Hlt.
  unfold key_at, count in *.
  destruct (nth_error (rows t) i) as [k |] eqn:Hnth.
  - exists k.
    reflexivity.
  - exfalso.
    apply (proj2 (nth_error_Some (rows t) i)) in Hlt.
    contradiction.
Qed.

Lemma lookup_unique :
  forall (t : table K) (k : K),
    table_wf t ->
    In k (rows t) ->
    exists! i, key_at t i = Some k.
Proof.
  intros t k Hwf Hin.
  destruct (In_nth_error (rows t) k Hin) as [i Hi].
  exists i.
  split.
  - exact Hi.
  - intros j Hj.
    apply (key_injective t i j k Hwf Hi Hj).
Qed.

End Interning.

Arguments find {K} eqb ks k.
Arguments intern {K} eqb t k.
Arguments intern_step {K} eqb t k.
Arguments intern_write_first {K} eqb t k.
Arguments intern_all {K} eqb t ks.
Arguments reindex {K} eqb u t i.

(* ------------------------------------------------------------------ *)
(* 6. REFERENCE ACYCLICITY.                                            *)
(*                                                                     *)
(* IR-TYPE stores a pointee as a module-local ORDINAL, and the         *)
(* constructor validates that ordinal against the LIVE COUNT before    *)
(* interning: IR-TYPE:POINTER does `r ptee ID-CK` (type.f:686), and    *)
(* IR-TYPE:ID-CK-N throws E-IR-TYPE-BOUND unless the ordinal is below  *)
(* the count (type.f:431-435).  The staged function-type path does the *)
(* same for every element through IR-TYPE:STG-REF-CK (type.f:572-575). *)
(* IR-ATTR:REF-OK (attr.f:552-555) is the reader-side twin.            *)
(*                                                                     *)
(* So the claim at type.f:44-51 — references are acyclic by            *)
(* construction — is modelled here as a hypothesis on the intern        *)
(* operation — the guard is part of the model of the Habu word — and    *)
(* well-foundedness is derived, not assumed.                           *)
(* ------------------------------------------------------------------ *)

Section References.

Context {K : Type}.
Variable eqb : K -> K -> bool.
Variable refs : K -> list nat.
Hypothesis eqb_true : forall a b, eqb a b = true -> a = b.
Hypothesis eqb_refl : forall a, eqb a a = true.

(* IR-TYPE:ID-CK-N's bound test (type.f:435) and IR-TYPE:STG-REF-CK's
   (type.f:575), lifted to every reference a key carries. *)
Definition refs_ok (t : table K) (k : K) : bool :=
  forallb (fun r => Nat.ltb r (count t)) (refs k).

Definition intern_ref (t : table K) (k : K) : option (table K * nat) :=
  if refs_ok t k then intern eqb t k else None.

(* The invariant the guard establishes: every stored reference points
   strictly below the row that carries it. *)
Definition refs_below (t : table K) : Prop :=
  forall i k r,
    key_at t i = Some k -> In r (refs k) -> r < i.

(* The reference relation a walk follows: `ref_rel t j i` means row i
   references row j. *)
Definition ref_rel (t : table K) (j i : nat) : Prop :=
  exists k, key_at t i = Some k /\ In j (refs k).

Lemma refs_ok_lt :
  forall t k r,
    refs_ok t k = true -> In r (refs k) -> r < count t.
Proof.
  intros t k r Hok Hin.
  unfold refs_ok in Hok.
  rewrite forallb_forall in Hok.
  apply Nat.ltb_lt.
  apply Hok.
  exact Hin.
Qed.

Theorem intern_ref_preserves_refs_below :
  forall t k t' i,
    refs_below t ->
    intern_ref t k = Some (t', i) ->
    refs_below t'.
Proof.
  intros t k t' i Hbelow Hintern.
  unfold intern_ref in Hintern.
  destruct (refs_ok t k) eqn:Hok; [| discriminate].
  destruct (intern_cases eqb t k t' i Hintern)
    as [[_ Heq] | [_ [_ [_ Heq]]]].
  - subst t'.
    exact Hbelow.
  - subst t'.
    intros i' k' r Hkey Hin.
    unfold key_at, count in *.
    simpl in Hkey.
    destruct (Nat.lt_ge_cases i' (length (rows t))) as [Hlt | Hge].
    + rewrite nth_error_app1 in Hkey by exact Hlt.
      apply (Hbelow i' k' r Hkey Hin).
    + rewrite nth_error_app2 in Hkey by exact Hge.
      destruct (i' - length (rows t)) as [| d] eqn:Hd; simpl in Hkey.
      * inversion Hkey; subst k'.
        pose proof (refs_ok_lt t k r Hok Hin) as Hr.
        unfold count in Hr.
        lia.
      * destruct d; discriminate.
Qed.

Theorem intern_ref_wf :
  forall t k t' i,
    table_wf t -> intern_ref t k = Some (t', i) -> table_wf t'.
Proof.
  intros t k t' i Hwf Hintern.
  unfold intern_ref in Hintern.
  destruct (refs_ok t k); [| discriminate].
  apply (intern_wf eqb eqb_refl t k t' i Hwf Hintern).
Qed.

(* A reference always points strictly downward. *)
Theorem ref_rel_decreasing :
  forall t j i,
    refs_below t -> ref_rel t j i -> j < i.
Proof.
  intros t j i Hbelow [k [Hkey Hin]].
  apply (Hbelow i k j Hkey Hin).
Qed.

(* Hence the reference relation is well founded, so every walk from an
   identity terminates.  This is the machine-checked form of type.f:44-51
   ("recursive walks terminate on any table state") and of the reader-side
   recheck IR-TYPE:REF-OK (type.f:465-468). *)
Theorem ref_well_founded :
  forall t, refs_below t -> well_founded (ref_rel t).
Proof.
  intros t Hbelow.
  apply (well_founded_lt_compat nat (fun n => n)).
  intros x y Hrel.
  apply (ref_rel_decreasing t x y Hbelow Hrel).
Qed.

(* The concrete termination bound: a walk that starts at ordinal i visits at
   most i further rows, so no walk can be longer than the table is tall. *)
Inductive ref_chain (t : table K) : nat -> list nat -> Prop :=
| ref_chain_nil : forall i, ref_chain t i []
| ref_chain_cons :
    forall i j rest,
      ref_rel t j i ->
      ref_chain t j rest ->
      ref_chain t i (j :: rest).

Theorem ref_chain_bounded :
  forall t i path,
    refs_below t -> ref_chain t i path -> length path <= i.
Proof.
  intros t i path Hbelow Hchain.
  induction Hchain as [i | i j rest Hrel Hchain IH]; simpl.
  - lia.
  - pose proof (ref_rel_decreasing t j i Hbelow Hrel).
    lia.
Qed.

(* A self-reference and any forward reference are rejected before anything is
   written: the guard is the whole reason a cycle cannot exist. *)
Theorem ref_guard_rejects_forward :
  forall t k r,
    In r (refs k) -> count t <= r -> intern_ref t k = None.
Proof.
  intros t k r Hin Hbound.
  unfold intern_ref.
  replace (refs_ok t k) with false.
  - reflexivity.
  - symmetry.
    unfold refs_ok.
    apply Bool.not_true_is_false.
    intros Hok.
    pose proof (refs_ok_lt t k r Hok Hin) as Hlt.
    lia.
Qed.

End References.

Arguments refs_ok {K} refs t k.
Arguments intern_ref {K} eqb refs t k.
Arguments refs_below {K} refs t.
Arguments ref_rel {K} refs t j i.
Arguments ref_chain {K} refs t i path.

(* ------------------------------------------------------------------ *)
(* INSTANTIATION A — the symbol table (IR-SYM, src/compiler/ir/symbol.f) *)
(* ------------------------------------------------------------------ *)

Module Symbols.

Definition byte : Type := nat.
Definition bytes : Type := list byte.

(* IR-SYM:BYTES-EQ (symbol.f:262-269).  The Habu word compares whole packed
   cells, which is exactly byte equality because IR-SYM:PACK-CELL is
   deterministic, every symbol starts on a fresh cell and the tail is
   zero-padded (symbol.f:16-21, 236-247), and the LENGTHS are compared first
   (symbol.f:297). *)
Fixpoint bytes_eqb (a b : bytes) : bool :=
  match a, b with
  | [], [] => true
  | x :: xs, y :: ys => Nat.eqb x y && bytes_eqb xs ys
  | _, _ => false
  end.

Lemma bytes_eqb_true : forall a b, bytes_eqb a b = true -> a = b.
Proof.
  induction a as [|x xs IH]; intros [|y ys] Heq; simpl in Heq;
    try discriminate; try reflexivity.
  apply andb_true_iff in Heq as [Hhead Htail].
  apply Nat.eqb_eq in Hhead.
  subst y.
  f_equal.
  apply IH.
  exact Htail.
Qed.

Lemma bytes_eqb_refl : forall a, bytes_eqb a a = true.
Proof.
  induction a as [|x xs IH]; simpl.
  - reflexivity.
  - rewrite Nat.eqb_refl, IH.
    reflexivity.
Qed.

(* ---- the content filter ---------------------------------------------- *)

(* IR-SYM:FILTER (symbol.f:287-289): the low sixteen bits of the bytes'
   SHA-256 word 0.  Modelled as an ARBITRARY function of the bytes.  No
   property of it is assumed anywhere below — not injectivity, not spread. *)
Section Filter.

Variable filter : bytes -> nat.

(* IR-SYM:ROW-MATCH? (symbol.f:294-298), with the stored filter cell left as
   a free parameter so that a corrupted or bypass-written cell is inside the
   statement rather than outside it. *)
Definition row_match (stored_filter : nat) (stored presented : bytes) : bool :=
  Nat.eqb stored_filter (filter presented)
  && Nat.eqb (length stored) (length presented)
  && bytes_eqb stored presented.

(* Soundness, and it does not depend on the stored filter cell at all: the
   byte verify behind the filter is what decides identity, so whatever the
   filter cell holds, two different byte strings never merge. *)
Theorem sym_row_match_sound :
  forall stored_filter a b,
    row_match stored_filter a b = true -> a = b.
Proof.
  intros stored_filter a b Hmatch.
  unfold row_match in Hmatch.
  apply andb_true_iff in Hmatch as [_ Hbytes].
  apply bytes_eqb_true.
  exact Hbytes.
Qed.

(* And when the stored cell is the honest filter of the stored bytes — which
   is what IR-SYM:ROW-ADD writes (symbol.f:333-339, fed by symbol.f:370) —
   the three-part predicate is extensionally EQUAL to plain byte equality.
   The filter buys scan cost and nothing else; it is not identity-bearing.
   This is what licenses the rest of the file to instantiate the symbol
   table at `bytes_eqb`. *)
Theorem sym_row_match_is_byte_equality :
  forall a b, row_match (filter a) a b = bytes_eqb a b.
Proof.
  intros a b.
  unfold row_match.
  destruct (bytes_eqb a b) eqn:Hbytes.
  - apply bytes_eqb_true in Hbytes.
    subst b.
    rewrite Nat.eqb_refl, Nat.eqb_refl.
    reflexivity.
  - rewrite andb_false_r.
    reflexivity.
Qed.

End Filter.

(* The mutation: a scan that trusts the filter and skips the byte verify.
   Under a filter that collides — and a sixteen-bit filter collides — it
   merges two different symbols into one identity. *)
Definition filter_only_match
    (filter : bytes -> nat) (stored_filter : nat) (presented : bytes) : bool :=
  Nat.eqb stored_filter (filter presented).

Definition collide (_ : bytes) : nat := 0.

Example sym_filter_without_verify_merges :
  row_match collide (collide [1]) [1] [2] = false
  /\ filter_only_match collide (collide [1]) [2] = true.
Proof. split; vm_compute; reflexivity. Qed.

Example sym_verify_discriminates_under_collision :
  collide [1] = collide [2]
  /\ bytes_eqb [1] [2] = false
  /\ row_match collide (collide [1]) [1] [1] = true.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* ---- the symbol table itself ----------------------------------------- *)

Definition sym_intern := intern bytes_eqb.
Definition sym_intern_step := intern_step bytes_eqb.
Definition sym_intern_all := intern_all bytes_eqb.

Definition empty (cap : nat) : table bytes := MkTable [] cap.

Lemma empty_wf : forall cap, table_wf (empty cap).
Proof.
  intros cap.
  unfold table_wf, empty, count, ceiling, rows.
  split.
  - constructor.
  - simpl.
    lia.
Qed.

(* Two byte strings, and the same bytes presented twice from "different
   buffers" — which in this model is the same list, because the model has no
   pointers, and that is precisely the point of symbol.f:33-35. *)
Definition ab : bytes := [97; 98].
Definition cd : bytes := [99; 100].

Example sym_intern_fresh :
  sym_intern (empty 4) ab = Some (MkTable [ab] 4, 0).
Proof. vm_compute; reflexivity. Qed.

(* FUNCTIONALITY: the second intern answers the same ordinal and returns the
   identical table — no row appended. *)
Example sym_intern_hit_allocates_nothing :
  sym_intern (MkTable [ab] 4) ab = Some (MkTable [ab] 4, 0).
Proof. vm_compute; reflexivity. Qed.

(* INJECTIVITY: distinct bytes get distinct ordinals. *)
Example sym_distinct_keys_distinct_ids :
  sym_intern (MkTable [ab] 4) cd = Some (MkTable [ab; cd] 4, 1)
  /\ key_at (MkTable [ab; cd] 4) 0 = Some ab
  /\ key_at (MkTable [ab; cd] 4) 1 = Some cd
  /\ bytes_eqb ab cd = false.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* ROUND TRIP. *)
Example sym_roundtrip_exact :
  key_at (MkTable [ab; cd] 4) 1 = Some cd.
Proof. vm_compute; reflexivity. Qed.

(* STABILITY UNDER GROWTH: ordinal 0 still denotes `ab`, and interning `ab`
   again in the grown table still answers 0 and allocates nothing. *)
Example sym_stability_exact :
  key_at (MkTable [ab; cd] 4) 0 = Some ab
  /\ sym_intern (MkTable [ab; cd] 4) ab = Some (MkTable [ab; cd] 4, 0).
Proof. split; vm_compute; reflexivity. Qed.

(* ORDER INDEPENDENCE: two orders, two different ordinal assignments, same
   set of stored keys. *)
Example sym_order_independent_exact :
  sym_intern_all (empty 4) [ab; cd] = Some (MkTable [ab; cd] 4)
  /\ sym_intern_all (empty 4) [cd; ab] = Some (MkTable [cd; ab] 4)
  /\ reindex bytes_eqb (MkTable [cd; ab] 4) (MkTable [ab; cd] 4) 0 = 1
  /\ reindex bytes_eqb (MkTable [cd; ab] 4) (MkTable [ab; cd] 4) 1 = 0.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* The generic order-independence theorem, instantiated at this table: any
   two orders of the same two symbols store the same two byte strings. *)
Theorem sym_order_independent_instance :
  forall u1 u2,
    sym_intern_all (empty 4) [ab; cd] = Some u1 ->
    sym_intern_all (empty 4) [cd; ab] = Some u2 ->
    Permutation (rows u1) (rows u2).
Proof.
  intros u1 u2 H1 H2.
  apply (order_independent bytes_eqb bytes_eqb_true bytes_eqb_refl
           (empty 4) [ab; cd] [cd; ab] u1 u2 (empty_wf 4) eq_refl H1 H2).
  intros x.
  simpl.
  tauto.
Qed.

(* CEILING: a table at its committed ceiling rejects a NEW symbol and keeps
   its rows exactly, while still answering a duplicate. *)
Example sym_ceiling_exact :
  sym_intern (MkTable [ab] 1) cd = None
  /\ sym_intern_step (MkTable [ab] 1) cd = (MkTable [ab] 1, None)
  /\ sym_intern (MkTable [ab] 1) ab = Some (MkTable [ab] 1, 0).
Proof. repeat split; vm_compute; reflexivity. Qed.

(* The write-before-check mutation leaves a partial table behind on exactly
   the call the real ordering rejects cleanly. *)
Example ceiling_write_order_counterexample :
  sym_intern_step (MkTable [ab] 1) cd = (MkTable [ab] 1, None)
  /\ intern_write_first bytes_eqb (MkTable [ab] 1) cd
       = (MkTable [ab; cd] 1, None).
Proof. split; vm_compute; reflexivity. Qed.

End Symbols.

(* ------------------------------------------------------------------ *)
(* INSTANTIATION B — the structural table (IR-TYPE, src/compiler/ir/type.f) *)
(*                                                                     *)
(* A cut-down IR-TYPE row: the integer kind (type.f:669-674, cells      *)
(* K-INT / width code / sign code / 0) and the pointer kind             *)
(* (type.f:683-687, cells K-PTR / space code / POINTEE ORDINAL / 0).    *)
(* The pointer is the interesting one: its third cell is another row's  *)
(* ordinal, which is what makes references and acyclicity real.         *)
(* ------------------------------------------------------------------ *)

Module Types.

Inductive ty : Type :=
| TInt (width sign : nat)
| TPtr (space pointee : nat).

(* IR-TYPE:ROW4-MATCH? (type.f:498-503): kind cell, then the three field
   cells, compared as plain integers. *)
Definition ty_eqb (a b : ty) : bool :=
  match a, b with
  | TInt w1 s1, TInt w2 s2 => Nat.eqb w1 w2 && Nat.eqb s1 s2
  | TPtr p1 e1, TPtr p2 e2 => Nat.eqb p1 p2 && Nat.eqb e1 e2
  | _, _ => false
  end.

Lemma ty_eqb_true : forall a b, ty_eqb a b = true -> a = b.
Proof.
  intros [w1 s1 | p1 e1] [w2 s2 | p2 e2] Heq; simpl in Heq;
    try discriminate;
    apply andb_true_iff in Heq as [H1 H2];
    apply Nat.eqb_eq in H1; apply Nat.eqb_eq in H2;
    subst; reflexivity.
Qed.

Lemma ty_eqb_refl : forall a, ty_eqb a a = true.
Proof.
  intros [w s | p e]; simpl; rewrite Nat.eqb_refl, Nat.eqb_refl;
    reflexivity.
Qed.

(* The ordinals a row references.  An integer row references nothing; a
   pointer row references its pointee (type.f:686). *)
Definition ty_refs (k : ty) : list nat :=
  match k with
  | TInt _ _ => []
  | TPtr _ pointee => [pointee]
  end.

Definition ty_intern := intern_ref ty_eqb ty_refs.
Definition ty_refs_below := refs_below ty_refs.
Definition empty (cap : nat) : table ty := MkTable [] cap.

(* Concrete rows: i8, i16, and a generic-space pointer to whichever row
   holds i8. *)
Definition i8 : ty := TInt 8 1.
Definition i16 : ty := TInt 16 1.

Example ty_build_exact :
  ty_intern (empty 8) i8 = Some (MkTable [i8] 8, 0)
  /\ ty_intern (MkTable [i8] 8) (TPtr 0 0)
       = Some (MkTable [i8; TPtr 0 0] 8, 1).
Proof. split; vm_compute; reflexivity. Qed.

(* A forward reference — a pointee ordinal at or past the live count — is
   rejected before anything is written.  A self-reference is the same case:
   the row would have to reference its own ordinal, which is the count.
   IR-TYPE:ID-CK-N throws E-IR-TYPE-BOUND here (type.f:435). *)
Example ty_forward_reference_rejected :
  ty_intern (MkTable [i8] 8) (TPtr 0 1) = None
  /\ ty_intern (empty 8) (TPtr 0 0) = None.
Proof. split; vm_compute; reflexivity. Qed.

(* The invariant holds of a fresh table and survives every guarded intern, so
   it holds of every table this package can build. *)
Theorem ty_empty_refs_below : forall cap, ty_refs_below (empty cap).
Proof.
  intros cap i k r Hkey Hin.
  unfold key_at in Hkey.
  simpl in Hkey.
  destruct i; discriminate.
Qed.

Theorem ty_intern_preserves_refs_below :
  forall t k t' i,
    ty_refs_below t -> ty_intern t k = Some (t', i) -> ty_refs_below t'.
Proof.
  intros t k t' i.
  apply (intern_ref_preserves_refs_below ty_eqb ty_refs t k t' i).
Qed.

(* The built table satisfies the strict-decrease invariant, so its reference
   relation is well founded and every walk terminates. *)
Lemma ty_built_refs_below : ty_refs_below (MkTable [i8; TPtr 0 0] 8).
Proof.
  intros i k r Hkey Hin.
  unfold key_at in Hkey.
  simpl in Hkey.
  destruct i as [| [| i]].
  - inversion Hkey; subst k.
    simpl in Hin.
    contradiction.
  - inversion Hkey; subst k.
    simpl in Hin.
    destruct Hin as [Heq | Hfalse]; [| contradiction].
    lia.
  - destruct i; discriminate.
Qed.

Theorem ty_walks_terminate :
  well_founded (ref_rel ty_refs (MkTable [i8; TPtr 0 0] 8)).
Proof.
  apply ref_well_founded.
  exact ty_built_refs_below.
Qed.

Example ty_chain_bound_exact :
  forall path,
    ref_chain ty_refs (MkTable [i8; TPtr 0 0] 8) 1 path -> length path <= 1.
Proof.
  intros path Hchain.
  apply (ref_chain_bounded ty_refs (MkTable [i8; TPtr 0 0] 8) 1 path).
  - exact ty_built_refs_below.
  - exact Hchain.
Qed.

(* ---- FINDING: order independence does NOT survive stored ordinals ----- *)

(* symbol.f:48-54 says the canonical encoder may sort symbols by their bytes
   and emit its own permutation, and type.f:77-79 says the type encoder
   "orders structurally, exactly as for symbols".  For symbols a permutation
   is enough, because a symbol row's identity-bearing content is its bytes
   and those do not move.  For TYPES it is not enough: a pointer row stores
   its pointee's ORDINAL, and reordering the table changes that number.
   Below, the same three types built in the two admissible orders produce
   stored rows that are not a permutation of each other.

   Consequence: a canonical type encoder must RENUMBER the references it
   emits under the permutation it chooses; sorting rows and emitting them is
   not canonicalisation.  `order_independent` above is therefore restricted
   to reference-free keys (MODEL GAP 8).

   Note also that not every order is admissible: a pointer can only be
   interned after its pointee exists, so the admissible orders are exactly
   the topological orders of the reference graph.  Both orders below are
   admissible. *)

Definition order_a : list ty := [i8; i16; TPtr 0 0].
Definition order_b : list ty := [i16; i8; TPtr 0 1].

Definition build (ks : list ty) (t : table ty) : option (table ty) :=
  fold_left
    (fun acc k =>
       match acc with
       | Some t' => match ty_intern t' k with
                    | Some (t'', _) => Some t''
                    | None => None
                    end
       | None => None
       end)
    ks (Some t).

Example ty_both_orders_admissible :
  build order_a (empty 8) = Some (MkTable [i8; i16; TPtr 0 0] 8)
  /\ build order_b (empty 8) = Some (MkTable [i16; i8; TPtr 0 1] 8).
Proof. split; vm_compute; reflexivity. Qed.

(* The two stored row lists denote the same three types, yet they are not a
   permutation of each other: `TPtr 0 0` appears in one and `TPtr 0 1` in the
   other. *)
Theorem structural_rows_not_permutation :
  ~ Permutation [i8; i16; TPtr 0 0] [i16; i8; TPtr 0 1].
Proof.
  intros Hperm.
  assert (Hin : In (TPtr 0 0) [i16; i8; TPtr 0 1]).
  { apply (Permutation_in (TPtr 0 0) Hperm).
    simpl.
    right; right; left; reflexivity. }
  simpl in Hin.
  destruct Hin as [H | [H | [H | H]]]; try discriminate; contradiction.
Qed.

(* The denotation the two orders DO agree on: unfold a row into a term, so
   the stored ordinals disappear.  Fuel bounded by the ordinal, which the
   acyclicity invariant makes sufficient. *)
Inductive term : Type :=
| TmInt (width sign : nat)
| TmPtr (space : nat) (pointee : term).

Fixpoint denote (rws : list ty) (fuel i : nat) : option term :=
  match fuel with
  | 0 => None
  | S f =>
      match nth_error rws i with
      | None => None
      | Some (TInt w s) => Some (TmInt w s)
      | Some (TPtr space pointee) =>
          option_map (TmPtr space) (denote rws f pointee)
      end
  end.

(* Under the strict-decrease invariant, fuel equal to the starting ordinal
   plus one always suffices: the walk cannot be longer than the ordinal. *)
Theorem denote_total :
  forall t,
    ty_refs_below t ->
    forall fuel i,
      i < fuel -> i < count t -> denote (rows t) fuel i <> None.
Proof.
  intros t Hbelow fuel.
  induction fuel as [|f IH]; intros i Hfuel Hcount.
  - lia.
  - simpl.
    destruct (nth_error (rows t) i) as [k |] eqn:Hnth.
    2: { exfalso.
         apply (proj2 (nth_error_Some (rows t) i)) in Hcount.
         contradiction. }
    destruct k as [w s | space pointee].
    + discriminate.
    + assert (Hkey : key_at t i = Some (TPtr space pointee))
        by exact Hnth.
      assert (Hlt : pointee < i).
      { apply (Hbelow i (TPtr space pointee) pointee Hkey).
        simpl.
        left; reflexivity. }
      destruct (denote (rows t) f pointee) as [tm |] eqn:Hsub.
      * simpl.
        discriminate.
      * exfalso.
        apply (IH pointee); [lia | lia | exact Hsub].
Qed.

(* And the two admissible orders agree on the denotation of the pointer type,
   even though their stored rows differ. *)
Example ty_denotation_order_independent :
  denote [i8; i16; TPtr 0 0] 3 2 = Some (TmPtr 0 (TmInt 8 1))
  /\ denote [i16; i8; TPtr 0 1] 3 2 = Some (TmPtr 0 (TmInt 8 1)).
Proof. split; vm_compute; reflexivity. Qed.

(* ---- what canonicalisation has to compute ------------------------------ *)

(* A canonical encoder is what answers the FINDING above: it numbers a frozen
   module's rows in a canonical order and rewrites every stored reference
   under that numbering.  What follows models both halves of that job, and
   shows that the second half cannot be dropped.

   A row's canonical key under a partial numbering `m` is its own content with
   every reference replaced by the referent's CANONICAL ordinal.  A row whose
   pointee has no canonical ordinal yet has no key yet, which is how the model
   states readiness. *)
Definition canon_key (m : list (option nat)) (k : ty) : option ty :=
  match k with
  | TInt w s => Some (TInt w s)
  | TPtr space pointee =>
      match nth_error m pointee with
      | Some (Some c) => Some (TPtr space c)
      | _ => None
      end
  end.

(* The same function with the renumbering removed and nothing else changed: a
   row is still emitted only once its pointee is numbered, but it keeps the
   ordinal it was stored with.  This is the "sort the rows and emit them"
   encoder the FINDING above rejects, and it is here so that rejection can be a
   theorem instead of a warning. *)
Definition stored_key (m : list (option nat)) (k : ty) : option ty :=
  match k with
  | TInt w s => Some (TInt w s)
  | TPtr space pointee =>
      match nth_error m pointee with
      | Some (Some _) => Some (TPtr space pointee)
      | _ => None
      end
  end.

(* A total order on keys: integer rows before pointer rows, then field by
   field - the kind's wire code first, then the fields with references
   already in canonical numbering. *)
Definition ty_ltb (a b : ty) : bool :=
  match a, b with
  | TInt w1 s1, TInt w2 s2 =>
      if Nat.eqb w1 w2 then Nat.ltb s1 s2 else Nat.ltb w1 w2
  | TInt _ _, TPtr _ _ => true
  | TPtr _ _, TInt _ _ => false
  | TPtr p1 e1, TPtr p2 e2 =>
      if Nat.eqb p1 p2 then Nat.ltb e1 e2 else Nat.ltb p1 p2
  end.

Definition assign (m : list (option nat)) (i c : nat) : list (option nat) :=
  firstn i m ++ Some c :: skipn (S i) m.

(* One selection round: the smallest key among the rows that are not
   numbered yet and whose references already are. *)
Definition pick (key : list (option nat) -> ty -> option ty)
                (rws : list ty) (m : list (option nat)) : option (nat * ty) :=
  fold_left
    (fun acc i =>
       match nth_error m i, nth_error rws i with
       | Some None, Some k =>
           match key m k with
           | None => acc
           | Some ck =>
               match acc with
               | None => Some (i, ck)
               | Some (_, bk) => if ty_ltb ck bk then Some (i, ck) else acc
               end
           end
       | _, _ => acc
       end)
    (seq 0 (length rws)) None.

(* One round per row, and no ready row left with rows still unnumbered is a
   refusal (a reference graph with no admissible order) rather than a
   partial answer. *)
Fixpoint canon_rounds (key : list (option nat) -> ty -> option ty)
                      (rws : list ty) (fuel : nat)
                      (m : list (option nat)) (next : nat)
                      (out : list ty) : option (list ty) :=
  match fuel with
  | 0 => Some (rev out)
  | S f =>
      match pick key rws m with
      | None => None
      | Some (i, ck) =>
          canon_rounds key rws f (assign m i next) (S next) (ck :: out)
      end
  end.

Definition canonize_with (key : list (option nat) -> ty -> option ty)
                         (rws : list ty) : option (list ty) :=
  canon_rounds key rws (length rws) (repeat None (length rws)) 0 [].

Definition canonize : list ty -> option (list ty) := canonize_with canon_key.

Definition canonize_stored : list ty -> option (list ty) :=
  canonize_with stored_key.

(* The stored rows of the two admissible orders, named rather than transcribed:
   the example below binds them to what the two builds actually produce. *)
Definition rows_a : list ty := [i8; i16; TPtr 0 0].
Definition rows_b : list ty := [i16; i8; TPtr 0 1].

Example ty_rows_are_the_built_tables :
  build order_a (empty 8) = Some (MkTable rows_a 8)
  /\ build order_b (empty 8) = Some (MkTable rows_b 8).
Proof. split; vm_compute; reflexivity. Qed.

(* The two admissible orders canonicalise to one row list, references and
   all - the property any canonical encoder over this table must have. *)
Example ty_canonize_orders_agree :
  canonize rows_a = Some rows_a /\ canonize rows_b = Some rows_a.
Proof. split; vm_compute; reflexivity. Qed.

(* And the canonical rows denote what the source rows denoted: the pointer is
   the last row of both tables and keeps canonical ordinal 2, so its denotation
   is comparable at the same index. *)
Example ty_canonize_preserves_denotation :
  match canonize rows_b with
  | Some rws => denote rws 3 2 = denote rows_b 3 2
  | None => False
  end.
Proof. vm_compute. reflexivity. Qed.

(* The renumbering is load-bearing, not bookkeeping.  Drop it - keep the
   canonical ORDER and emit the stored ordinal - and the same module built
   along the two admissible orders no longer has one canonical form.  This is
   the counterexample that rejects the "sort the rows and emit them"
   encoder. *)
Theorem ty_canonize_needs_renumbering :
  canonize_stored rows_a <> canonize_stored rows_b.
Proof. vm_compute. discriminate. Qed.

End Types.

(* ------------------------------------------------------------------ *)
(* Assumption discipline.                                              *)
(*                                                                     *)
(* Every top-level theorem must report that it is closed under the     *)
(* global context.  The two axioms in Habu.Common.IdAllocator model    *)
(* the                                                                 *)
(* atomic compare-and-swap boundary of the module-serial allocator and *)
(* are not needed by anything here.                                     *)
(* ------------------------------------------------------------------ *)

Print Assumptions intern_idempotent.
Print Assumptions intern_wf.
Print Assumptions intern_roundtrip.
Print Assumptions intern_ordinal_in_range.
Print Assumptions key_injective.
Print Assumptions intern_id_iff_key.
Print Assumptions intern_key_stable.
Print Assumptions intern_answer_stable.
Print Assumptions ceiling_fail_closed.
Print Assumptions order_independent.
Print Assumptions order_independent_permuted_input.
Print Assumptions intern_ref_preserves_refs_below.
Print Assumptions intern_ref_wf.
Print Assumptions ref_rel_decreasing.
Print Assumptions ref_well_founded.
Print Assumptions ref_chain_bounded.
Print Assumptions ref_guard_rejects_forward.
Print Assumptions Symbols.sym_row_match_sound.
Print Assumptions Symbols.sym_row_match_is_byte_equality.
Print Assumptions Symbols.sym_order_independent_instance.
Print Assumptions Types.ty_empty_refs_below.
Print Assumptions Types.ty_intern_preserves_refs_below.
Print Assumptions Types.ty_walks_terminate.
Print Assumptions Types.structural_rows_not_permutation.
Print Assumptions Types.denote_total.
Print Assumptions Types.ty_canonize_needs_renumbering.
