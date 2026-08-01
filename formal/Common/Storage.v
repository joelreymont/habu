(* Habu.Common.Storage — a model of the compiler IR storage and lifetime layer:
   IR-ARENA (src/compiler/ir/arena.f) and IR-CTX (src/compiler/ir/context.f).

   Everything else in src/compiler/ir rests on these two.  IR-CTX owns one
   compilation: it hands out a generation handle, a bump-allocated scratch
   region inside a single 128K mapping, and a module-serial budget, and it takes
   all of that back when the quotation it wrapped returns.  IR-ARENA is the one
   generic append-only cell store; every dialect table is an instance of it, and
   every arena lives in spans bump-allocated from its owning context.

   This file models what those two words actually do, and proves the six arena
   properties and four context properties the storage layer is relied on for.
   Two of the proofs came out weaker than the code's own header comments claim.
   Both places are written down below as findings rather than smoothed over.

   ------------------------------------------------------------------------
   MODEL GAPS

   Places where this model and the Habu code deliberately differ, or where the
   model is weaker than the prose in the two source files.  Nothing here is
   idealised away silently.

   1. Cells are `nat` and there is no memory.  A real arena cell is an
      eight-byte little-endian slot written with CDIGEST:SLOT! into a span the
      owning context bump-allocated (arena.f:191-193, 286).  This model has a
      list of numbers.  So the model says nothing about aliasing between two
      spans, about the copy in IR-ARENA:COPY-CELLS reading the right bytes, or
      about a pointer that outlived its mapping.  What it does say is what the
      index arithmetic and the ordering of the checks guarantee.

   2. Indices and marks are pairs, not packed words.  IR-ARENA:PACK puts
      (generation << 32 | ordinal) in one cell (arena.f:76-84).  Here a
      `cell_id` is a record of a generation and an ordinal.  The packing and
      its projections are the same shape IR-ID uses and are proved in
      Habu.Common.IdLaws; this file is about what the pair means, not about how
      it is encoded.  Consequence: the model cannot see an ordinal overflowing
      the 32-bit local field.  IR-ARENA:CEIL-MAX bounds that (arena.f:70).

   3. The registry is not modelled as a table of slots.  IR-ARENA keeps 64
      slots in parallel arrays and resolves a handle by scanning them
      (arena.f:90-96, 163-178); IR-CTX keeps a 64-deep stack (context.f:109-112,
      157-167).  For the arena this file models ONE arena value directly, and
      models staleness through the generation field instead: a retired slot is
      generation 0, and a handle that does not name the arena's own generation
      is rejected.  That is exactly what IR-ARENA:IDX-AT compares
      (arena.f:197-201).  For the context the registry IS modelled, as a list,
      because context teardown is a statement about the registry.

   4. Concurrency is not modelled.  IR-ARENA and IR-CTX each take generations
      from an atomic counter (arena.f:148-160, context.f:142-154), and this
      file models `TAKE-GEN` as the pure successor step that the compare-and-
      swap loop eventually commits.  The compare-and-swap boundary itself is
      Habu.Common.IdAllocator, which is where the two host axioms live.  The
      registries in both files are process-wide and single-task by design
      (arena.f:35-37, context.f:45-49).

   5. Growth allocates from the context, and this model does not charge it.
      IR-ARENA:GROW takes a fresh doubled span with IR-CTX:SCRATCH-TAKE and
      abandons the old one (arena.f:217-225).  So an arena that reaches
      capacity C has consumed 8 + 16 + ... + C cells of the context's
      262016-byte scratch region.  In this model growth is free, so every
      ceiling theorem below is about the ceiling only.  FINDING 3 says what
      that leaves uncovered.

   6. Failure is `None`, not a throw code.  Habu names every refusal
      (E-IR-ARENA-FULL, E-IR-ARENA-BOUND, E-IR-ARENA-OWNER, E-IR-ARENA-MARK,
      E-IR-ARENA-FROZEN, E-IR-ARENA-STALE, E-IR-CTX-SCRATCH, E-IR-CTX-SERIALS,
      E-IR-CTX-DEPTH, E-IR-CTX-STALE).  The model has one refusal.  The shared
      vector rows in test/compiler/ir-storage-schema.f carry the throw code, so
      the two sides stay bound row by row rather than only agreeing that
      something failed.

   7. The frozen view is the same value with a flag.  IR-ARENA:FREEZE flips the
      registry slot's state and mints a `view` over the same storage
      (arena.f:336-339).  The model sets a boolean.  The nominal separation
      between `arena` and `view` is a Habu type-system fact and is not modelled.

   8. IR-CTX's persisted header is not modelled.  The target/policy binding is
      written as ten wire codes and read back through the validating
      constructors (context.f:169-292).  That round trip is its own concern and
      is covered by test/compiler/ir-context.f; this file models the mapping as
      a cursor and a module counter.

   9. `abandon` models the throw path of IR-CTX:CTX-ENTER, and it is a real
      difference from normal exit rather than a modelling convenience.  See
      FINDING 1.

  10. Machine integers are `nat`.  IR-CTX:GEN-NEXT-N and IR-ARENA:AGEN-NEXT-N
      also reject a NEGATIVE counter (context.f:139, arena.f:145), which `nat`
      cannot represent, so that half of each guard is invisible here.

  11. `depth_max < gen_max` is assumed, not computed.  `nat` is unary and
      `gen_max` is 2147483647, so any computation that forces the generation
      ceiling into constructor form takes over a minute; every other proof in
      this file keeps the ceiling symbolic, which is why they are all fast.
      The nesting results below therefore take `depth_max < gen_max` as a
      hypothesis.  The two numbers are 64 and 2147483647 and both are pinned to
      the shipped source by the capacity rows in
      test/compiler/ir-storage-schema.f, so the hypothesis is a true fact about
      two checked constants; it is simply not one this representation can be
      asked to decide.

   ------------------------------------------------------------------------
   BINDING GAPS

   What the parity gate does and does not notice, measured by mutating
   src/compiler/ir/{arena,context}.f and rerunning
   test/compiler/ir-storage-proof.f.  Every published result below survives
   only because the gate ties the model to the shipped words; where that tie
   is weak, the theorem is still a claim about the design but the gate would
   not catch the code drifting away from it.

   All three gaps recorded here before have since been closed by rows, and each
   closure was checked the same way it was found: break the code, watch the gate
   go red on the new row, put the code back, watch it go green again.

   B1. CLOSED.  IR-ARENA:ABORT is driven by the `abort` row.  It pushes a cell,
       keeps its index, reads it, aborts the arena and reads the same index
       again, which must now be refused, while the second arena carries on
       working.  Deleting the `0 slot AGEN!` that retires the slot now makes
       that read answer the cell again and the row fails.

   B2. CLOSED.  IR-CTX:DEPTH-ROOM is driven by the two nesting depth rows.  One
       opens 63 contexts inside one another and requires the next entry to be
       accepted; the other opens 64 and requires the next entry to be refused
       with E-IR-CTX-DEPTH.  Raising the bound fails the second row and lowering
       it fails the first, so the limit is pinned from both sides rather than
       only through the DEPTH-MAX literal.

   B3. CLOSED for the mark comparison.  The `foreign_mark` row rolls one arena
       back with a mark the other arena minted, requires E-IR-ARENA-OWNER, and
       then shows the first arena's cursor and cells untouched and its own mark
       still accepted.  Removing the generation compare from IR-ARENA:ROLLBACK
       now fails that row on behaviour, not only the frozen guard body: with the
       compare gone the first arena truncates to the other one's cursor.

   ------------------------------------------------------------------------
   FINDINGS — three claims in the source comments that these proofs do not
   support.  Each is exhibited as an executable example below.

   FINDING 1.  arena.f:20-26 says every arena resolution "probes
   IR-CTX:SERIAL-LIVE? so an arena whose context tore down rejects with
   E-IR-ARENA-STALE before any pointer is touched".  That holds for a context
   that LEAVES NORMALLY.  It does not hold for a context abandoned by a throw.
   IR-CTX:CTX-ENTER retires its slot and truncates the depth in the two lines
   AFTER the body runs (context.f:329-330), so a throw skips both, while
   MEM:WITH-BYTES still releases the mapping.  The registry therefore reports
   that serial LIVE while its storage is gone, until an enclosing context
   leaves.  `ctx_abandoned_context_still_reports_live` below is that state.
   The safety of the layer on that path rests on the sealed handle argument in
   context.f:22-27 (no reachable handle survives the throw), not on the liveness
   probe — which is a different, weaker guarantee than the comment states.

   FINDING 2.  IR-ARENA:ROLLBACK truncates the cursor and nothing else
   (arena.f:323-330).  It does not bump the arena's generation, and an index
   carries only (generation, ordinal).  So an index minted above a mark is dead
   only while the cursor stays below it: push again and the SAME index becomes
   valid and denotes a DIFFERENT cell, silently.
   `arena_rollback_reuses_ordinal` below is that state.  The append-only
   theorems in this file are therefore theorems about push alone; rollback is
   not append-only, and the arena's users must not hold an index across a
   rollback that could be re-passed.  Nothing in the code enforces that.

   FINDING 3.  arena.f:13-15 calls the abandoned-span discipline "bounded by
   the committed ceiling".  The real bound is the context's single 256K
   mapping, and it bites first.  Reaching capacity C costs 8 + 16 + ... + C
   cells of a 262016-byte scratch region, so the largest capacity an arena can
   ever reach — even as the only allocation in its context — is 8192 cells.
   A committed ceiling above that cannot be enforced: the arena dies of
   E-IR-CTX-SCRATCH inside IR-ARENA:GROW instead of E-IR-ARENA-FULL, which is
   a different error and, unlike E-IR-ARENA-FULL, is not the one the caller
   was told to expect.  `arena_ceiling_beyond_the_mapping` below is the
   arithmetic.  The atomicity is not affected — the scratch take precedes the
   copy and both field writes, so a refused growth still leaves the arena
   untouched — but the ceiling a caller commits to above 8192 cells is
   advisory rather than enforced.
   ------------------------------------------------------------------------ *)

From Stdlib Require Import Bool List Lia Arith.
Import ListNotations.

(* ================================================================== *)
(* Constants, mirrored from the two source files.                      *)
(*                                                                     *)
(* Each one is also a frozen row in test/compiler/ir-storage-schema.f, *)
(* read back out of the production source by the gate, so a renumbered *)
(* Habu constant and this file cannot drift apart.                     *)
(* ================================================================== *)

Definition seed_cells : nat := 8.            (* IR-ARENA:SEED-CELLS, arena.f:66 *)
Definition slot_max : nat := 64.             (* IR-ARENA:SLOT-MAX, arena.f:65 *)
Definition slot_bytes : nat := 8.            (* CDIGEST:SLOT-BYTES, digest.f:54 *)
Definition hdr_slots : nat := 16.            (* IR-CTX:HDR-SLOTS, context.f:83 *)
Definition map_bytes : nat := 262144.        (* IR-CTX:MAP-BYTES, context.f:73 *)
Definition depth_max : nat := 64.            (* IR-CTX:DEPTH-MAX, context.f:72 *)
Definition gen_max : nat := 2147483647.      (* IR-CTX:GEN-MAX, context.f:71 *)
Definition local_bits : nat := 32.           (* IR-ARENA:LOCAL-BITS, arena.f:68 *)

Definition hdr_bytes : nat := hdr_slots * slot_bytes.
Definition scratch_cap : nat := map_bytes - hdr_bytes.

(* ================================================================== *)
(* IR-ARENA                                                            *)
(* ================================================================== *)

(* One arena's registry row.  `cells` in insertion order, so a cell's
   position IS its ordinal — IR-ARENA:ACOUNT is its length (arena.f:113-117)
   and IR-ARENA:NTH-RAW stamps the position (arena.f:203-206).  `agen` is the
   slot's generation cell IR-ARENA:AGENS, zero meaning a retired slot
   (arena.f:139, 346).  `acap` is the committed span capacity IR-ARENA:ACAPS
   and `aceil` the ceiling IR-ARENA:ACEILS.  `afrozen` is IR-ARENA:ASTATES
   holding ST-FROZEN rather than ST-LIVE. *)
Record arena : Type := MkArena {
  agen : nat;
  aowner : nat;
  cells : list nat;
  acap : nat;
  aceil : nat;
  afrozen : bool
}.

Definition acount (a : arena) : nat := length (cells a).

(* Whether the registry slot behind this arena is still installed.  A retired
   slot holds generation zero (arena.f:346, and arena.f:176-177 for a slot whose
   owner died), and IR-ARENA:RESOLVE throws E-IR-ARENA-STALE on it before any
   word looks at a cursor or a pointer.  This is the decidable form of the live
   clause of `arena_wf`, and it is what the generated vector machine in
   test/compiler/ir-storage-obligations.f consults before every step, so that an
   aborted arena refuses everything on the model side exactly as the shipped
   word does. *)
Definition alive (a : arena) : bool := negb (Nat.eqb (agen a) 0).

(* A minted index and a mark.  Both are (generation, ordinal) pairs that name
   the arena that minted them: IR-ARENA:MINT-IDX (arena.f:206) and
   IR-ARENA:MINT-MARK (arena.f:317-319). *)
Record cell_id : Type := MkIdx { idx_gen : nat; idx_ord : nat }.
Record amark : Type := MkMark { mk_gen : nat; mk_cur : nat }.

(* The structural invariant an arena maintains: a live generation, a cursor
   inside the committed span, a span inside the committed ceiling. *)
Definition arena_wf (a : arena) : Prop :=
  agen a <> 0 /\ 1 <= acap a /\ acount a <= acap a /\ acap a <= aceil a.

(* IR-ARENA:NEW (arena.f:260-275).  The seed span is the ceiling capped at
   SEED-CELLS, and the ceiling itself must be at least one cell — that is
   IR-ARENA:CEIL-OK (arena.f:232-234). *)
Definition anew (g owner ceil : nat) : option arena :=
  if Nat.ltb ceil 1 then None
  else Some (MkArena g owner [] (Nat.min ceil seed_cells) ceil false).

(* IR-ARENA:GROW (arena.f:217-225).  The ceiling test comes FIRST, so a hit at
   the ceiling mutates nothing; then the capacity doubles, capped at the
   ceiling.  `arena_grow_ceiling_fail_closed` is that ordering as a theorem and
   `arena_grow_write_first_leaves_partial_row` is the mutation that shows it is
   load-bearing. *)
Definition agrow (a : arena) : option arena :=
  if Nat.leb (aceil a) (acap a) then None
  else Some (MkArena (agen a) (aowner a) (cells a)
                     (Nat.min (2 * acap a) (aceil a)) (aceil a) (afrozen a)).

(* IR-ARENA:PUSH (arena.f:280-288).  A frozen builder is refused by
   IR-ARENA:LIVE-SLOT (arena.f:180-182), a foreign context by
   IR-ARENA:OWN-CHECK (arena.f:227-229), and the span is grown only when the
   cursor has reached it. *)
Definition apush (owner : nat) (a : arena) (v : nat) : option (arena * nat) :=
  if afrozen a then None
  else if negb (Nat.eqb owner (aowner a)) then None
  else
    let g := if Nat.ltb (acount a) (acap a) then Some a else agrow a in
    match g with
    | None => None
    | Some a' =>
        Some (MkArena (agen a') (aowner a') (cells a' ++ [v])
                      (acap a') (aceil a') (afrozen a'),
              acount a')
    end.

(* The state-passing form, so "the arena is unchanged on failure" is a
   statement about an arena and not about the absence of one. *)
Definition apush_step (owner : nat) (a : arena) (v : nat) : arena * option nat :=
  match apush owner a v with
  | Some (a', i) => (a', Some i)
  | None => (a, None)
  end.

(* IR-ARENA:NTH (arena.f:302-303) through IR-ARENA:NTH-RAW: bounds first, then
   stamp this arena's own generation.  The one sanctioned raw-to-index
   crossing. *)
Definition amint (a : arena) (k : nat) : option cell_id :=
  if Nat.ltb k (acount a) then Some (MkIdx (agen a) k) else None.

(* IR-ARENA:IDX-AT (arena.f:197-201): the generation must be this arena's, and
   the ordinal must be inside the readable count.  IR-ARENA:PEEK adds the live
   check (arena.f:291-295) and IR-ARENA:AT the frozen one (arena.f:349-353). *)
Definition aread (a : arena) (x : cell_id) : option nat :=
  if negb (Nat.eqb (idx_gen x) (agen a)) then None
  else nth_error (cells a) (idx_ord x).

Definition apeek (a : arena) (x : cell_id) : option nat :=
  if afrozen a then None else aread a x.

Definition aat (a : arena) (x : cell_id) : option nat :=
  if afrozen a then aread a x else None.

(* IR-ARENA:MARK (arena.f:317-319) and IR-ARENA:ROLLBACK (arena.f:323-330).
   A mark from another arena and a cursor past the live count both reject. *)
Definition amark_of (a : arena) : option amark :=
  if afrozen a then None else Some (MkMark (agen a) (acount a)).

Definition arollback (a : arena) (m : amark) : option arena :=
  if afrozen a then None
  else if negb (Nat.eqb (mk_gen m) (agen a)) then None
  else if Nat.ltb (acount a) (mk_cur m) then None
  else Some (MkArena (agen a) (aowner a) (firstn (mk_cur m) (cells a))
                     (acap a) (aceil a) (afrozen a)).

(* IR-ARENA:FREEZE (arena.f:336-339) and IR-ARENA:ABORT (arena.f:344-346). *)
Definition afreeze (a : arena) : option arena :=
  if afrozen a then None
  else Some (MkArena (agen a) (aowner a) (cells a) (acap a) (aceil a) true).

Definition aabort (a : arena) : option arena :=
  if afrozen a then None
  else Some (MkArena 0 (aowner a) (cells a) (acap a) (aceil a) (afrozen a)).

(* ---- list facts used below, proved rather than assumed ---------------- *)

Lemma nth_error_firstn_lt :
  forall (A : Type) (l : list A) (n i : nat),
    i < n -> nth_error (firstn n l) i = nth_error l i.
Proof.
  intros A l.
  induction l as [|x xs IH]; intros n i Hlt.
  - destruct n; simpl; [lia |].
    destruct i; reflexivity.
  - destruct n as [|n']; [lia |].
    simpl.
    destruct i as [|i'].
    + reflexivity.
    + apply IH.
      lia.
Qed.

Lemma nth_error_firstn_ge :
  forall (A : Type) (l : list A) (n i : nat),
    n <= i -> nth_error (firstn n l) i = None.
Proof.
  intros A l n i Hge.
  apply nth_error_None.
  rewrite length_firstn.
  lia.
Qed.

Lemma apush_cases :
  forall owner a v a' i,
    apush owner a v = Some (a', i) ->
    afrozen a = false
    /\ owner = aowner a
    /\ i = acount a
    /\ cells a' = cells a ++ [v]
    /\ agen a' = agen a
    /\ aowner a' = aowner a
    /\ aceil a' = aceil a
    /\ afrozen a' = afrozen a
    /\ ((acount a < acap a /\ acap a' = acap a)
        \/ (acap a <= acount a
            /\ acap a < aceil a
            /\ acap a' = Nat.min (2 * acap a) (aceil a))).
Proof.
  intros owner a v a' i Hpush.
  unfold apush in Hpush.
  destruct (afrozen a) eqn:Hfrozen; [discriminate |].
  destruct (Nat.eqb owner (aowner a)) eqn:Howner; simpl in Hpush;
    [| discriminate].
  apply Nat.eqb_eq in Howner.
  destruct (Nat.ltb (acount a) (acap a)) eqn:Hroom.
  - apply Nat.ltb_lt in Hroom.
    inversion Hpush; subst a' i.
    simpl.
    repeat split; try reflexivity; try assumption.
    left.
    split; [exact Hroom | reflexivity].
  - apply Nat.ltb_ge in Hroom.
    unfold agrow in Hpush.
    destruct (Nat.leb (aceil a) (acap a)) eqn:Hfull; [discriminate |].
    apply Nat.leb_gt in Hfull.
    inversion Hpush; subst a' i.
    simpl.
    repeat split; try reflexivity; try assumption.
    right.
    repeat split; assumption.
Qed.

(* ---- 1. APPEND-ONLY --------------------------------------------------- *)

(* Appending never changes an existing cell: the old cells are a prefix of the
   new ones.  IR-ARENA:PUSH writes at the cursor and then advances it
   (arena.f:286-287); no word in the package overwrites a published ordinal.
   That the new cell list is the old one with `v` appended is one clause of
   `apush_cases`, which is the unfolding of `apush` itself, so it is machinery
   rather than a published result: what is observable, and what the theorems
   below publish, is that every earlier index still reads its own cell. *)

Theorem arena_push_answers_the_cursor :
  forall owner a v a' i,
    apush owner a v = Some (a', i) -> i = acount a.
Proof.
  intros owner a v a' i Hpush.
  destruct (apush_cases owner a v a' i Hpush) as [_ [_ [Hi _]]].
  exact Hi.
Qed.

(* Every previously valid index stays valid and denotes the same cell. *)
Theorem arena_push_preserves_reads :
  forall owner a v a' i x n,
    apush owner a v = Some (a', i) ->
    aread a x = Some n ->
    aread a' x = Some n.
Proof.
  intros owner a v a' i x n Hpush Hread.
  destruct (apush_cases owner a v a' i Hpush)
    as [_ [_ [_ [Hcells [Hgen _]]]]].
  unfold aread in *.
  rewrite Hgen, Hcells.
  destruct (Nat.eqb (idx_gen x) (agen a)); simpl in *; [| discriminate].
  rewrite nth_error_app1.
  - exact Hread.
  - apply nth_error_Some.
    rewrite Hread.
    discriminate.
Qed.

Theorem arena_push_index_stays_valid :
  forall owner a v a' i k x,
    apush owner a v = Some (a', i) ->
    amint a k = Some x ->
    amint a' k = Some x.
Proof.
  intros owner a v a' i k x Hpush Hmint.
  destruct (apush_cases owner a v a' i Hpush)
    as [_ [_ [_ [Hcells [Hgen _]]]]].
  unfold amint, acount in *.
  rewrite Hgen, Hcells.
  destruct (Nat.ltb k (length (cells a))) eqn:Hlt; [| discriminate].
  apply Nat.ltb_lt in Hlt.
  replace (Nat.ltb k (length (cells a ++ [v]))) with true.
  - exact Hmint.
  - symmetry.
    apply Nat.ltb_lt.
    rewrite length_app.
    simpl.
    lia.
Qed.

(* ---- 2. GROWTH TRANSPARENCY ------------------------------------------- *)

(* Doubling the span changes the capacity and nothing a reader can see.
   IR-ARENA:GROW copies every published ordinal into the new span and installs
   it (arena.f:222-225); the model has no bytes, so this is the statement that
   the observable table is untouched. *)
Theorem arena_grow_preserves_reads :
  forall a a' x,
    agrow a = Some a' -> aread a' x = aread a x.
Proof.
  intros a a' x Hgrow.
  unfold agrow in Hgrow.
  destruct (Nat.leb (aceil a) (acap a)); [discriminate |].
  inversion Hgrow; subst a'.
  unfold aread.
  reflexivity.
Qed.

Theorem arena_grow_preserves_count :
  forall a a', agrow a = Some a' -> acount a' = acount a.
Proof.
  intros a a' Hgrow.
  unfold agrow in Hgrow.
  destruct (Nat.leb (aceil a) (acap a)); [discriminate |].
  inversion Hgrow; subst a'.
  reflexivity.
Qed.

(* The observable table does not depend on the growth schedule at all: two
   arenas that differ only in their committed span capacity answer every index
   the same way.  This is why MODEL GAP 5 — not charging growth to the
   context — costs the reader nothing. *)
Theorem arena_reads_ignore_capacity :
  forall g owner cs cap1 cap2 ceil1 ceil2 fz x,
    aread (MkArena g owner cs cap1 ceil1 fz) x
    = aread (MkArena g owner cs cap2 ceil2 fz) x.
Proof.
  intros g owner cs cap1 cap2 ceil1 ceil2 fz x.
  unfold aread.
  reflexivity.
Qed.

(* ---- 3. CEILING FAIL-CLOSED ------------------------------------------- *)

(* At the ceiling the growth step refuses and the arena is untouched:
   IR-ARENA:GROW throws E-IR-ARENA-FULL as its FIRST act (arena.f:220), before
   the scratch take, the copy, or the two field writes. *)
Theorem arena_grow_ceiling_fail_closed :
  forall a, aceil a <= acap a -> agrow a = None.
Proof.
  intros a Hfull.
  unfold agrow.
  replace (Nat.leb (aceil a) (acap a)) with true.
  - reflexivity.
  - symmetry.
    apply Nat.leb_le.
    exact Hfull.
Qed.

(* And therefore the push at the ceiling leaves the arena bit for bit as it
   was: no cell, no partial row, no cursor move.  arena.f:31-33 states this as
   "E-IR-ARENA-FULL is thrown before any mutation, so a full arena stays
   usable". *)
Theorem arena_push_at_ceiling_fail_closed :
  forall owner a v,
    afrozen a = false ->
    owner = aowner a ->
    acap a <= acount a ->
    aceil a <= acap a ->
    apush_step owner a v = (a, None).
Proof.
  intros owner a v Hfrozen Howner Hfilled Hfull.
  unfold apush_step, apush.
  rewrite Hfrozen.
  subst owner.
  rewrite Nat.eqb_refl.
  simpl.
  replace (Nat.ltb (acount a) (acap a)) with false.
  - rewrite (arena_grow_ceiling_fail_closed a Hfull).
    reflexivity.
  - symmetry.
    apply Nat.ltb_ge.
    exact Hfilled.
Qed.

(* "Whenever a push fails the arena that survives is the arena that went in"
   is NOT published here.  `apush_step` is DEFINED to answer `(a, None)` on a
   `None`, so that statement holds of any push whatever, including one that
   corrupts the arena before refusing.  What carries the claim is
   `arena_push_at_ceiling_fail_closed` above, which names the arena the caller
   started with, and `arena_write_first_leaves_partial_row` below, which shows
   a push that checks late does NOT satisfy it.  For the same reason "a full
   arena still reads" is not published: it is that same definitional identity
   composed with `aread`. *)

(* The invariant survives every accepted push, so no arena this package can
   build ever holds more cells than its committed ceiling. *)
Theorem arena_push_preserves_wf :
  forall owner a v a' i,
    arena_wf a -> apush owner a v = Some (a', i) -> arena_wf a'.
Proof.
  intros owner a v a' i [Hgen [Hcapnz [Hcursor Hspan]]] Hpush.
  destruct (apush_cases owner a v a' i Hpush)
    as [_ [_ [_ [Hcells [Hgen' [_ [Hceil [_ Hcap]]]]]]]].
  unfold arena_wf, acount in *.
  rewrite Hgen', Hceil, Hcells, length_app.
  simpl.
  destruct Hcap as [[Hroom Hsame] | [Hfilled [Hbelow Hgrown]]].
  - rewrite Hsame.
    repeat split; [exact Hgen | lia | lia | lia].
  - rewrite Hgrown.
    assert (Hmin : Nat.min (2 * acap a) (aceil a) >= acap a + 1)
      by (apply Nat.min_glb; lia).
    repeat split; [exact Hgen | lia | lia |].
    apply Nat.le_min_r.
Qed.

Lemma arena_count_never_exceeds_ceiling :
  forall a, arena_wf a -> acount a <= aceil a.
Proof.
  intros a [_ [_ [Hcursor Hspan]]].
  lia.
Qed.

(* The published form is about the push rather than about the invariant on its
   own: an accepted append can never leave the cursor above the ceiling the
   caller committed to.  Stated this way it rules out a growth step that
   doubles past the ceiling instead of capping at it, which the invariant
   alone, being a hypothesis, does not. *)
Theorem arena_push_keeps_count_under_ceiling :
  forall owner a v a' i,
    arena_wf a -> apush owner a v = Some (a', i) -> acount a' <= aceil a'.
Proof.
  intros owner a v a' i Hwf Hpush.
  apply arena_count_never_exceeds_ceiling.
  apply (arena_push_preserves_wf owner a v a' i Hwf Hpush).
Qed.

(* The write-before-check mutation, to show the ordering inside IR-ARENA:GROW
   is load-bearing rather than stylistic: this variant installs the doubled
   capacity first and tests the ceiling afterwards, so a refused growth leaves
   a capacity behind that the ceiling never authorised. *)
Definition agrow_write_first (a : arena) : arena * option arena :=
  let a' := MkArena (agen a) (aowner a) (cells a)
                    (2 * acap a) (aceil a) (afrozen a) in
  if Nat.leb (aceil a) (acap a) then (a', None) else (a', Some a').

(* And the push built on it: the cell is appended before the ceiling is
   consulted, so a refused push leaves a row behind. *)
Definition apush_write_first (owner : nat) (a : arena) (v : nat)
  : arena * option nat :=
  if afrozen a then (a, None)
  else if negb (Nat.eqb owner (aowner a)) then (a, None)
  else
    let a' := MkArena (agen a) (aowner a) (cells a ++ [v])
                      (acap a) (aceil a) (afrozen a) in
    if Nat.ltb (acount a) (acap a) then (a', Some (acount a))
    else if Nat.leb (aceil a) (acap a) then (a', None)
    else (a', Some (acount a)).

(* ---- 4. MARK AND ROLLBACK --------------------------------------------- *)

(* A mark is this arena's own cursor, stamped with this arena's generation. *)
Theorem arena_mark_is_own_cursor :
  forall a m,
    amark_of a = Some m -> mk_gen m = agen a /\ mk_cur m = acount a.
Proof.
  intros a m Hmark.
  unfold amark_of in Hmark.
  destruct (afrozen a); [discriminate |].
  inversion Hmark; subst m.
  split; reflexivity.
Qed.

(* Rollback truncates exactly to the mark's cursor. *)
Theorem arena_rollback_truncates :
  forall a m a',
    arollback a m = Some a' -> acount a' = mk_cur m.
Proof.
  intros a m a' Hroll.
  unfold arollback in Hroll.
  destruct (afrozen a); [discriminate |].
  destruct (Nat.eqb (mk_gen m) (agen a)); simpl in Hroll; [| discriminate].
  destruct (Nat.ltb (acount a) (mk_cur m)) eqn:Hstale; [discriminate |].
  apply Nat.ltb_ge in Hstale.
  inversion Hroll; subst a'.
  unfold acount in *.
  simpl.
  rewrite length_firstn.
  lia.
Qed.

(* Indices below the mark keep their values. *)
Theorem arena_rollback_keeps_below_mark :
  forall a m a' x n,
    arollback a m = Some a' ->
    idx_ord x < mk_cur m ->
    aread a x = Some n ->
    aread a' x = Some n.
Proof.
  intros a m a' x n Hroll Hbelow Hread.
  unfold arollback in Hroll.
  destruct (afrozen a); [discriminate |].
  destruct (Nat.eqb (mk_gen m) (agen a)); simpl in Hroll; [| discriminate].
  destruct (Nat.ltb (acount a) (mk_cur m)); [discriminate |].
  inversion Hroll; subst a'.
  unfold aread in *.
  simpl in *.
  destruct (Nat.eqb (idx_gen x) (agen a)); simpl in *; [| discriminate].
  rewrite nth_error_firstn_lt by exact Hbelow.
  exact Hread.
Qed.

(* An index minted above the mark is dead after the rollback. *)
Theorem arena_rollback_kills_above_mark :
  forall a m a' x,
    arollback a m = Some a' ->
    mk_cur m <= idx_ord x ->
    aread a' x = None.
Proof.
  intros a m a' x Hroll Habove.
  unfold arollback in Hroll.
  destruct (afrozen a); [discriminate |].
  destruct (Nat.eqb (mk_gen m) (agen a)); simpl in Hroll; [| discriminate].
  destruct (Nat.ltb (acount a) (mk_cur m)); [discriminate |].
  inversion Hroll; subst a'.
  unfold aread.
  simpl.
  destruct (Nat.eqb (idx_gen x) (agen a)); [| reflexivity].
  apply nth_error_firstn_ge.
  exact Habove.
Qed.

(* A mark minted by a different arena is refused before the cursor moves.
   IR-ARENA:ROLLBACK compares the mark's generation with this slot's
   (arena.f:327) and throws E-IR-ARENA-OWNER. *)
Theorem arena_rollback_foreign_mark_rejected :
  forall a m, mk_gen m <> agen a -> arollback a m = None.
Proof.
  intros a m Hforeign.
  unfold arollback.
  destruct (afrozen a); [reflexivity |].
  replace (Nat.eqb (mk_gen m) (agen a)) with false.
  - reflexivity.
  - symmetry.
    apply Nat.eqb_neq.
    exact Hforeign.
Qed.

(* A mark whose cursor is past the live count — one an earlier, deeper
   rollback already invalidated — is refused too (arena.f:329,
   E-IR-ARENA-MARK). *)
Theorem arena_rollback_stale_mark_rejected :
  forall a m, acount a < mk_cur m -> arollback a m = None.
Proof.
  intros a m Hstale.
  unfold arollback.
  destruct (afrozen a); [reflexivity |].
  destruct (Nat.eqb (mk_gen m) (agen a)); simpl; [| reflexivity].
  replace (Nat.ltb (acount a) (mk_cur m)) with true.
  - reflexivity.
  - symmetry.
    apply Nat.ltb_lt.
    exact Hstale.
Qed.

(* ---- 5. FREEZE --------------------------------------------------------- *)

(* A frozen view answers exactly what the live arena answered at the moment it
   was frozen.  IR-ARENA:FREEZE flips the state cell and mints a view over the
   SAME storage (arena.f:336-339). *)
Theorem arena_freeze_preserves_reads :
  forall a a' x,
    afreeze a = Some a' -> aat a' x = apeek a x.
Proof.
  intros a a' x Hfreeze.
  unfold afreeze in Hfreeze.
  destruct (afrozen a) eqn:Hfz; [discriminate |].
  inversion Hfreeze; subst a'.
  unfold aat, apeek, aread.
  simpl.
  rewrite Hfz.
  reflexivity.
Qed.

Theorem arena_freeze_preserves_count :
  forall a a', afreeze a = Some a' -> acount a' = acount a.
Proof.
  intros a a' Hfreeze.
  unfold afreeze in Hfreeze.
  destruct (afrozen a); [discriminate |].
  inversion Hfreeze; subst a'.
  reflexivity.
Qed.

(* No mutation is possible after a freeze.  Every builder word resolves through
   IR-ARENA:LIVE-SLOT, which throws E-IR-ARENA-FROZEN on a slot in state
   ST-FROZEN (arena.f:180-182). *)
Theorem arena_freeze_blocks_mutation :
  forall a a',
    afreeze a = Some a' ->
    (forall owner v, apush owner a' v = None)
    /\ (forall m, arollback a' m = None)
    /\ amark_of a' = None
    /\ afreeze a' = None
    /\ aabort a' = None.
Proof.
  intros a a' Hfreeze.
  unfold afreeze in Hfreeze.
  destruct (afrozen a); [discriminate |].
  inversion Hfreeze; subst a'.
  unfold apush, arollback, amark_of, afreeze, aabort.
  simpl.
  repeat split; intros; reflexivity.
Qed.

(* The live readers stop answering through the consumed builder handle, so a
   caller cannot keep reading through the value it gave up. *)
Theorem arena_freeze_retires_live_reader :
  forall a a' x, afreeze a = Some a' -> apeek a' x = None.
Proof.
  intros a a' x Hfreeze.
  unfold afreeze in Hfreeze.
  destruct (afrozen a); [discriminate |].
  inversion Hfreeze; subst a'.
  unfold apeek.
  reflexivity.
Qed.

(* ---- 6. CROSS-OWNER ---------------------------------------------------- *)

(* An index minted by one arena is refused by another.  IR-ARENA:IDX-AT
   compares the index's generation with the resolved slot's own
   (arena.f:199) and throws E-IR-ARENA-OWNER before any ordinal is used.
   Generations are unique across arenas because they come from one monotone
   counter (arena.f:144-160), which is the hypothesis here. *)
Theorem arena_cross_owner_rejects :
  forall a b k x,
    agen b <> agen a ->
    amint a k = Some x ->
    aread b x = None.
Proof.
  intros a b k x Hdistinct Hmint.
  unfold amint in Hmint.
  destruct (Nat.ltb k (acount a)); [| discriminate].
  inversion Hmint; subst x.
  unfold aread.
  simpl.
  replace (Nat.eqb (agen a) (agen b)) with false.
  - reflexivity.
  - symmetry.
    apply Nat.eqb_neq.
    intros Heq.
    apply Hdistinct.
    symmetry.
    exact Heq.
Qed.

(* A retired slot — IR-ARENA:ABORT, and IR-ARENA:RESOLVE retiring a slot whose
   owner died (arena.f:176-177, 346) — writes generation 0, and no minted index
   ever carries a zero generation, so every index of an aborted arena is dead. *)
Theorem arena_abort_kills_every_index :
  forall a a' k x,
    arena_wf a ->
    amint a k = Some x ->
    aabort a = Some a' ->
    aread a' x = None.
Proof.
  intros a a' k x [Hgen _] Hmint Habort.
  unfold amint in Hmint.
  destruct (Nat.ltb k (acount a)); [| discriminate].
  inversion Hmint; subst x.
  unfold aabort in Habort.
  destruct (afrozen a); [discriminate |].
  inversion Habort; subst a'.
  unfold aread.
  simpl.
  replace (Nat.eqb (agen a) 0) with false.
  - reflexivity.
  - symmetry.
    apply Nat.eqb_neq.
    exact Hgen.
Qed.

(* ================================================================== *)
(* IR-CTX                                                              *)
(* ================================================================== *)

(* A live context: its generation, and the base of the mapping that backs it.
   IR-CTX:GENS and IR-CTX:BASES (context.f:111-112). *)
Record ctx : Type := MkCtx { cgen : nat; cbase : nat }.

(* The whole registry.  `counter` is IR-CTX:GEN-CELL and `reg` is the
   IR-CTX:GENS/BASES stack, innermost last, so `length reg` is IR-CTX:DEPTH
   (context.f:107-112, 160). *)
Record cstate : Type := MkCState { counter : nat; reg : list ctx }.

(* IR-CTX:GEN-NEXT-N (context.f:138-140).  Nonzero because the counter starts
   at zero and the answer is its successor; bounded by GEN-MAX. *)
Definition ctake_gen (c : nat) : option nat :=
  if Nat.ltb c gen_max then Some (S c) else None.

(* IR-CTX:FIND-SLOT and IR-CTX:SERIAL-LIVE? (context.f:157-167, 363-364): a
   linear scan of the live depth only. *)
Definition cserial_live (s : cstate) (g : nat) : bool :=
  existsb (fun c => Nat.eqb (cgen c) g) (reg s).

(* IR-CTX:CTX-ENTER (context.f:321-330), up to the point the body runs:
   DEPTH-ROOM, then the generation, then the install, then the depth bump. *)
Definition center (s : cstate) (base : nat) : option (cstate * ctx) :=
  if Nat.leb depth_max (length (reg s)) then None
  else
    match ctake_gen (counter s) with
    | None => None
    | Some g =>
        let c := MkCtx g base in
        Some (MkCState g (reg s ++ [c]), c)
    end.

(* The normal-exit tail of the same word: `0 at GEN!` then `at DEPTH !`
   (context.f:329-330).  Truncating to the depth saved at entry retires this
   context AND every child context and arena registered inside it, in one
   step. *)
Definition cleave (s : cstate) (depth_at_entry : nat) : cstate :=
  MkCState (counter s) (firstn depth_at_entry (reg s)).

(* The throw path of the same word.  MEM:WITH-BYTES releases the mapping, but
   neither of those two lines runs, so the registry is left exactly as the body
   left it.  This is FINDING 1. *)
Definition cabandon (s : cstate) : cstate := s.

(* The mutation that shows the truncation is load-bearing: an exit that retires
   only its own slot and leaves the depth alone. *)
Definition cleave_no_truncate (s : cstate) (depth_at_entry : nat) : cstate :=
  MkCState (counter s)
           (firstn depth_at_entry (reg s)
            ++ skipn (S depth_at_entry) (reg s)).

(* ---- 7. GENERATION UNIQUENESS ----------------------------------------- *)

(* The counter only ever moves up, and every issued generation is its new
   value, so generations are nonzero, strictly increasing and never reused. *)
Theorem ctx_gen_nonzero :
  forall c g, ctake_gen c = Some g -> g <> 0.
Proof.
  intros c g Htake.
  unfold ctake_gen in Htake.
  destruct (Nat.ltb c gen_max); [| discriminate].
  inversion Htake.
  lia.
Qed.

(* Machinery: the counter's step is strictly increasing.  The published form
   of "never reused" is `ctx_enter_raises_counter` and `ctx_gen_never_reused`,
   which say it of an entered context rather than of the bare successor. *)
Lemma ctx_gen_strictly_monotone :
  forall c g, ctake_gen c = Some g -> c < g.
Proof.
  intros c g Htake.
  unfold ctake_gen in Htake.
  destruct (Nat.ltb c gen_max); [| discriminate].
  inversion Htake.
  lia.
Qed.

Theorem ctx_enter_raises_counter :
  forall s base s' c,
    center s base = Some (s', c) -> counter s < counter s' /\ cgen c = counter s'.
Proof.
  intros s base s' c Henter.
  unfold center in Henter.
  destruct (Nat.leb depth_max (length (reg s))); [discriminate |].
  destruct (ctake_gen (counter s)) as [g |] eqn:Htake; [| discriminate].
  inversion Henter; subst s' c.
  simpl.
  split; [| reflexivity].
  apply (ctx_gen_strictly_monotone (counter s) g Htake).
Qed.

(* The registry invariant that makes "never reused" a statement about the
   registry and not only about the counter: every registered generation is at
   or below the counter. *)
Definition cstate_wf (s : cstate) : Prop :=
  forall c, In c (reg s) -> cgen c <> 0 /\ cgen c <= counter s.

Theorem ctx_enter_preserves_wf :
  forall s base s' c,
    cstate_wf s -> center s base = Some (s', c) -> cstate_wf s'.
Proof.
  intros s base s' c Hwf Henter.
  unfold center in Henter.
  destruct (Nat.leb depth_max (length (reg s))); [discriminate |].
  destruct (ctake_gen (counter s)) as [g |] eqn:Htake; [| discriminate].
  inversion Henter; subst s' c.
  pose proof (ctx_gen_nonzero (counter s) g Htake) as Hnz.
  pose proof (ctx_gen_strictly_monotone (counter s) g Htake) as Hlt.
  intros c' Hin.
  simpl in Hin.
  apply in_app_iff in Hin as [Hin | Hin].
  - destruct (Hwf c' Hin) as [Hnz' Hle].
    simpl.
    split; [exact Hnz' | lia].
  - simpl in Hin.
    destruct Hin as [Heq | Hfalse]; [| contradiction].
    subst c'.
    simpl.
    split; [exact Hnz | lia].
Qed.

(* A freshly minted generation was not already registered, so no two live
   contexts ever share one. *)
Theorem ctx_gen_never_reused :
  forall s base s' c,
    cstate_wf s -> center s base = Some (s', c) -> cserial_live s (cgen c) = false.
Proof.
  intros s base s' c Hwf Henter.
  pose proof Henter as Henter'.
  unfold center in Henter'.
  destruct (Nat.leb depth_max (length (reg s))); [discriminate |].
  destruct (ctake_gen (counter s)) as [g |] eqn:Htake; [| discriminate].
  inversion Henter'; subst s' c.
  pose proof (ctx_gen_strictly_monotone (counter s) g Htake) as Hlt.
  simpl.
  unfold cserial_live.
  apply Bool.not_true_is_false.
  intros Hlive.
  apply existsb_exists in Hlive as [c' [Hin Heq]].
  apply Nat.eqb_eq in Heq.
  destruct (Hwf c' Hin) as [_ Hle].
  simpl in Hle.
  lia.
Qed.

(* ---- 8. STALE REJECTION ------------------------------------------------ *)

(* A handle that is not in the registry is refused.  IR-CTX:RESOLVE throws
   E-IR-CTX-STALE on a miss, before any mapping pointer is produced
   (context.f:164-167). *)
Definition cresolve (s : cstate) (c : ctx) : option nat :=
  if cserial_live s (cgen c) then Some (cbase c) else None.

(* Machinery, not a published result: `cresolve` is DEFINED to answer `None`
   off a dead serial, so on its own this says nothing about IR-CTX.  The
   published statement is `ctx_leave_kills_the_mapping_handle` below, which
   composes it with the teardown and so names a handle a caller could actually
   still be holding. *)
Lemma ctx_stale_handle_rejected :
  forall s c, cserial_live s (cgen c) = false -> cresolve s c = None.
Proof.
  intros s c Hdead.
  unfold cresolve.
  rewrite Hdead.
  reflexivity.
Qed.

Theorem ctx_enter_registers_live :
  forall s base s' c,
    center s base = Some (s', c) -> cserial_live s' (cgen c) = true.
Proof.
  intros s base s' c Henter.
  unfold center in Henter.
  destruct (Nat.leb depth_max (length (reg s))); [discriminate |].
  destruct (ctake_gen (counter s)) as [g |]; [| discriminate].
  inversion Henter; subst s' c.
  unfold cserial_live.
  simpl.
  apply existsb_exists.
  exists (MkCtx g base).
  split.
  - apply in_app_iff.
    right.
    left.
    reflexivity.
  - simpl.
    apply Nat.eqb_refl.
Qed.

(* ---- 9. NESTING AND TEARDOWN ------------------------------------------ *)

(* Leaving truncates the registry to the depth saved at entry, so every context
   registered at or beyond that depth — this one and every child — becomes
   stale in one step.  This is the whole-range release described in
   context.f:26-28.

   That the registry after a leave IS `firstn d` of the registry before it is
   the body of `cleave`, so it is not published; what is published is what a
   caller can observe through it — that nothing at or beyond the saved depth
   answers a liveness probe or resolves to a mapping any more. *)
Lemma ctx_leave_kills_from_depth :
  forall s d c i,
    nth_error (reg s) i = Some c ->
    d <= i ->
    (forall c', In c' (firstn d (reg s)) -> cgen c' <> cgen c) ->
    cserial_live (cleave s d) (cgen c) = false.
Proof.
  intros s d c i Hnth Hdepth Hfresh.
  unfold cserial_live, cleave.
  simpl.
  apply Bool.not_true_is_false.
  intros Hlive.
  apply existsb_exists in Hlive as [c' [Hin Heq]].
  apply Nat.eqb_eq in Heq.
  apply (Hfresh c' Hin).
  exact Heq.
Qed.

(* The clean statement of the same thing, using the registry invariant that
   generations are distinct: nothing entered at or beyond the saved depth
   survives the leave.  A child context and every arena registered under it
   therefore die together with their parent. *)
Definition cgens_distinct (s : cstate) : Prop :=
  forall i j ci cj,
    nth_error (reg s) i = Some ci ->
    nth_error (reg s) j = Some cj ->
    cgen ci = cgen cj -> i = j.

Theorem ctx_no_handle_survives_owner_exit :
  forall s d c i,
    cgens_distinct s ->
    nth_error (reg s) i = Some c ->
    d <= i ->
    cserial_live (cleave s d) (cgen c) = false.
Proof.
  intros s d c i Hdistinct Hnth Hdepth.
  apply (ctx_leave_kills_from_depth s d c i Hnth Hdepth).
  intros c' Hin.
  apply In_nth_error in Hin as [j Hj].
  assert (Hjlt : j < d).
  { assert (Hlen : j < length (firstn d (reg s))).
    { apply nth_error_Some.
      rewrite Hj.
      discriminate. }
    rewrite length_firstn in Hlen.
    lia. }
  rewrite nth_error_firstn_lt in Hj by exact Hjlt.
  intros Heq.
  pose proof (Hdistinct j i c' c Hj Hnth Heq) as Hji.
  lia.
Qed.

(* And therefore the handle stops answering with a mapping base, which is the
   fact the layer actually rests on: IR-CTX:RESOLVE throws E-IR-CTX-STALE
   rather than handing back a pointer into storage the leave released
   (context.f:164-167).  Neither `ctx_stale_handle_rejected` nor
   `ctx_no_handle_survives_owner_exit` says this on its own — the first is the
   definition of `cresolve` and the second stops at the liveness bit. *)
Theorem ctx_leave_kills_the_mapping_handle :
  forall s d c i,
    cgens_distinct s ->
    nth_error (reg s) i = Some c ->
    d <= i ->
    cresolve (cleave s d) c = None.
Proof.
  intros s d c i Hdistinct Hnth Hdepth.
  apply ctx_stale_handle_rejected.
  apply (ctx_no_handle_survives_owner_exit s d c i Hdistinct Hnth Hdepth).
Qed.

(* Entering is bounded: IR-CTX:DEPTH-ROOM throws E-IR-CTX-DEPTH rather than
   writing past the registry (context.f:299-300). *)
Theorem ctx_depth_bounded :
  forall s base, depth_max <= length (reg s) -> center s base = None.
Proof.
  intros s base Hfull.
  unfold center.
  replace (Nat.leb depth_max (length (reg s))) with true.
  - reflexivity.
  - symmetry.
    apply Nat.leb_le.
    exact Hfull.
Qed.

Theorem ctx_enter_keeps_depth_bounded :
  forall s base s' c,
    center s base = Some (s', c) -> length (reg s') <= depth_max.
Proof.
  intros s base s' c Henter.
  unfold center in Henter.
  destruct (Nat.leb depth_max (length (reg s))) eqn:Hroom; [discriminate |].
  apply Nat.leb_gt in Hroom.
  destruct (ctake_gen (counter s)) as [g |]; [| discriminate].
  inversion Henter; subst s' c.
  simpl.
  rewrite length_app.
  simpl.
  lia.
Qed.

(* ---- the nesting the depth vector rows drive --------------------------- *)

(* Opening one context inside another, starting from an empty registry.
   `dnest` answers nothing at all if any entry on the way in is refused, so
   `dprobe n = Some b` already carries the claim that all n entries were
   accepted, and b is what the entry after them does.  These are the shape
   test/compiler/ir-storage-obligations.f asks Rocq about, on the same frozen
   rows test/compiler/ir-storage-cases.f opens for real through
   IR-CTX:WITH-CONTEXT. *)
Fixpoint dnest (s : cstate) (n : nat) : option cstate :=
  match n with
  | 0 => Some s
  | S k =>
      match center s 0 with
      | Some (s', _) => dnest s' k
      | None => None
      end
  end.

Definition dprobe (n : nat) : option bool :=
  match dnest (MkCState 0 []) n with
  | None => None
  | Some s =>
      match center s 0 with
      | Some _ => Some true
      | None => Some false
      end
  end.

(* Below the registry depth every entry still has a generation to take.  The
   generation ceiling is two billion and this model counts in `nat`, so
   `depth_max < gen_max` is carried as a hypothesis rather than computed; see
   MODEL GAP 11. *)
Lemma ctake_gen_below_depth :
  forall c, depth_max < gen_max -> c <= depth_max -> ctake_gen c = Some (S c).
Proof.
  intros c Hroom Hle.
  unfold ctake_gen.
  replace (Nat.ltb c gen_max) with true; [reflexivity |].
  symmetry.
  apply Nat.ltb_lt.
  lia.
Qed.

(* Machinery: below the registry depth every entry is accepted, and each one
   spends exactly one generation and one registry slot. *)
Lemma dnest_shape :
  forall n s,
    depth_max < gen_max ->
    length (reg s) + n <= depth_max ->
    counter s + n <= depth_max ->
    exists s', dnest s n = Some s'
               /\ counter s' = counter s + n
               /\ length (reg s') = length (reg s) + n.
Proof.
  induction n as [| k IH]; intros s Hroom Hlen Hcnt.
  - exists s.
    simpl.
    repeat split; lia.
  - simpl.
    unfold center.
    destruct (Nat.leb depth_max (length (reg s))) eqn:Hfull.
    + apply Nat.leb_le in Hfull.
      lia.
    + rewrite ctake_gen_below_depth by lia.
      destruct (IH (MkCState (S (counter s))
                             (reg s ++ [MkCtx (S (counter s)) 0])))
        as [s' [H1 [H2 H3]]].
      * exact Hroom.
      * simpl.
        rewrite length_app.
        simpl.
        lia.
      * simpl.
        lia.
      * exists s'.
        simpl in H2, H3.
        rewrite length_app in H3.
        simpl in H3.
        rewrite H1.
        repeat split; lia.
Qed.

(* The published form: nesting from empty never stalls before the registry
   depth, and the entry that follows exactly `n` of them is accepted precisely
   while n is still below that depth.  IR-CTX:DEPTH-ROOM is the whole content
   here, so this is the statement the two depth vector rows instantiate. *)
Theorem ctx_nesting_stops_at_depth_max :
  forall n,
    depth_max < gen_max ->
    n <= depth_max ->
    dprobe n = Some (Nat.ltb n depth_max).
Proof.
  intros n Hroom Hle.
  unfold dprobe.
  destruct (dnest_shape n (MkCState 0 []) Hroom) as [s [H1 [H2 H3]]];
    simpl; try lia.
  simpl in H2, H3.
  rewrite H1.
  unfold center.
  rewrite H3.
  destruct (Nat.leb depth_max n) eqn:Hfull.
  - apply Nat.leb_le in Hfull.
    replace (Nat.ltb n depth_max) with false; [reflexivity |].
    symmetry.
    apply Nat.ltb_ge.
    exact Hfull.
  - apply Nat.leb_gt in Hfull.
    rewrite H2.
    rewrite ctake_gen_below_depth by lia.
    replace (Nat.ltb n depth_max) with true; [reflexivity |].
    symmetry.
    apply Nat.ltb_lt.
    exact Hfull.
Qed.

(* ---- 10. SCRATCH MONOTONICITY ----------------------------------------- *)

(* IR-CTX:ALIGN8 (context.f:397-398).  The cursor always advances by at least
   the requested size, which is what makes two spans disjoint. *)
Definition align8 (n : nat) : nat := 8 * ((n + 7) / 8).

Lemma align8_ge : forall n, n <= align8 n.
Proof.
  intros n.
  unfold align8.
  pose proof (Nat.div_mod_eq (n + 7) 8) as Heq.
  assert (Hmod : (n + 7) mod 8 < 8) by (apply Nat.mod_upper_bound; lia).
  lia.
Qed.

Lemma align8_le : forall n, align8 n <= n + 7.
Proof.
  intros n.
  unfold align8.
  pose proof (Nat.div_mod_eq (n + 7) 8) as Heq.
  lia.
Qed.

(* IR-CTX:SCRATCH-TAKE (context.f:404-413).  `off` is the cursor as a byte
   offset from the mapping base, so it starts at HDR-BYTES.  The answer is the
   span's start and the new cursor.  A zero or negative size is
   E-IR-CTX-SIZE, a size past the region or a cursor that would leave the
   mapping is E-IR-CTX-SCRATCH, and neither moves the cursor. *)
Definition scratch_take (off need : nat) : option (nat * nat) :=
  if Nat.ltb need 1 then None
  else if Nat.ltb scratch_cap need then None
  else if Nat.ltb map_bytes (off + align8 need) then None
  else Some (off, off + align8 need).

(* Machinery: the two published span facts below are what a caller can see. *)
Lemma scratch_take_advances :
  forall off need start off',
    scratch_take off need = Some (start, off') ->
    start = off /\ off + need <= off'.
Proof.
  intros off need start off' Htake.
  unfold scratch_take in Htake.
  destruct (Nat.ltb need 1); [discriminate |].
  destruct (Nat.ltb scratch_cap need); [discriminate |].
  destruct (Nat.ltb map_bytes (off + align8 need)); [discriminate |].
  inversion Htake; subst start off'.
  pose proof (align8_ge need).
  split; [reflexivity | lia].
Qed.

(* Two consecutive takes never overlap: the second span starts at or after the
   end of the first. *)
Theorem scratch_spans_disjoint :
  forall off need1 start1 off1 need2 start2 off2,
    scratch_take off need1 = Some (start1, off1) ->
    scratch_take off1 need2 = Some (start2, off2) ->
    start1 + need1 <= start2.
Proof.
  intros off need1 start1 off1 need2 start2 off2 H1 H2.
  destruct (scratch_take_advances off need1 start1 off1 H1) as [Hs1 Hoff1].
  destruct (scratch_take_advances off1 need2 start2 off2 H2) as [Hs2 _].
  lia.
Qed.

(* The cursor never leaves the mapping, and neither does the span it hands
   back.  Only the span statement is published: the cursor bound is the
   `Nat.ltb map_bytes` test read straight back off `scratch_take`, while the
   span bound is what a caller who writes into the span depends on. *)
Lemma scratch_cursor_within_mapping :
  forall off need start off',
    scratch_take off need = Some (start, off') -> off' <= map_bytes.
Proof.
  intros off need start off' Htake.
  unfold scratch_take in Htake.
  destruct (Nat.ltb need 1); [discriminate |].
  destruct (Nat.ltb scratch_cap need); [discriminate |].
  destruct (Nat.ltb map_bytes (off + align8 need)) eqn:Hover; [discriminate |].
  apply Nat.ltb_ge in Hover.
  inversion Htake; subst start off'.
  exact Hover.
Qed.

Theorem scratch_span_within_mapping :
  forall off need start off',
    scratch_take off need = Some (start, off') -> start + need <= map_bytes.
Proof.
  intros off need start off' Htake.
  pose proof (scratch_take_advances off need start off' Htake) as [Hs Hle].
  pose proof (scratch_cursor_within_mapping off need start off' Htake) as Hcap.
  lia.
Qed.

(* Exhaustion is refused and the cursor does not move.  The Habu word writes
   the new cursor only after both tests (context.f:411-412). *)
Theorem scratch_exhaustion_fail_closed :
  forall off need,
    map_bytes < off + align8 need -> scratch_take off need = None.
Proof.
  intros off need Hover.
  unfold scratch_take.
  destruct (Nat.ltb need 1); [reflexivity |].
  destruct (Nat.ltb scratch_cap need); [reflexivity |].
  replace (Nat.ltb map_bytes (off + align8 need)) with true;
    [reflexivity |].
  symmetry.
  apply Nat.ltb_lt.
  exact Hover.
Qed.

Theorem scratch_zero_size_rejected :
  forall off, scratch_take off 0 = None.
Proof.
  intros off.
  reflexivity.
Qed.

(* ---- module serial budget --------------------------------------------- *)

(* IR-CTX:MINT-TAKE (context.f:376-380): the count is reserved against this
   context's ceiling BEFORE the global identity is taken, so a context can
   never hold a module it did not account for. *)
Definition mint_take (minted ceil : nat) : option nat :=
  if Nat.leb ceil minted then None else Some (S minted).

Theorem mint_take_bounded :
  forall minted ceil minted',
    minted <= ceil -> mint_take minted ceil = Some minted' -> minted' <= ceil.
Proof.
  intros minted ceil minted' Hle Htake.
  unfold mint_take in Htake.
  destruct (Nat.leb ceil minted) eqn:Hfull; [discriminate |].
  apply Nat.leb_gt in Hfull.
  inversion Htake.
  lia.
Qed.

Theorem mint_take_at_ceiling_rejected :
  forall minted ceil, ceil <= minted -> mint_take minted ceil = None.
Proof.
  intros minted ceil Hfull.
  unfold mint_take.
  replace (Nat.leb ceil minted) with true; [reflexivity |].
  symmetry.
  apply Nat.leb_le.
  exact Hfull.
Qed.

(* ================================================================== *)
(* Executable examples, one group per property.                        *)
(* ================================================================== *)

(* A concrete arena: generation 7, owner serial 3, ceiling 4, seeded at 4
   because the ceiling is below SEED-CELLS. *)
Definition a0 : arena := MkArena 7 3 [] 4 4 false.

Example ex_new_seeds_below_ceiling :
  anew 7 3 4 = Some a0
  /\ anew 7 3 100 = Some (MkArena 7 3 [] 8 100 false)
  /\ anew 7 3 0 = None.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* 1. APPEND-ONLY. *)
Example ex_append_only :
  apush 3 a0 11 = Some (MkArena 7 3 [11] 4 4 false, 0)
  /\ apush 3 (MkArena 7 3 [11] 4 4 false) 22
       = Some (MkArena 7 3 [11; 22] 4 4 false, 1)
  /\ aread (MkArena 7 3 [11; 22] 4 4 false) (MkIdx 7 0) = Some 11.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* 2. GROWTH TRANSPARENCY: the cursor reaching the span doubles it, and every
   index reads the same value before and after. *)
Example ex_growth_transparent :
  agrow (MkArena 7 3 [1; 2; 3; 4] 4 16 false)
    = Some (MkArena 7 3 [1; 2; 3; 4] 8 16 false)
  /\ aread (MkArena 7 3 [1; 2; 3; 4] 4 16 false) (MkIdx 7 2) = Some 3
  /\ aread (MkArena 7 3 [1; 2; 3; 4] 8 16 false) (MkIdx 7 2) = Some 3
  /\ apush 3 (MkArena 7 3 [1; 2; 3; 4] 4 16 false) 5
       = Some (MkArena 7 3 [1; 2; 3; 4; 5] 8 16 false, 4).
Proof. repeat split; vm_compute; reflexivity. Qed.

(* The doubling stops at the ceiling rather than passing it. *)
Example ex_growth_caps_at_ceiling :
  agrow (MkArena 7 3 [1; 2; 3; 4; 5; 6] 6 10 false)
    = Some (MkArena 7 3 [1; 2; 3; 4; 5; 6] 10 10 false).
Proof. vm_compute; reflexivity. Qed.

(* 3. CEILING FAIL-CLOSED: at the ceiling the push refuses and the arena is
   unchanged, while reads and duplicates still work. *)
Example ex_ceiling_fail_closed :
  apush_step 3 (MkArena 7 3 [1; 2; 3; 4] 4 4 false) 5
    = (MkArena 7 3 [1; 2; 3; 4] 4 4 false, None)
  /\ aread (MkArena 7 3 [1; 2; 3; 4] 4 4 false) (MkIdx 7 3) = Some 4.
Proof. split; vm_compute; reflexivity. Qed.

(* COUNTEREXAMPLE 1.  Writing before the ceiling check leaves a partial row:
   the mutation answers the same refusal but the arena it leaves behind holds
   a fifth cell the ceiling never authorised. *)
Example arena_write_first_leaves_partial_row :
  apush_step 3 (MkArena 7 3 [1; 2; 3; 4] 4 4 false) 5
    = (MkArena 7 3 [1; 2; 3; 4] 4 4 false, None)
  /\ apush_write_first 3 (MkArena 7 3 [1; 2; 3; 4] 4 4 false) 5
       = (MkArena 7 3 [1; 2; 3; 4; 5] 4 4 false, None).
Proof. split; vm_compute; reflexivity. Qed.

(* And the same at the growth step: the mutation publishes a span capacity of
   eight cells against a committed ceiling of four. *)
Example arena_grow_write_first_leaves_partial_row :
  agrow (MkArena 7 3 [1; 2; 3; 4] 4 4 false) = None
  /\ fst (agrow_write_first (MkArena 7 3 [1; 2; 3; 4] 4 4 false))
       = MkArena 7 3 [1; 2; 3; 4] 8 4 false.
Proof. split; vm_compute; reflexivity. Qed.

(* 4. MARK AND ROLLBACK. *)
Definition a3 : arena := MkArena 7 3 [11; 22; 33] 4 8 false.

Example ex_rollback_truncates :
  amark_of a3 = Some (MkMark 7 3)
  /\ arollback (MkArena 7 3 [11; 22; 33; 44; 55] 8 8 false) (MkMark 7 3)
       = Some (MkArena 7 3 [11; 22; 33] 8 8 false)
  /\ aread (MkArena 7 3 [11; 22; 33] 8 8 false) (MkIdx 7 1) = Some 22
  /\ aread (MkArena 7 3 [11; 22; 33] 8 8 false) (MkIdx 7 3) = None.
Proof. repeat split; vm_compute; reflexivity. Qed.

Example ex_rollback_rejects :
  arollback a3 (MkMark 9 1) = None
  /\ arollback a3 (MkMark 7 5) = None.
Proof. split; vm_compute; reflexivity. Qed.

(* COUNTEREXAMPLE 2 — and FINDING 2.  Rollback does not bump the generation,
   so an index minted above a mark comes back to life when the cursor passes it
   again, and it then denotes a DIFFERENT cell with no diagnostic. *)
Example arena_rollback_reuses_ordinal :
  amint (MkArena 7 3 [11; 22] 4 8 false) 1 = Some (MkIdx 7 1)
  /\ aread (MkArena 7 3 [11; 22] 4 8 false) (MkIdx 7 1) = Some 22
  /\ arollback (MkArena 7 3 [11; 22] 4 8 false) (MkMark 7 1)
       = Some (MkArena 7 3 [11] 4 8 false)
  /\ aread (MkArena 7 3 [11] 4 8 false) (MkIdx 7 1) = None
  /\ apush 3 (MkArena 7 3 [11] 4 8 false) 99
       = Some (MkArena 7 3 [11; 99] 4 8 false, 1)
  /\ aread (MkArena 7 3 [11; 99] 4 8 false) (MkIdx 7 1) = Some 99.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* COUNTEREXAMPLE 3.  A mark accepted from a foreign arena corrupts the cursor:
   the real word refuses it, the mutation that skips the generation test
   truncates this arena to the other arena's cursor and loses two cells. *)
Definition arollback_no_owner_check (a : arena) (m : amark) : option arena :=
  if afrozen a then None
  else if Nat.ltb (acount a) (mk_cur m) then None
  else Some (MkArena (agen a) (aowner a) (firstn (mk_cur m) (cells a))
                     (acap a) (aceil a) (afrozen a)).

Example arena_foreign_mark_corrupts_cursor :
  arollback a3 (MkMark 9 1) = None
  /\ arollback_no_owner_check a3 (MkMark 9 1)
       = Some (MkArena 7 3 [11] 4 8 false)
  /\ acount a3 = 3.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* 5. FREEZE. *)
Example ex_freeze :
  afreeze a3 = Some (MkArena 7 3 [11; 22; 33] 4 8 true)
  /\ aat (MkArena 7 3 [11; 22; 33] 4 8 true) (MkIdx 7 2) = Some 33
  /\ apeek a3 (MkIdx 7 2) = Some 33
  /\ apeek (MkArena 7 3 [11; 22; 33] 4 8 true) (MkIdx 7 2) = None
  /\ apush 3 (MkArena 7 3 [11; 22; 33] 4 8 true) 44 = None
  /\ amark_of (MkArena 7 3 [11; 22; 33] 4 8 true) = None.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* 6. CROSS-OWNER. *)
Example ex_cross_owner :
  amint a3 1 = Some (MkIdx 7 1)
  /\ aread (MkArena 8 3 [90; 91; 92] 4 8 false) (MkIdx 7 1) = None
  /\ aread a3 (MkIdx 7 1) = Some 22.
Proof. repeat split; vm_compute; reflexivity. Qed.

Example ex_abort_kills_indices :
  aabort a3 = Some (MkArena 0 3 [11; 22; 33] 4 8 false)
  /\ aread (MkArena 0 3 [11; 22; 33] 4 8 false) (MkIdx 7 1) = None.
Proof. split; vm_compute; reflexivity. Qed.

(* 7-9. CONTEXT LIFETIME.  An outer context, then a child inside it. *)
Definition s0 : cstate := MkCState 0 [].

Example ex_ctx_nesting :
  center s0 100 = Some (MkCState 1 [MkCtx 1 100], MkCtx 1 100)
  /\ center (MkCState 1 [MkCtx 1 100]) 200
       = Some (MkCState 2 [MkCtx 1 100; MkCtx 2 200], MkCtx 2 200)
  /\ cserial_live (MkCState 2 [MkCtx 1 100; MkCtx 2 200]) 2 = true
  /\ cserial_live (cleave (MkCState 2 [MkCtx 1 100; MkCtx 2 200]) 1) 2 = false
  /\ cserial_live (cleave (MkCState 2 [MkCtx 1 100; MkCtx 2 200]) 1) 1 = true
  /\ cserial_live (cleave (MkCState 2 [MkCtx 1 100; MkCtx 2 200]) 0) 1 = false.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* Leaving the outer context retires the child and the grandchild in one step,
   which is what makes every arena registered inside them stale at once. *)
Example ex_ctx_leave_kills_children :
  cserial_live
    (cleave (MkCState 3 [MkCtx 1 100; MkCtx 2 200; MkCtx 3 300]) 1) 2 = false
  /\ cserial_live
       (cleave (MkCState 3 [MkCtx 1 100; MkCtx 2 200; MkCtx 3 300]) 1) 3
     = false.
Proof. split; vm_compute; reflexivity. Qed.

(* COUNTEREXAMPLE 4.  An exit that retires only its own slot and leaves the
   depth alone keeps the grandchild live after its owner is gone. *)
Example ctx_no_truncation_leaves_live_handle :
  cserial_live
    (cleave (MkCState 3 [MkCtx 1 100; MkCtx 2 200; MkCtx 3 300]) 1) 3 = false
  /\ cserial_live
       (cleave_no_truncate (MkCState 3 [MkCtx 1 100; MkCtx 2 200; MkCtx 3 300]) 1)
       3
     = true.
Proof. split; vm_compute; reflexivity. Qed.

(* COUNTEREXAMPLE 5 — and FINDING 1.  A context abandoned by a throw keeps its
   registry slot, so its serial still reports LIVE even though MEM:WITH-BYTES
   has already released its mapping.  Compare the normal exit directly beside
   it. *)
Example ctx_abandoned_context_still_reports_live :
  cserial_live (cleave (MkCState 2 [MkCtx 1 100; MkCtx 2 200]) 1) 2 = false
  /\ cserial_live (cabandon (MkCState 2 [MkCtx 1 100; MkCtx 2 200])) 2 = true.
Proof. split; vm_compute; reflexivity. Qed.

(* 10. SCRATCH. *)
Example ex_scratch_bumps :
  scratch_take 128 5 = Some (128, 136)
  /\ scratch_take 136 16 = Some (136, 152)
  /\ scratch_take 128 0 = None
  /\ scratch_take 128 262017 = None
  /\ scratch_take 262144 1 = None.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* FINDING 3, as arithmetic.  `growth_bytes cap` is what the doubling schedule
   costs to reach `cap`: the spans 8, 16, ... cap are all taken from the
   context and only the last one is used, so the total is 2 * cap - seed_cells
   cells.  A ceiling of 8192 cells fits the scratch region; the next capacity
   the doubling would reach does not, so 16384 is unreachable and any ceiling
   above 8192 cannot be enforced by IR-ARENA:GROW. *)
Definition growth_bytes (cap : nat) : nat := (2 * cap - seed_cells) * slot_bytes.

Example arena_ceiling_beyond_the_mapping :
  Nat.leb (growth_bytes 8192) scratch_cap = true
  /\ Nat.ltb scratch_cap (growth_bytes 16384) = true.
Proof. split; vm_compute; reflexivity. Qed.

Example ex_constants :
  hdr_bytes = 128 /\ scratch_cap = 262016 /\ seed_cells = 8
  /\ depth_max = 64 /\ slot_max = 64 /\ local_bits = 32.
Proof. repeat split; vm_compute; reflexivity. Qed.

Example ex_module_budget :
  mint_take 0 2 = Some 1 /\ mint_take 1 2 = Some 2 /\ mint_take 2 2 = None.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* ================================================================== *)
(* Assumption discipline.                                              *)
(*                                                                     *)
(* Every published result must report that it is closed under the      *)
(* global context.  This file needs no external assumption and has     *)
(* none; the two host assumptions in Habu.Common.IdAllocator belong to *)
(* the compare-and-swap boundary and are not used here.                *)
(* ================================================================== *)

Print Assumptions arena_push_answers_the_cursor.
Print Assumptions arena_push_preserves_reads.
Print Assumptions arena_push_index_stays_valid.
Print Assumptions arena_grow_preserves_reads.
Print Assumptions arena_grow_preserves_count.
Print Assumptions arena_reads_ignore_capacity.
Print Assumptions arena_grow_ceiling_fail_closed.
Print Assumptions arena_push_at_ceiling_fail_closed.
Print Assumptions arena_push_preserves_wf.
Print Assumptions arena_push_keeps_count_under_ceiling.
Print Assumptions arena_mark_is_own_cursor.
Print Assumptions arena_rollback_truncates.
Print Assumptions arena_rollback_keeps_below_mark.
Print Assumptions arena_rollback_kills_above_mark.
Print Assumptions arena_rollback_foreign_mark_rejected.
Print Assumptions arena_rollback_stale_mark_rejected.
Print Assumptions arena_freeze_preserves_reads.
Print Assumptions arena_freeze_preserves_count.
Print Assumptions arena_freeze_blocks_mutation.
Print Assumptions arena_freeze_retires_live_reader.
Print Assumptions arena_cross_owner_rejects.
Print Assumptions arena_abort_kills_every_index.
Print Assumptions ctx_gen_nonzero.
Print Assumptions ctx_enter_raises_counter.
Print Assumptions ctx_enter_preserves_wf.
Print Assumptions ctx_gen_never_reused.
Print Assumptions ctx_enter_registers_live.
Print Assumptions ctx_no_handle_survives_owner_exit.
Print Assumptions ctx_leave_kills_the_mapping_handle.
Print Assumptions ctx_depth_bounded.
Print Assumptions ctx_enter_keeps_depth_bounded.
Print Assumptions ctx_nesting_stops_at_depth_max.
Print Assumptions scratch_spans_disjoint.
Print Assumptions scratch_span_within_mapping.
Print Assumptions scratch_exhaustion_fail_closed.
Print Assumptions scratch_zero_size_rejected.
Print Assumptions mint_take_bounded.
Print Assumptions mint_take_at_ceiling_rejected.
