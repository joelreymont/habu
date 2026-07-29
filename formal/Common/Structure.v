(* Habu.Common.Structure — a model of the two newest stores in the shared
   compiler substrate: the operation and value store IR-OP
   (src/compiler/ir/op.f) and the function and block store IR-FUN
   (src/compiler/ir/fun.f).

   Both files make two central claims in their headers, and both claims are
   the kind that is easy to believe and hard to check by reading:

     A. SSA DOMINANCE BY CONSTRUCTION.  op.f:77-88 says an operand must name
        a value whose ordinal is strictly below the live value count at the
        moment the operation is appended (IR-OP:OPERANDS-CK, op.f:580-585,
        called from IR-OP:END-OP, op.f:806 with `v VCNT` — the count BEFORE
        the operation's own results are minted, op.f:820).  The claim is
        that use-before-definition and definitional cycles are therefore
        IMPOSSIBLE rather than merely detected.

     B. WINDOW TILING.  op.f:102-111 says all four of an operation's
        reference windows live in one pool written in a fixed order
        (IR-OP:WIN-STARTS, op.f:671-676; IR-OP:ROW-ADD, op.f:678-688), so
        each row's window starts exactly where the previous row's ended, and
        IR-OP:TILE-CK (op.f:455-465) revalidates that with a constant-cost
        check against ONE neighbouring row instead of a search.  fun.f:66-80
        makes the same claim for a block's operation window over IR-OP's
        operation table (IR-FUN:OTILE-CK, fun.f:575-578) and for a
        function's block window over the block table (IR-FUN:BTILE-CK,
        fun.f:555-558).

   Everything below is proved from the global context alone.  Every
   `Print Assumptions` at the bottom reports "Closed under the global
   context".  There is no `Axiom`, no `admit` and no `Admitted` here.

   ------------------------------------------------------------------------
   MODEL GAPS

   Places where this model and the Habu code deliberately differ.  Each is
   named, so a reader knows exactly what the theorems do and do not cover.

   1. Storage layout is abstracted away.  The real stores are IR-ARENA
      arenas with a three-cell header each (format tag, owning module
      serial, committed capacity), and every read revalidates shape, magic,
      owner serial and window (IR-OP:PHDR-CK/VHDR-CK/RHDR-CK, op.f:247-260;
      IR-FUN:PHDR-CK/FNHDR-CK/BHDR-CK, fun.f:330-343).  This model has no
      cells, so it says nothing about a bypass-written header.  What it does
      model is the arithmetic those headers protect: window starts, window
      lengths, row counts and pool cell counts as plain numbers.

   2. Ownership is not modelled.  IR-OP:TRIO-CK (op.f:302-308),
      IR-OP:RKEY-CK (op.f:318-321) and the IR-FUN twins (fun.f:385-438)
      reject a foreign module key or a cross-module store trio before any
      row is read.  Cross-module separation is Habu.Common.IdLaws.  This
      model is one module's stores.

   3. Ordinals are not packed.  Every identity here is a plain `nat`; the
      Habu words answer `key l IR-ID:PACK-OP` and friends.  Packing and its
      round trips are Habu.Common.IdLaws.

   4. The dialect schema is abstracted to one predicate.  IR-OP:SCHEMA-CK
      (op.f:627-631) asks IR-SCHEMA for the opcode's operand, result and
      successor arity, and IR-FUN:TERM-CK (fun.f:789-794) asks it whether an
      opcode is a terminator.  This model takes "is this operation a
      terminator" as an arbitrary boolean function of the operation ordinal
      and does not model arity at all: arity is a property of the schema
      table, which Habu.Common.Interning already covers as an interning
      table, and it is independent of both central claims.

   5. Successors are not modelled as references.  op.f:84-88 says a
      successor legitimately points forward — a branch to a block still
      being built — so IR-OP:SUCCS-CK (op.f:606-611) checks only ownership
      and non-negativity.  A successor therefore contributes nothing to the
      use relation, and the model's use relation is operands only.  The
      existence of a successor block belongs to the section 6.5 freeze
      verifier and is out of scope here.

   6. Machine integers are `nat`.  The real cells are 64-bit and the
      capacities are bounded by IR-OP:ROW-CAP-MAX / VAL-CAP-MAX /
      POOL-CAP-MAX (op.f:197-199) and the IR-FUN equivalents
      (fun.f:244-246).  Ordinal overflow is out of scope.  `nat` subtraction
      truncates, and the one place that matters — the stored terminator
      ordinal `wend - 1` — is only ever formed when the window is non-empty.

   7. Arena growth, freezing and the frozen readers are not modelled.  The
      `F*` readers in both files (IR-OP:FTILE-CK, op.f:467-477;
      IR-FUN:FOTILE-CK, fun.f:580-583) run the identical arithmetic against
      an IR-ARENA:view instead of an IR-ARENA:arena, so every theorem about
      a live check is a theorem about the frozen one.  That the two really
      are the same arithmetic is checked structurally on the Habu side by
      test/compiler/ir-structure-cases.f, not proved here.

   8. Attributes are counted, not compared.  IR-OP:ATTRS-CK (op.f:596-601)
      and IR-FUN:ATTRS-CK (fun.f:721-726) confirm each staged attribute is a
      real row of this module's attribute table; that is an interning fact
      and belongs to Habu.Common.Interning.  Here an attribute window is a
      length.

   9. Single task, no interleaving.  Both stores are built by one
      compilation task holding the context.  The one staged operation and
      the one staged function and block are package-owned globals
      (op.f:514-525, fun.f:611-638) precisely because of that discipline.

  10. Abandonment is modelled only through its effect on the tables.
      IR-OP:ABANDON (op.f:752-753), IR-FUN:ABANDON-FUN (fun.f:908-909) and
      IR-FUN:ABANDON-BLOCK (fun.f:1000-1001) drop a stage without appending
      a row.  This model has no stage; it models the resulting table states
      directly, which is where findings F1 and F3 below live.

  11. The block-argument window is modelled as a run of value ordinals with
      the value rows beside it.  fun.f:91-104 is explicit that this window
      does NOT tile the value table, because operation results are
      interleaved between one block's arguments and the next block's.  The
      model reflects that: `args_ck` is a per-element check against the
      value row, never a tiling check.

  12. Linkage, visibility, calling convention and the duplicate-symbol scan
      (IR-FUN:BODY-CK fun.f:744-753, IR-FUN:TARGET-CK fun.f:760-764,
      IR-FUN:DUP-CK fun.f:706-710) are not modelled.  They are local
      decisions on one staged row and neither touches a window, a value
      ordinal or a parent.
   ------------------------------------------------------------------------
   BINDING GAPS

   Measured by mutating src/compiler/ir/{op,fun}.f and rerunning
   test/compiler/ir-structure-proof.f.  The operand bound, the row ceiling and
   the block's STEP-CK are all caught by a vector row.  Three are caught only
   by the frozen guard bodies, which notice any edit to those words but no
   behaviour: giving the result window the operand window's start
   (IR-OP:WIN-STARTS), dropping the operand-window arm of IR-OP:TILE-CK, and
   dropping the "this block" arm of IR-FUN:ARGS-CK.  So the tiling results and
   `args_ck_sound` rest on pinned text rather than on a driven row.

   ------------------------------------------------------------------------
   FINDINGS

   Three facts fall out of the proofs that the Habu headers do not state.
   Each is a theorem below with an executable counterexample.

   F1. AN OPERATION APPENDED AFTER THE LAST BLOCK BELONGS TO NO BLOCK, AND
       NOTHING IN IR-FUN CATCHES IT.  fun.f:78-80 says "an operation
       appended outside a block leaves a gap the tiling check rejects".
       That holds only for an operation appended BETWEEN two blocks: the
       next IR-FUN:BEGIN-BLOCK captures a start that no longer equals the
       previous block's end, and IR-FUN:END-BLOCK's STEP-CK (fun.f:1039)
       rejects it.  An operation appended after the LAST block is past the
       end of every window, so no STEP-CK ever looks at it, and an operation
       row carries no parent field by design (op.f:42-43), so there is no
       second record to disagree with either.
       `orphan_op_outside_every_block` proves the gap exists and
       `coverage_is_necessary` proves exactly which check closes it: the
       last block's operation-window end must equal IR-OP:OPS.  The same
       argument applies one level up, to blocks left behind by an abandoned
       function and the block table's count.

   F2. THE STORED TERMINATOR ORDINAL CARRIES NO INFORMATION.
       IR-FUN:BROW-ADD writes `opst opn + 1-` (fun.f:847) and
       IR-FUN:TERMINATOR@ recomputes `b l BOP-END 1-` and rejects any other
       stored value (fun.f:1193-1194).  The stored field is a function of
       the operation window, so on any row this package built the comparison
       can never fail.  It is a second authority that is checked to agree —
       the very shape fun.f:73-76 rejects for the parent field.
       `stored_terminator_is_derived` proves it.

   F3. A (BLOCK, ARGUMENT POSITION) PAIR DOES NOT IDENTIFY ONE VALUE.
       fun.f:98-101 claims "Two blocks therefore cannot share an argument
       value at all, which is a stronger fact than non-overlap".  That is
       true and is proved below as `arg_values_are_not_shared`.  The dual is
       false: two DIFFERENT values can both claim to be block l's argument
       at position p.  IR-FUN:ADD-BLOCK-ARG names the block being built as
       `b BCNT` (fun.f:1010), so a block that is opened, given arguments and
       then dropped by IR-FUN:ABANDON-BLOCK leaves value rows naming the
       ordinal the NEXT block will receive, at the same positions.
       IR-FUN:ARGS-CK (fun.f:800-808) checks the window's elements and never
       asks whether some other value also claims the pair, so the next block
       closes cleanly.  fun.f:130-138 acknowledges the stranded rows for a
       REJECTED END-BLOCK and points at the builder ABORT; ABANDON-BLOCK is
       a normal, non-error path with the same effect and no such note.
       `arg_identity_is_not_injective` exhibits it and
       `arg_ownership_forces_injectivity` proves which check closes it.
   ------------------------------------------------------------------------ *)

From Stdlib Require Import Bool List Lia Arith Wf_nat.
Import ListNotations.

(* ================================================================== *)
(* Part 1.  Windows and tiling.                                        *)
(*                                                                     *)
(* A window is a start and a length into some table.  That is the shape *)
(* of all seven windows in the two files: the operand, result,          *)
(* successor and attribute windows of an operation row (op.f:178-185),  *)
(* the block and attribute windows of a function row (fun.f:221-224),   *)
(* and the argument and operation windows of a block row               *)
(* (fun.f:234-237).                                                     *)
(* ================================================================== *)

Record win : Type := MkWin { wst : nat; wlen : nat }.

Definition wend (w : win) : nat := wst w + wlen w.

Definition w0 : win := MkWin 0 0.

Definition nthw (ws : list win) (k : nat) : win := nth k ws w0.

(* Cell i belongs to window w. *)
Definition covers (w : win) (i : nat) : Prop := wst w <= i /\ i < wend w.

(* Contiguity: the first window starts at the base and each following one
   starts exactly where the previous one ended.  This is what the append
   side establishes — IR-OP:WIN-STARTS (op.f:671-676) computes exactly
   these starts and IR-OP:ROW-ADD (op.f:678-688) writes them. *)
Fixpoint contig (base : nat) (ws : list win) : Prop :=
  match ws with
  | [] => True
  | w :: rest => wst w = base /\ contig (wend w) rest
  end.

(* Total length.  Under contiguity this is where the last window ends, and
   that is the O(1) coverage check named in F1. *)
Fixpoint sumlen (ws : list win) : nat :=
  match ws with
  | [] => 0
  | w :: rest => wlen w + sumlen rest
  end.

Definition span (base : nat) (ws : list win) : nat := base + sumlen ws.

(* The neighbour check the readers actually run, written as a condition on
   ONE row: IR-OP:TILE-CK (op.f:455-465) reads row l-1's attribute-window
   end and requires row l's operand window to start there; IR-FUN:OTILE-CK
   (fun.f:575-578), IR-FUN:BTILE-CK (fun.f:555-558) and IR-FUN:ATILE-CK
   (fun.f:532-536) do the same for one block row and one function row. *)
Definition step_ok (base : nat) (ws : list win) (k : nat) : Prop :=
  wst (nthw ws k) = match k with
                    | O => base
                    | S j => wend (nthw ws j)
                    end.

Definition steps_ok (base : nat) (ws : list win) : Prop :=
  forall k, k < length ws -> step_ok base ws k.

(* Global non-overlap, no gap, and staying inside the table: the properties
   a search over every other row would establish. *)
Definition no_overlap (ws : list win) : Prop :=
  forall j k i,
    j < length ws -> k < length ws ->
    covers (nthw ws j) i -> covers (nthw ws k) i -> j = k.

Definition no_gap (base top : nat) (ws : list win) : Prop :=
  forall i, base <= i -> i < top ->
    exists k, k < length ws /\ covers (nthw ws k) i.

Definition in_range (base top : nat) (ws : list win) : Prop :=
  forall k i, k < length ws -> covers (nthw ws k) i -> base <= i /\ i < top.

Definition owner (ws : list win) (k i : nat) : Prop :=
  k < length ws /\ covers (nthw ws k) i.

(* ---- basic consequences of contiguity ---------------------------- *)

Lemma contig_base_le :
  forall ws base k,
    contig base ws -> k < length ws -> base <= wst (nthw ws k).
Proof.
  induction ws as [|w rest IH]; simpl; intros base k Hcontig Hk.
  - lia.
  - destruct Hcontig as [Hst Hrest].
    destruct k as [|k'].
    + unfold nthw. simpl. lia.
    + unfold nthw. simpl.
      assert (Hle : wend w <= wst (nth k' rest w0)).
      { apply (IH (wend w) k' Hrest). lia. }
      unfold wend in Hle. lia.
Qed.

Lemma contig_end_le_span :
  forall ws base k,
    contig base ws -> k < length ws -> wend (nthw ws k) <= span base ws.
Proof.
  induction ws as [|w rest IH]; simpl; intros base k Hcontig Hk.
  - lia.
  - destruct Hcontig as [Hst Hrest].
    unfold span in *. simpl.
    destruct k as [|k'].
    + unfold nthw, wend. simpl. lia.
    + unfold nthw. simpl.
      assert (Hle : wend (nth k' rest w0) <= wend w + sumlen rest).
      { apply (IH (wend w) k' Hrest). lia. }
      unfold wend in *. lia.
Qed.

Lemma contig_mono :
  forall ws base j k,
    contig base ws -> j < k -> k < length ws ->
    wend (nthw ws j) <= wst (nthw ws k).
Proof.
  induction ws as [|w rest IH]; simpl; intros base j k Hcontig Hjk Hk.
  - lia.
  - destruct Hcontig as [Hst Hrest].
    destruct k as [|k']; [lia |].
    destruct j as [|j'].
    + unfold nthw. simpl.
      apply (contig_base_le rest (wend w) k' Hrest). lia.
    + unfold nthw. simpl.
      apply (IH (wend w) j' k' Hrest); lia.
Qed.

(* ---- contiguity implies a partition ------------------------------ *)

(* Non-overlap.  This is the theorem behind op.f:108-111 ("Non-overlap
   becomes a constant-cost check on the row itself instead of a search over
   every other row") and fun.f:73-76.  Models IR-OP:TILE-CK,
   IR-FUN:OTILE-CK, IR-FUN:BTILE-CK and IR-FUN:ATILE-CK. *)
Theorem tiling_no_overlap :
  forall base ws, contig base ws -> no_overlap ws.
Proof.
  intros base ws Hcontig j k i Hj Hk [Hjlo Hjhi] [Hklo Hkhi].
  destruct (lt_eq_lt_dec j k) as [[Hlt | Heq] | Hgt].
  - pose proof (contig_mono ws base j k Hcontig Hlt Hk) as Hle. lia.
  - exact Heq.
  - pose proof (contig_mono ws base k j Hcontig Hgt Hj) as Hle. lia.
Qed.

(* No gap: every cell from the base up to the total span has an owner. *)
Lemma tiling_no_gap :
  forall base ws, contig base ws -> no_gap base (span base ws) ws.
Proof.
  intros base ws. revert base.
  induction ws as [|w rest IH]; intros base Hcontig i Hlo Hhi.
  - unfold span in Hhi. simpl in Hhi. lia.
  - destruct Hcontig as [Hst Hrest].
    unfold span in Hhi. simpl in Hhi.
    destruct (lt_dec i (wend w)) as [Hin | Hout].
    + exists 0. split; [simpl; lia |].
      unfold nthw, covers. simpl. lia.
    + assert (Hlo' : wend w <= i) by lia.
      assert (Hhi' : i < span (wend w) rest).
      { unfold span, wend. lia. }
      destruct (IH (wend w) Hrest i Hlo' Hhi') as [k [Hk Hcov]].
      exists (S k). split; [simpl; lia |].
      unfold nthw in *. simpl. exact Hcov.
Qed.

(* Nothing outside the base-to-span range is owned. *)
Lemma tiling_in_range :
  forall base ws, contig base ws -> in_range base (span base ws) ws.
Proof.
  intros base ws Hcontig k i Hk Hcov.
  pose proof (contig_base_le ws base k Hcontig Hk) as Hbase.
  pose proof (contig_end_le_span ws base k Hcontig Hk) as Hend.
  destruct Hcov as [Hlo Hhi]. lia.
Qed.

(* The partition in one piece: every cell in range belongs to exactly one
   window, and nothing else is owned. *)
Theorem tiling_partition :
  forall base ws,
    contig base ws ->
    (forall i, base <= i -> i < span base ws -> exists ! k, owner ws k i)
    /\ in_range base (span base ws) ws.
Proof.
  intros base ws Hcontig.
  split.
  - intros i Hlo Hhi.
    destruct (tiling_no_gap base ws Hcontig i Hlo Hhi) as [k [Hk Hcov]].
    exists k. split.
    + unfold owner. split; assumption.
    + intros k' [Hk' Hcov'].
      apply (tiling_no_overlap base ws Hcontig k k' i Hk Hk' Hcov Hcov').
  - apply tiling_in_range. exact Hcontig.
Qed.

(* ---- the O(1) neighbour check ------------------------------------ *)

(* The check each reader performs — one row against the row before it — is
   exactly contiguity.  Models IR-OP:TILE-CK's STEP-CK chain
   (op.f:458-464). *)
Theorem neighbour_check_is_contiguity :
  forall base ws, contig base ws <-> steps_ok base ws.
Proof.
  intros base ws. split.
  - generalize dependent base.
    induction ws as [|w rest IH]; intros base Hcontig k Hk; simpl in Hk.
    + lia.
    + destruct Hcontig as [Hst Hrest].
      destruct k as [|k'].
      * unfold step_ok, nthw. simpl. exact Hst.
      * unfold step_ok, nthw. simpl.
        assert (Hk' : k' < length rest) by lia.
        pose proof (IH (wend w) Hrest k' Hk') as Hstep.
        unfold step_ok, nthw in Hstep. simpl in Hstep.
        destruct k' as [|k'']; simpl in *; exact Hstep.
  - generalize dependent base.
    induction ws as [|w rest IH]; intros base Hsteps.
    + exact I.
    + split.
      * assert (H0 : 0 < length (w :: rest)) by (simpl; lia).
        pose proof (Hsteps 0 H0) as Hz.
        unfold step_ok, nthw in Hz. simpl in Hz. exact Hz.
      * apply IH. intros k Hk.
        assert (HSk : S k < length (w :: rest)) by (simpl; lia).
        pose proof (Hsteps (S k) HSk) as HS.
        unfold step_ok, nthw in *. simpl in HS.
        destruct k as [|k']; simpl in *; exact HS.
Qed.

(* Hence checking every row against its one neighbour is enough for global
   non-overlap: the search over every other row is never needed.  This is
   the equivalence claimed by op.f:108-111, in the direction that matters. *)
Corollary neighbour_check_gives_non_overlap :
  forall base ws, steps_ok base ws -> no_overlap ws.
Proof.
  intros base ws Hsteps.
  apply (tiling_no_overlap base).
  apply neighbour_check_is_contiguity. exact Hsteps.
Qed.

(* The converse fails, and that is worth writing down: the neighbour check
   is STRICTLY STRONGER than non-overlap, because it also forces ascending
   order.  Two windows in descending order partition the same cells without
   passing the check.  So the check is sound for the property the headers
   claim, and is not a characterisation of it. *)
Definition descending : list win := [MkWin 1 1; MkWin 0 1].

Theorem non_overlap_does_not_imply_neighbour_check :
  no_overlap descending
  /\ no_gap 0 (span 0 descending) descending
  /\ in_range 0 (span 0 descending) descending
  /\ ~ contig 0 descending.
Proof.
  unfold descending, span, no_overlap, no_gap, in_range, nthw, covers, wend.
  split; [| split; [| split]].
  - intros j k i Hj Hk Hcj Hck. simpl in *.
    destruct j as [|[|j]]; destruct k as [|[|k]]; simpl in *; lia.
  - intros i Hlo Hhi. simpl in *.
    destruct i as [|[|i]].
    + exists 1. simpl. lia.
    + exists 0. simpl. lia.
    + lia.
  - intros k i Hk Hcov. simpl in *.
    destruct k as [|[|k]]; simpl in *; lia.
  - intros [Hst _]. simpl in Hst. lia.
Qed.

(* And checking ONE row against its neighbour is not enough: a row can tile
   against the row before it while two earlier rows overlap.  The check has
   to run for every row, which is what makes it an invariant and not a spot
   check.  IR-OP:WIN@ (op.f:480-485) runs TILE-CK on the row it is about to
   read, so this is the shape of what a reader alone establishes. *)
Definition one_late_check : list win := [MkWin 0 2; MkWin 1 2; MkWin 3 1].

Theorem single_row_check_is_not_enough :
  step_ok 0 one_late_check 2
  /\ covers (nthw one_late_check 0) 1
  /\ covers (nthw one_late_check 1) 1
  /\ ~ no_overlap one_late_check.
Proof.
  unfold one_late_check, step_ok, nthw, covers, wend.
  split; [| split; [| split]].
  - simpl. lia.
  - simpl. lia.
  - simpl. lia.
  - intros Hno.
    assert (H01 : 0 = 1).
    { apply (Hno 0 1 1); simpl; unfold nthw, covers, wend; simpl; lia. }
    discriminate.
Qed.

(* ---- what contiguity does NOT give you --------------------------- *)

(* Contiguity is silent about everything past the total span.  That is the
   whole of finding F1 in one line. *)
Theorem tiling_says_nothing_past_the_span :
  forall base ws i,
    contig base ws -> span base ws <= i -> ~ (exists k, owner ws k i).
Proof.
  intros base ws i Hcontig Hpast [k [Hk Hcov]].
  pose proof (tiling_in_range base ws Hcontig k i Hk Hcov) as [_ Hlt].
  lia.
Qed.

(* The coverage comparison that closes the gap, in its O(1) form: under
   contiguity the total span IS the last row's end, so comparing that one
   number against the owned table's count is the whole check. *)
Theorem coverage_check_is_last_row_end :
  forall base ws,
    contig base ws -> ws <> [] -> span base ws = wend (last ws w0).
Proof.
  intros base ws. revert base.
  induction ws as [|w rest IH]; intros base Hcontig Hne.
  - contradiction.
  - destruct Hcontig as [Hst Hrest].
    destruct rest as [|w' rest'].
    + unfold span, wend. simpl. lia.
    + assert (Hne' : w' :: rest' <> []) by discriminate.
      pose proof (IH (wend w) Hrest Hne') as Heq.
      unfold span, wend in *. simpl in *. lia.
Qed.

(* With the coverage comparison in hand the partition is total over the
   whole table, not merely over the part the windows happen to reach. *)
Theorem coverage_closes_the_gap :
  forall base ws top,
    contig base ws -> span base ws = top ->
    (forall i, base <= i -> i < top -> exists ! k, owner ws k i)
    /\ (forall i, top <= i -> ~ (exists k, owner ws k i)).
Proof.
  intros base ws top Hcontig Htop.
  split.
  - intros i Hlo Hhi.
    apply (proj1 (tiling_partition base ws Hcontig)); lia.
  - intros i Hhi.
    apply (tiling_says_nothing_past_the_span base ws i Hcontig). lia.
Qed.

(* Coverage is also necessary: if every cell of the table is owned and
   nothing outside it is, the span has to be the table's size.  So the
   comparison is not one sufficient check among many — it is the check. *)
Theorem coverage_is_necessary :
  forall base ws top,
    contig base ws ->
    base <= top ->
    (forall i, base <= i -> i < top -> exists k, owner ws k i) ->
    (forall k i, owner ws k i -> i < top) ->
    span base ws = top.
Proof.
  intros base ws top Hcontig Hbt Hall Hbound.
  assert (Hle : span base ws <= top).
  { destruct (le_lt_dec (span base ws) top) as [Hle | Hgt]; [exact Hle |].
    exfalso.
    assert (Hlo : base <= span base ws - 1) by (unfold span in *; lia).
    assert (Hhi : span base ws - 1 < span base ws) by (unfold span in *; lia).
    destruct (tiling_no_gap base ws Hcontig (span base ws - 1) Hlo Hhi)
      as [k [Hk Hcov]].
    pose proof (Hbound k (span base ws - 1) (conj Hk Hcov)) as Hlt.
    lia. }
  destruct (le_lt_eq_dec (span base ws) top Hle) as [Hlt | Heq]; [| exact Heq].
  exfalso.
  assert (Hlo : base <= span base ws) by (unfold span; lia).
  destruct (Hall (span base ws) Hlo Hlt) as [k Hown].
  apply (tiling_says_nothing_past_the_span base ws (span base ws) Hcontig).
  - lia.
  - exists k. exact Hown.
Qed.

(* ---- appending windows preserves the tiling ---------------------- *)

Lemma contig_app :
  forall xs base ys,
    contig base (xs ++ ys) <-> contig base xs /\ contig (span base xs) ys.
Proof.
  induction xs as [|x xs IH]; simpl; intros base ys.
  - unfold span. simpl.
    rewrite Nat.add_0_r.
    split; [intros H; split; [exact I | exact H] | intros [_ H]; exact H].
  - split.
    + intros [Hst Hrest].
      apply IH in Hrest as [Hxs Hys].
      split; [split; assumption |].
      assert (Heq : span (wend x) xs = span base (x :: xs)).
      { unfold span, wend. simpl. lia. }
      rewrite Heq in Hys. exact Hys.
    + intros [[Hst Hxs] Hys].
      split; [exact Hst |].
      apply IH. split; [exact Hxs |].
      assert (Heq : span base (x :: xs) = span (wend x) xs).
      { unfold span, wend. simpl. lia. }
      rewrite Heq in Hys. exact Hys.
Qed.

Lemma contig_snoc :
  forall base ws w,
    contig base ws -> wst w = span base ws -> contig base (ws ++ [w]).
Proof.
  intros base ws w Hcontig Hst.
  apply contig_app. split; [exact Hcontig |].
  simpl. split; [exact Hst | exact I].
Qed.

(* ================================================================== *)
(* Part 2.  IR-OP: the four windows of one operation row.              *)
(*                                                                     *)
(* op.f:102-111.  All four windows live in one pool and an append       *)
(* writes them in a fixed order: operands, results, successors, then    *)
(* attributes (IR-OP:WIN-STARTS op.f:671-676, IR-OP:ROW-ADD             *)
(* op.f:678-688, and the four IR-OP:LIST-ADD / RESULTS-ADD calls        *)
(* op.f:815-818).                                                       *)
(* ================================================================== *)

Record oprow : Type := MkOpRow {
  r_operands : nat;
  r_results  : nat;
  r_succs    : nat;
  r_attrs    : nat
}.

Definition row_cells (r : oprow) : nat :=
  r_operands r + r_results r + r_succs r + r_attrs r.

(* Exactly IR-OP:WIN-STARTS: st, st+operands, st+operands+results,
   st+operands+results+successors. *)
Definition row_wins (base : nat) (r : oprow) : list win :=
  [ MkWin base (r_operands r)
  ; MkWin (base + r_operands r) (r_results r)
  ; MkWin (base + r_operands r + r_results r) (r_succs r)
  ; MkWin (base + r_operands r + r_results r + r_succs r) (r_attrs r) ].

Fixpoint pool_wins (base : nat) (rs : list oprow) : list win :=
  match rs with
  | [] => []
  | r :: rest => row_wins base r ++ pool_wins (base + row_cells r) rest
  end.

Lemma row_wins_sumlen : forall base r, sumlen (row_wins base r) = row_cells r.
Proof.
  intros base r. unfold row_wins, row_cells. simpl. lia.
Qed.

Lemma row_wins_span : forall base r, span base (row_wins base r) = base + row_cells r.
Proof.
  intros base r. unfold span. rewrite row_wins_sumlen. reflexivity.
Qed.

(* One row's four windows tile the pool from its start. *)
Theorem op_row_windows_tile :
  forall base r, contig base (row_wins base r).
Proof.
  intros base r. unfold row_wins. simpl. unfold wend. simpl.
  repeat split; lia.
Qed.

(* And every row's four windows together tile the whole pool.  This is the
   fact IR-OP:TILE-CK revalidates one row at a time. *)
Theorem op_pool_windows_tile :
  forall rs base, contig base (pool_wins base rs).
Proof.
  induction rs as [|r rest IH]; intros base.
  - exact I.
  - replace (pool_wins base (r :: rest))
      with (row_wins base r ++ pool_wins (base + row_cells r) rest)
      by reflexivity.
    apply contig_app. split.
    + apply op_row_windows_tile.
    + rewrite row_wins_span. apply IH.
Qed.

(* Hence every pool cell below the total belongs to exactly one window of
   exactly one row: "belongs to exactly one owner" by construction. *)
Corollary op_pool_cell_has_one_owner :
  forall rs base i,
    base <= i -> i < span base (pool_wins base rs) ->
    exists ! k, owner (pool_wins base rs) k i.
Proof.
  intros rs base i Hlo Hhi.
  apply (proj1 (tiling_partition base (pool_wins base rs)
                  (op_pool_windows_tile rs base))); assumption.
Qed.

(* A row whose windows do not continue where the previous row ended lets one
   pool cell be claimed twice.  This is the mutation IR-OP:TILE-CK's
   STEP-CK (op.f:458) refuses. *)
Definition overlapping_pool : list win := [MkWin 0 2; MkWin 1 2].

Theorem non_tiling_admits_double_ownership :
  covers (nthw overlapping_pool 0) 1
  /\ covers (nthw overlapping_pool 1) 1
  /\ ~ contig 0 overlapping_pool
  /\ ~ no_overlap overlapping_pool.
Proof.
  unfold overlapping_pool, nthw, covers, wend.
  split; [| split; [| split]].
  - simpl. lia.
  - simpl. lia.
  - intros [_ [Hst _]]. unfold wend in Hst. simpl in Hst. lia.
  - intros Hno.
    assert (H01 : 0 = 1).
    { apply (Hno 0 1 1); simpl; unfold nthw, covers, wend; simpl; lia. }
    discriminate.
Qed.

Example op_row_windows_example :
  row_wins 5 (MkOpRow 2 1 0 3)
    = [MkWin 5 2; MkWin 7 1; MkWin 8 0; MkWin 8 3].
Proof. vm_compute. reflexivity. Qed.

Example op_pool_windows_example :
  pool_wins 0 [MkOpRow 0 1 0 0; MkOpRow 1 1 0 2]
    = [MkWin 0 0; MkWin 0 1; MkWin 1 0; MkWin 1 0;
       MkWin 1 1; MkWin 2 1; MkWin 3 0; MkWin 3 2].
Proof. vm_compute. reflexivity. Qed.

(* ================================================================== *)
(* Part 3.  IR-OP: SSA dominance by construction.                      *)
(* ================================================================== *)

Record op : Type := MkOp {
  uses  : list nat;   (* operand value ordinals, IR-OP:ADD-OPERAND *)
  nres  : nat;        (* result count, IR-OP:ADD-RESULT *)
  nsucc : nat;        (* successor count, IR-OP:ADD-SUCCESSOR *)
  nattr : nat         (* attribute count, IR-OP:ADD-ATTR *)
}.

Definition opnil : op := MkOp [] 0 0 0.

Definition op_row (o : op) : oprow :=
  MkOpRow (length (uses o)) (nres o) (nsucc o) (nattr o).

Definition op_cells (o : op) : nat := row_cells (op_row o).

(* The three live counts IR-OP:END-OP reads: the operation table's row
   count, the value table's row count, and the pool's live cells. *)
Record ostate : Type := MkOState {
  ops    : list op;
  nvals  : nat;
  pcells : nat
}.

(* The three committed ceilings IR-OP:NEW writes into the headers
   (op.f:720-737) and IR-OP:ROOM-CK reads back (op.f:646-650). *)
Record ocaps : Type := MkOCaps {
  cap_ops  : nat;
  cap_vals : nat;
  cap_pool : nat
}.

(* IR-OP:OPERANDS-CK (op.f:580-585).  `vcnt` is the value table's live count
   at the moment IR-OP:END-OP runs it (op.f:806), which is before the
   operation's own results are minted (op.f:820).  Strictly below. *)
Definition operands_ok (st : ostate) (o : op) : bool :=
  forallb (fun r => Nat.ltb r (nvals st)) (uses o).

(* IR-OP:ROOM-CK (op.f:646-650), all three ceilings, before any push. *)
Definition room_ok (c : ocaps) (st : ostate) (o : op) : bool :=
  Nat.ltb (length (ops st)) (cap_ops c)
  && Nat.leb (nvals st + nres o) (cap_vals c)
  && Nat.leb (pcells st + op_cells o) (cap_pool c).

(* IR-OP:END-OP (op.f:798-821): every check first, then the pool cells, the
   operation row, and one value row per result. *)
Definition end_op (c : ocaps) (st : ostate) (o : op) : option ostate :=
  if operands_ok st o && room_ok c st o
  then Some (MkOState (ops st ++ [o]) (nvals st + nres o) (pcells st + op_cells o))
  else None.

(* The state-passing form, so "the stores are unchanged on failure" is a
   statement about a state and not about the absence of one. *)
Definition end_op_step (c : ocaps) (st : ostate) (o : op) : ostate * bool :=
  match end_op c st o with
  | Some st' => (st', true)
  | None => (st, false)
  end.

Fixpoint build (c : ocaps) (st : ostate) (os : list op) : option ostate :=
  match os with
  | [] => Some st
  | o :: rest =>
      match end_op c st o with
      | Some st' => build c st' rest
      | None => None
      end
  end.

(* ---- atomicity and fail-closed ----------------------------------- *)

Lemma end_op_all_or_nothing :
  forall c st o st',
    end_op c st o = Some st' ->
    ops st' = ops st ++ [o]
    /\ nvals st' = nvals st + nres o
    /\ pcells st' = pcells st + op_cells o.
Proof.
  intros c st o st' Hend. unfold end_op in Hend.
  destruct (operands_ok st o && room_ok c st o) eqn:Hck; [| discriminate].
  inversion Hend. simpl. repeat split.
Qed.

Lemma end_op_failure_atomic :
  forall c st o, end_op c st o = None -> end_op_step c st o = (st, false).
Proof.
  intros c st o Hnone. unfold end_op_step. rewrite Hnone. reflexivity.
Qed.

Theorem ceiling_fail_closed :
  forall c st o, room_ok c st o = false -> end_op_step c st o = (st, false).
Proof.
  intros c st o Hroom.
  apply end_op_failure_atomic.
  unfold end_op. rewrite Hroom, andb_false_r. reflexivity.
Qed.

Theorem ssa_violation_fail_closed :
  forall c st o, operands_ok st o = false -> end_op_step c st o = (st, false).
Proof.
  intros c st o Hop.
  apply end_op_failure_atomic.
  unfold end_op. rewrite Hop. reflexivity.
Qed.

(* The write-first mutation: append the row and then check.  It leaves a row
   behind on a refused append, which is exactly what op.f:70-75 promises
   cannot happen. *)
Definition end_op_write_first (c : ocaps) (st : ostate) (o : op) : ostate * bool :=
  let st' := MkOState (ops st ++ [o]) (nvals st + nres o)
                      (pcells st + op_cells o) in
  if operands_ok st o && room_ok c st o then (st', true) else (st', false).

Definition one_result : op := MkOp [] 1 0 0.
Definition tiny : ocaps := MkOCaps 1 8 64.
Definition full_store : ostate := MkOState [one_result] 1 1.

Theorem write_order_is_load_bearing :
  end_op_step tiny full_store one_result = (full_store, false)
  /\ fst (end_op_write_first tiny full_store one_result) <> full_store.
Proof.
  split.
  - vm_compute. reflexivity.
  - vm_compute. discriminate.
Qed.

(* ---- what a completed build guarantees --------------------------- *)

Fixpoint sumres (os : list op) : nat :=
  match os with
  | [] => 0
  | o :: rest => nres o + sumres rest
  end.

(* The value ordinal the operation at index k starts minting at: the
   initial value count (block arguments, IR-OP:MINT-ARG) plus every result
   minted before it. *)
Definition vbase (v0 : nat) (os : list op) (k : nat) : nat :=
  v0 + sumres (firstn k os).

(* Value v is one of the results the operation at index k defines. *)
Definition defines (v0 : nat) (os : list op) (k v : nat) : Prop :=
  vbase v0 os k <= v /\ v < vbase v0 os k + nres (nth k os opnil).

Lemma build_shape :
  forall os c st st',
    build c st os = Some st' ->
    ops st' = ops st ++ os /\ nvals st' = nvals st + sumres os.
Proof.
  induction os as [|o rest IH]; simpl; intros c st st' Hbuild.
  - inversion Hbuild. rewrite app_nil_r. split; [reflexivity | lia].
  - destruct (end_op c st o) as [st1 |] eqn:Hend; [| discriminate].
    pose proof (end_op_all_or_nothing c st o st1 Hend) as [Hops [Hvals _]].
    destruct (IH c st1 st' Hbuild) as [Hops' Hvals'].
    split.
    + rewrite Hops', Hops, <- app_assoc. reflexivity.
    + rewrite Hvals', Hvals. lia.
Qed.

Lemma build_uses_below :
  forall os c st st',
    build c st os = Some st' ->
    forall k u,
      k < length os -> In u (uses (nth k os opnil)) ->
      u < nvals st + sumres (firstn k os).
Proof.
  induction os as [|o rest IH]; simpl; intros c st st' Hbuild k u Hk Hin.
  - lia.
  - destruct (end_op c st o) as [st1 |] eqn:Hend; [| discriminate].
    assert (Hck : operands_ok st o = true).
    { unfold end_op in Hend.
      destruct (operands_ok st o) eqn:Hop; [reflexivity |].
      simpl in Hend. discriminate. }
    pose proof (end_op_all_or_nothing c st o st1 Hend) as [_ [Hvals _]].
    destruct k as [|k'].
    + simpl in Hin. simpl.
      unfold operands_ok in Hck.
      rewrite forallb_forall in Hck.
      pose proof (Hck u Hin) as Hlt.
      apply Nat.ltb_lt in Hlt. lia.
    + simpl in Hin, Hk. simpl.
      assert (Hk' : k' < length rest) by lia.
      pose proof (IH c st1 st' Hbuild k' u Hk' Hin) as Hlt.
      rewrite Hvals in Hlt. lia.
Qed.

(* THE CENTRAL SSA FACT.  Models IR-OP:OPERANDS-CK (op.f:580-585) inside
   IR-OP:END-OP (op.f:806): every operand of the operation at index k names
   a value ordinal strictly below the first ordinal that operation itself
   receives. *)
Theorem operand_strictly_below_own_results :
  forall c v0 os st,
    build c (MkOState [] v0 0) os = Some st ->
    forall k u,
      k < length os -> In u (uses (nth k os opnil)) -> u < vbase v0 os k.
Proof.
  intros c v0 os st Hbuild k u Hk Hin.
  unfold vbase.
  apply (build_uses_below os c (MkOState [] v0 0) st Hbuild k u Hk Hin).
Qed.

(* An operation cannot take its own result: op.f:80-82. *)
Theorem op_cannot_take_own_result :
  forall c v0 os st,
    build c (MkOState [] v0 0) os = Some st ->
    forall k u,
      k < length os -> In u (uses (nth k os opnil)) -> ~ defines v0 os k u.
Proof.
  intros c v0 os st Hbuild k u Hk Hin [Hlo _].
  pose proof (operand_strictly_below_own_results c v0 os st Hbuild k u Hk Hin).
  lia.
Qed.

(* ---- the use relation is well founded ---------------------------- *)

(* Value u is used in the definition of value v. *)
Definition use_rel (v0 : nat) (os : list op) (u v : nat) : Prop :=
  exists k, k < length os
            /\ defines v0 os k v
            /\ In u (uses (nth k os opnil)).

Lemma use_rel_decreasing :
  forall c v0 os st,
    build c (MkOState [] v0 0) os = Some st ->
    forall u v, use_rel v0 os u v -> u < v.
Proof.
  intros c v0 os st Hbuild u v [k [Hk [[Hlo _] Hin]]].
  pose proof (operand_strictly_below_own_results c v0 os st Hbuild k u Hk Hin).
  lia.
Qed.

(* Hence every walk from a value terminates: dominance is a property of the
   construction order, not something a later pass has to detect. *)
Theorem use_well_founded :
  forall c v0 os st,
    build c (MkOState [] v0 0) os = Some st ->
    well_founded (use_rel v0 os).
Proof.
  intros c v0 os st Hbuild.
  apply (well_founded_lt_compat nat (fun n => n)).
  intros x y Hrel.
  apply (use_rel_decreasing c v0 os st Hbuild x y Hrel).
Qed.

(* "No value uses itself" is not published on its own: it is the one-step case
   of `no_definitional_cycle` below, which quantifies over chains of any
   length, and `op_cannot_take_own_result` above already states it at the
   operation.  Publishing all three would report one fact three times. *)

(* A definitional chain of any length. *)
Inductive use_path (v0 : nat) (os : list op) : nat -> nat -> Prop :=
| use_path_one :
    forall u v, use_rel v0 os u v -> use_path v0 os u v
| use_path_step :
    forall u w v,
      use_rel v0 os w v -> use_path v0 os u w -> use_path v0 os u v.

Lemma use_path_decreasing :
  forall c v0 os st,
    build c (MkOState [] v0 0) os = Some st ->
    forall u v, use_path v0 os u v -> u < v.
Proof.
  intros c v0 os st Hbuild u v Hpath.
  induction Hpath as [u v Hrel | u w v Hrel Hpath IH].
  - apply (use_rel_decreasing c v0 os st Hbuild u v Hrel).
  - pose proof (use_rel_decreasing c v0 os st Hbuild w v Hrel). lia.
Qed.

(* No value can transitively depend on itself. *)
Theorem no_definitional_cycle :
  forall c v0 os st,
    build c (MkOState [] v0 0) os = Some st ->
    forall v, ~ use_path v0 os v v.
Proof.
  intros c v0 os st Hbuild v Hpath.
  pose proof (use_path_decreasing c v0 os st Hbuild v v Hpath). lia.
Qed.

(* ---- the strictly-below check is load bearing -------------------- *)

(* The off-by-one mutation: `<=` where op.f:584 writes `>=` on the other
   side of the comparison, that is, allowing an operand to name the ordinal
   the operation itself is about to receive. *)
Definition lax_operands_ok (st : ostate) (o : op) : bool :=
  forallb (fun r => Nat.leb r (nvals st)) (uses o).

Definition lax_end_op (c : ocaps) (st : ostate) (o : op) : option ostate :=
  if lax_operands_ok st o && room_ok c st o
  then Some (MkOState (ops st ++ [o]) (nvals st + nres o) (pcells st + op_cells o))
  else None.

Fixpoint lax_build (c : ocaps) (st : ostate) (os : list op) : option ostate :=
  match os with
  | [] => Some st
  | o :: rest =>
      match lax_end_op c st o with
      | Some st' => lax_build c st' rest
      | None => None
      end
  end.

(* Dropping the check altogether. *)
Definition open_end_op (c : ocaps) (st : ostate) (o : op) : option ostate :=
  if room_ok c st o
  then Some (MkOState (ops st ++ [o]) (nvals st + nres o) (pcells st + op_cells o))
  else None.

Fixpoint open_build (c : ocaps) (st : ostate) (os : list op) : option ostate :=
  match os with
  | [] => Some st
  | o :: rest =>
      match open_end_op c st o with
      | Some st' => open_build c st' rest
      | None => None
      end
  end.

Definition big : ocaps := MkOCaps 8 8 64.
Definition empty_store : ostate := MkOState [] 0 0.

(* One operation taking its own result as an operand. *)
Definition self_program : list op := [MkOp [0] 1 0 0].

(* Two operations, each taking the other's result. *)
Definition cyclic_program : list op := [MkOp [1] 1 0 0; MkOp [0] 1 0 0].

Example strict_rejects_self_reference :
  build big empty_store self_program = None.
Proof. vm_compute. reflexivity. Qed.

Example strict_rejects_cycle :
  build big empty_store cyclic_program = None.
Proof. vm_compute. reflexivity. Qed.

Example off_by_one_accepts_self_reference :
  lax_build big empty_store self_program
    = Some (MkOState [MkOp [0] 1 0 0] 1 2).
Proof. vm_compute. reflexivity. Qed.

Example no_check_accepts_cycle :
  open_build big empty_store cyclic_program
    = Some (MkOState [MkOp [1] 1 0 0; MkOp [0] 1 0 0] 2 4).
Proof. vm_compute. reflexivity. Qed.

(* An off-by-one in the operand bound admits a value that is its own
   operand: the relation the strict check makes well founded gets a loop of
   length one. *)
Theorem off_by_one_admits_self_definition :
  use_rel 0 self_program 0 0.
Proof.
  exists 0. unfold self_program, defines, vbase. simpl.
  repeat split; try lia; auto.
Qed.

(* Dropping the check admits a two-step definitional cycle. *)
Theorem no_check_admits_definitional_cycle :
  use_rel 0 cyclic_program 1 0
  /\ use_rel 0 cyclic_program 0 1
  /\ use_path 0 cyclic_program 0 0.
Proof.
  assert (H10 : use_rel 0 cyclic_program 1 0).
  { exists 0. unfold cyclic_program, defines, vbase. simpl.
    repeat split; try lia; auto. }
  assert (H01 : use_rel 0 cyclic_program 0 1).
  { exists 1. unfold cyclic_program, defines, vbase. simpl.
    repeat split; try lia; auto. }
  split; [exact H10 | split; [exact H01 |]].
  apply (use_path_step 0 cyclic_program 0 1 0 H10).
  apply use_path_one. exact H01.
Qed.

(* And the two mutations really do differ from the shipped check on the same
   program, so the counterexamples are about this code and not about a
   different one. *)
Theorem strictly_below_check_is_load_bearing :
  build big empty_store self_program = None
  /\ lax_build big empty_store self_program <> None
  /\ build big empty_store cyclic_program = None
  /\ open_build big empty_store cyclic_program <> None.
Proof.
  split; [| split; [| split]].
  - vm_compute. reflexivity.
  - vm_compute. discriminate.
  - vm_compute. reflexivity.
  - vm_compute. discriminate.
Qed.

(* ================================================================== *)
(* Part 4.  IR-FUN: blocks over operations, functions over blocks.     *)
(* ================================================================== *)

Record blk : Type := MkBlk {
  b_ops    : win;   (* design line 402, fun.f:236-237 *)
  b_parent : nat;   (* design line 400, fun.f:233 *)
  b_term   : nat;   (* design line 403, fun.f:238 *)
  b_args   : win    (* design line 401, fun.f:234-235 *)
}.

Record fnr : Type := MkFn {
  f_blocks : win;   (* design lines 386-387, fun.f:221-222 *)
  f_attrs  : win    (* design line 388, fun.f:223-224 *)
}.

Definition bnil : blk := MkBlk w0 0 0 w0.
Definition fnil : fnr := MkFn w0 w0.

Definition nthb (bs : list blk) (k : nat) : blk := nth k bs bnil.
Definition nthf (fs : list fnr) (k : nat) : fnr := nth k fs fnil.

Lemma nthw_map_bops :
  forall bs k, nthw (map b_ops bs) k = b_ops (nthb bs k).
Proof.
  intros bs k. unfold nthw, nthb. apply (map_nth b_ops bs bnil k).
Qed.

Lemma nthw_map_fblocks :
  forall fs k, nthw (map f_blocks fs) k = f_blocks (nthf fs k).
Proof.
  intros fs k. unfold nthw, nthf. apply (map_nth f_blocks fs fnil k).
Qed.

Lemma map_last_gen :
  forall (A B : Type) (f : A -> B) (l : list A) (d : A),
    l <> [] -> last (map f l) (f d) = f (last l d).
Proof.
  intros A B f l d.
  induction l as [|a l IH]; intros Hne.
  - contradiction.
  - destruct l as [|b l'].
    + reflexivity.
    + simpl in *. apply IH. discriminate.
Qed.

(* IR-FUN's two tilings.  fun.f:69-72 for blocks over IR-OP's operation
   table, fun.f:544-546 for functions over the block table. *)
Definition blocks_tile (bs : list blk) : Prop := contig 0 (map b_ops bs).
Definition funs_tile (fs : list fnr) : Prop := contig 0 (map f_blocks fs).

(* IR-FUN:END-BLOCK's STEP-CK reads the previous block's operation-window
   end, or zero for the first block (fun.f:1039).  IR-FUN:END-FUN does the
   same for the block window (fun.f:973). *)
Definition block_start (bs : list blk) : nat :=
  match bs with
  | [] => 0
  | _ => wend (b_ops (last bs bnil))
  end.

Definition fun_block_start (fs : list fnr) : nat :=
  match fs with
  | [] => 0
  | _ => wend (f_blocks (last fs fnil))
  end.

Lemma blocks_span_is_block_start :
  forall bs, blocks_tile bs -> span 0 (map b_ops bs) = block_start bs.
Proof.
  intros bs Htile. unfold blocks_tile in Htile.
  destruct bs as [|b bs'].
  - unfold span, block_start. reflexivity.
  - unfold block_start.
    rewrite (coverage_check_is_last_row_end 0 (map b_ops (b :: bs')) Htile)
      by discriminate.
    change w0 with (b_ops bnil).
    rewrite (map_last_gen blk win b_ops (b :: bs') bnil) by discriminate.
    reflexivity.
Qed.

Lemma funs_span_is_fun_block_start :
  forall fs, funs_tile fs -> span 0 (map f_blocks fs) = fun_block_start fs.
Proof.
  intros fs Htile. unfold funs_tile in Htile.
  destruct fs as [|f fs'].
  - unfold span, fun_block_start. reflexivity.
  - unfold fun_block_start.
    rewrite (coverage_check_is_last_row_end 0 (map f_blocks (f :: fs')) Htile)
      by discriminate.
    change w0 with (f_blocks fnil).
    rewrite (map_last_gen fnr win f_blocks (f :: fs') fnil) by discriminate.
    reflexivity.
Qed.

(* IR-FUN:END-BLOCK (fun.f:1031-1048).  `opst` is what IR-FUN:BEGIN-BLOCK
   captured (fun.f:994); `opn` is IR-OP:OPS minus it (fun.f:1040); the
   terminator ordinal written is `opst opn + 1-` (fun.f:847). *)
Definition end_block (bs : list blk) (opst opn parent agst agn : nat)
  : option blk :=
  if Nat.eqb opst (block_start bs) && Nat.ltb 0 opn
  then Some (MkBlk (MkWin opst opn) parent (opst + opn - 1) (MkWin agst agn))
  else None.

(* IR-FUN:END-FUN's block window (fun.f:972-975). *)
Definition end_fun (fs : list fnr) (bst bn atst atn : nat) : option fnr :=
  if Nat.eqb bst (fun_block_start fs)
  then Some (MkFn (MkWin bst bn) (MkWin atst atn))
  else None.

(* Every block IR-FUN:END-BLOCK appends keeps the block table's operation
   windows a tiling of IR-OP's operation table. *)
Theorem end_block_preserves_tiling :
  forall bs opst opn parent agst agn b,
    blocks_tile bs ->
    end_block bs opst opn parent agst agn = Some b ->
    blocks_tile (bs ++ [b]).
Proof.
  intros bs opst opn parent agst agn b Htile Hend.
  unfold end_block in Hend.
  destruct (Nat.eqb opst (block_start bs) && Nat.ltb 0 opn) eqn:Hck;
    [| discriminate].
  apply andb_true_iff in Hck as [Heq _].
  apply Nat.eqb_eq in Heq.
  inversion Hend. subst b.
  unfold blocks_tile in *.
  rewrite map_app. simpl.
  apply contig_snoc; [exact Htile |].
  simpl. rewrite (blocks_span_is_block_start bs Htile). exact Heq.
Qed.

Theorem end_fun_preserves_tiling :
  forall fs bst bn atst atn f,
    funs_tile fs ->
    end_fun fs bst bn atst atn = Some f ->
    funs_tile (fs ++ [f]).
Proof.
  intros fs bst bn atst atn f Htile Hend.
  unfold end_fun in Hend.
  destruct (Nat.eqb bst (fun_block_start fs)) eqn:Hck; [| discriminate].
  apply Nat.eqb_eq in Hck.
  inversion Hend. subst f.
  unfold funs_tile in *.
  rewrite map_app. simpl.
  apply contig_snoc; [exact Htile |].
  simpl. rewrite (funs_span_is_fun_block_start fs Htile). exact Hck.
Qed.

(* Every operation the blocks reach belongs to exactly one block, with no
   scan and no agreement check: fun.f:66-76. *)
Theorem op_belongs_to_exactly_one_block :
  forall bs nops,
    blocks_tile bs ->
    span 0 (map b_ops bs) = nops ->
    forall i, i < nops ->
      exists ! k, k < length bs /\ covers (b_ops (nthb bs k)) i.
Proof.
  intros bs nops Htile Hcov i Hi.
  destruct (proj1 (coverage_closes_the_gap 0 (map b_ops bs) nops Htile Hcov)
              i (Nat.le_0_l i) Hi) as [k [[Hk Hc] Huniq]].
  rewrite length_map in Hk.
  rewrite nthw_map_bops in Hc.
  exists k. split; [split; assumption |].
  intros k' [Hk' Hc'].
  apply Huniq. unfold owner. split.
  - rewrite length_map. exact Hk'.
  - rewrite nthw_map_bops. exact Hc'.
Qed.

(* FINDING F1.  Contiguity alone leaves every operation past the last
   block's end unowned, and no check in IR-FUN looks there. *)
Theorem orphan_op_outside_every_block :
  forall bs nops,
    blocks_tile bs ->
    span 0 (map b_ops bs) < nops ->
    exists i,
      i < nops
      /\ forall k, k < length bs -> ~ covers (b_ops (nthb bs k)) i.
Proof.
  intros bs nops Htile Hlt.
  exists (span 0 (map b_ops bs)).
  split; [exact Hlt |].
  intros k Hk Hcov.
  apply (tiling_says_nothing_past_the_span 0 (map b_ops bs)
           (span 0 (map b_ops bs)) Htile (Nat.le_refl _)).
  exists k. unfold owner. split.
  - rewrite length_map. exact Hk.
  - rewrite nthw_map_bops. exact Hcov.
Qed.

(* The same one level up: blocks left behind by an abandoned function.
   IR-FUN:BEGIN-FUN captures the block count (fun.f:900) and
   IR-FUN:END-FUN's STEP-CK (fun.f:973) refuses to absorb them, so they are
   past the last function's block window and belong to no function. *)
Theorem orphan_block_outside_every_function :
  forall fs nblocks,
    funs_tile fs ->
    span 0 (map f_blocks fs) < nblocks ->
    exists i,
      i < nblocks
      /\ forall k, k < length fs -> ~ covers (f_blocks (nthf fs k)) i.
Proof.
  intros fs nblocks Htile Hlt.
  exists (span 0 (map f_blocks fs)).
  split; [exact Hlt |].
  intros k Hk Hcov.
  apply (tiling_says_nothing_past_the_span 0 (map f_blocks fs)
           (span 0 (map f_blocks fs)) Htile (Nat.le_refl _)).
  exists k. unfold owner. split.
  - rewrite length_map. exact Hk.
  - rewrite nthw_map_fblocks. exact Hcov.
Qed.

(* Executable: one block covering operations 0 and 1, an operation table
   holding three, and operation 2 owned by nothing. *)
Definition one_block : list blk := [MkBlk (MkWin 0 2) 0 1 w0].

Example orphan_op_counterexample :
  span 0 (map b_ops one_block) = 2.
Proof. vm_compute. reflexivity. Qed.

Theorem orphan_op_counterexample_is_real :
  blocks_tile one_block
  /\ span 0 (map b_ops one_block) < 3
  /\ (forall k, k < length one_block -> ~ covers (b_ops (nthb one_block k)) 2).
Proof.
  unfold blocks_tile, one_block, span, nthb, covers, wend.
  split; [| split].
  - simpl. split; [reflexivity | exact I].
  - simpl. lia.
  - intros k Hk Hcov. simpl in Hk.
    destruct k as [|k]; simpl in *; lia.
Qed.

(* ---- the parent relation ----------------------------------------- *)

(* IR-FUN:PARENTS-CK (fun.f:770-774), also rechecked by IR-FUN:BLOCK@
   (fun.f:1136): every block a function's window claims names that
   function. *)
Definition parents_forward (bs : list blk) (fs : list fnr) : Prop :=
  forall f j,
    f < length fs -> j < wlen (f_blocks (nthf fs f)) ->
    b_parent (nthb bs (wst (f_blocks (nthf fs f)) + j)) = f.

(* What IR-FUN:PARENT@ (fun.f:1142-1147) does NOT check: that the function a
   block names really claims it.  PARENT@ bound-checks the stored ordinal
   against the function table's count and stops there. *)
Definition parents_backward (bs : list blk) (fs : list fnr) : Prop :=
  forall b,
    b < length bs ->
    b_parent (nthb bs b) < length fs
    /\ covers (f_blocks (nthf fs (b_parent (nthb bs b)))) b.

(* The block window is the single authority for "which block owns this
   operation": there is no stored back-reference on an operation row, so
   nothing can disagree with it (op.f:42-43, fun.f:66-76). *)
Theorem operation_parent_is_single_authority :
  forall bs,
    blocks_tile bs ->
    forall i j k,
      j < length bs -> k < length bs ->
      covers (b_ops (nthb bs j)) i -> covers (b_ops (nthb bs k)) i ->
      j = k.
Proof.
  intros bs Htile i j k Hj Hk Hcj Hck.
  apply (tiling_no_overlap 0 (map b_ops bs) Htile j k i).
  - rewrite length_map. exact Hj.
  - rewrite length_map. exact Hk.
  - rewrite nthw_map_bops. exact Hcj.
  - rewrite nthw_map_bops. exact Hck.
Qed.

(* The function-to-block relation IS stored twice by design mandate — the
   function row's block window and the block row's parent field — and the
   two agree only when the cross-check runs in both directions.  Forward
   plus coverage gives backward. *)
Theorem parents_forward_and_coverage_give_backward :
  forall bs fs,
    funs_tile fs ->
    span 0 (map f_blocks fs) = length bs ->
    parents_forward bs fs ->
    parents_backward bs fs.
Proof.
  intros bs fs Htile Hcov Hfwd b Hb.
  destruct (proj1 (coverage_closes_the_gap 0 (map f_blocks fs)
                     (length bs) Htile Hcov) b (Nat.le_0_l b) Hb)
    as [f [[Hf Hc] _]].
  rewrite length_map in Hf.
  rewrite nthw_map_fblocks in Hc.
  destruct Hc as [Hlo Hhi]. unfold wend in Hhi.
  assert (Hj : b - wst (f_blocks (nthf fs f)) < wlen (f_blocks (nthf fs f)))
    by lia.
  pose proof (Hfwd f (b - wst (f_blocks (nthf fs f))) Hf Hj) as Hpar.
  replace (wst (f_blocks (nthf fs f)) + (b - wst (f_blocks (nthf fs f))))
    with b in Hpar by lia.
  rewrite Hpar.
  split; [exact Hf | split; [exact Hlo | unfold wend; lia]].
Qed.

(* Without the coverage comparison the forward check passes while the two
   records disagree, and IR-FUN:PARENT@'s bound check does not notice: the
   stored parent is a real function ordinal, it just does not claim this
   block.  This is a block that outlived an abandoned function. *)
Definition orphan_parent_fs : list fnr := [MkFn (MkWin 0 1) w0].
Definition orphan_parent_bs : list blk :=
  [MkBlk (MkWin 0 1) 0 0 w0; MkBlk (MkWin 1 1) 0 1 w0].

Theorem parent_bound_check_does_not_imply_agreement :
  funs_tile orphan_parent_fs
  /\ blocks_tile orphan_parent_bs
  /\ parents_forward orphan_parent_bs orphan_parent_fs
  /\ b_parent (nthb orphan_parent_bs 1) < length orphan_parent_fs
  /\ ~ parents_backward orphan_parent_bs orphan_parent_fs
  /\ span 0 (map f_blocks orphan_parent_fs) <> length orphan_parent_bs.
Proof.
  unfold funs_tile, blocks_tile, parents_forward, parents_backward,
         orphan_parent_fs, orphan_parent_bs, nthb, nthf, covers, wend, span.
  split; [| split; [| split; [| split; [| split]]]].
  - simpl. split; [reflexivity | exact I].
  - simpl. split; [reflexivity | split; [reflexivity | exact I]].
  - intros f j Hf Hj. simpl in Hf.
    destruct f as [|f]; simpl in *; [| lia].
    destruct j as [|j]; simpl in *; [reflexivity | lia].
  - simpl. lia.
  - intros Hback.
    assert (H1 : 1 < 2) by lia.
    destruct (Hback 1 H1) as [_ [_ Hhi]]. simpl in Hhi. lia.
  - simpl. lia.
Qed.

(* ---- the terminator ---------------------------------------------- *)

(* IR-FUN:TERM-CK (fun.f:789-794) with IR-FUN:TERM-AT-CK (fun.f:779-787):
   the block must hold at least one operation, and for every operation in
   the window "is a terminator" must agree with "is the last one". *)
Definition term_ck (is_term : nat -> bool) (opst opn : nat) : bool :=
  Nat.ltb 0 opn
  && forallb (fun i => Bool.eqb (is_term (opst + i)) (Nat.eqb i (opn - 1)))
             (seq 0 opn).

Theorem term_ck_rejects_empty_block :
  forall is_term opst, term_ck is_term opst 0 = false.
Proof.
  intros is_term opst. unfold term_ck. reflexivity.
Qed.

Theorem term_ck_sound :
  forall is_term opst opn,
    term_ck is_term opst opn = true ->
    0 < opn
    /\ is_term (opst + (opn - 1)) = true
    /\ forall i, i < opn - 1 -> is_term (opst + i) = false.
Proof.
  intros is_term opst opn Hck.
  unfold term_ck in Hck.
  apply andb_true_iff in Hck as [Hpos Hall].
  apply Nat.ltb_lt in Hpos.
  rewrite forallb_forall in Hall.
  assert (Hlast : In (opn - 1) (seq 0 opn)).
  { apply in_seq. lia. }
  pose proof (Hall (opn - 1) Hlast) as Hl.
  rewrite Nat.eqb_refl in Hl.
  apply eqb_prop in Hl.
  split; [exact Hpos | split; [exact Hl |]].
  intros i Hi.
  assert (Hin : In i (seq 0 opn)) by (apply in_seq; lia).
  pose proof (Hall i Hin) as Hi'.
  replace (Nat.eqb i (opn - 1)) with false in Hi'
    by (symmetry; apply Nat.eqb_neq; lia).
  destruct (is_term (opst + i)); [discriminate | reflexivity].
Qed.

Theorem term_ck_complete :
  forall is_term opst opn,
    0 < opn ->
    is_term (opst + (opn - 1)) = true ->
    (forall i, i < opn - 1 -> is_term (opst + i) = false) ->
    term_ck is_term opst opn = true.
Proof.
  intros is_term opst opn Hpos Hlast Hrest.
  unfold term_ck.
  apply andb_true_iff. split; [apply Nat.ltb_lt; exact Hpos |].
  apply forallb_forall. intros i Hin.
  apply in_seq in Hin as [_ Hi]. simpl in Hi.
  destruct (Nat.eq_dec i (opn - 1)) as [Heq | Hne].
  - subst i. rewrite Nat.eqb_refl, Hlast. reflexivity.
  - replace (Nat.eqb i (opn - 1)) with false
      by (symmetry; apply Nat.eqb_neq; exact Hne).
    rewrite (Hrest i ltac:(lia)). reflexivity.
Qed.

(* Exactly one terminator, and it is the last operation: design line 403. *)
Theorem terminator_is_unique_and_last :
  forall is_term opst opn,
    term_ck is_term opst opn = true ->
    forall i j,
      i < opn -> j < opn ->
      is_term (opst + i) = true -> is_term (opst + j) = true ->
      i = j /\ i = opn - 1.
Proof.
  intros is_term opst opn Hck i j Hi Hj Hti Htj.
  destruct (term_ck_sound is_term opst opn Hck) as [Hpos [_ Hnone]].
  assert (Hilast : i = opn - 1).
  { destruct (Nat.eq_dec i (opn - 1)) as [Heq | Hne]; [exact Heq |].
    rewrite (Hnone i ltac:(lia)) in Hti. discriminate. }
  assert (Hjlast : j = opn - 1).
  { destruct (Nat.eq_dec j (opn - 1)) as [Heq | Hne]; [exact Heq |].
    rewrite (Hnone j ltac:(lia)) in Htj. discriminate. }
  split; [lia | exact Hilast].
Qed.

(* FINDING F2.  The stored terminator ordinal is a function of the operation
   window, so IR-FUN:TERMINATOR@'s comparison against `BOP-END 1-` can never
   fail on a row this package built.  The field is a second authority that
   is definitionally in agreement — it carries no information. *)
Theorem stored_terminator_is_derived :
  forall bs opst opn parent agst agn b,
    end_block bs opst opn parent agst agn = Some b ->
    b_term b = wend (b_ops b) - 1 /\ 0 < wlen (b_ops b).
Proof.
  intros bs opst opn parent agst agn b Hend.
  unfold end_block in Hend.
  destruct (Nat.eqb opst (block_start bs) && Nat.ltb 0 opn) eqn:Hck;
    [| discriminate].
  apply andb_true_iff in Hck as [_ Hpos].
  apply Nat.ltb_lt in Hpos.
  inversion Hend. subst b. simpl. unfold wend. simpl.
  split; [reflexivity | exact Hpos].
Qed.

(* ================================================================== *)
(* Part 5.  IR-FUN: a block's argument run.                            *)
(* ================================================================== *)

Inductive vkind : Type := KArg | KRes.

Record vrow : Type := MkVRow {
  v_kind : vkind;
  v_def  : nat;   (* the block or operation that defined it, op.f:193 *)
  v_pos  : nat    (* the position within that definition, op.f:194 *)
}.

Definition vnil : vrow := MkVRow KRes 0 0.

Definition is_arg (r : vrow) : bool :=
  match v_kind r with KArg => true | KRes => false end.

(* IR-FUN:ARGS-CK (fun.f:800-808), also rechecked one element at a time by
   IR-FUN:ARG@ (fun.f:1154-1167): the value must be a block argument, of
   this block, at this position. *)
Definition args_ck (vs : list vrow) (l agst agn : nat) : bool :=
  forallb (fun i =>
             match nth_error vs (agst + i) with
             | Some r => is_arg r && Nat.eqb (v_def r) l && Nat.eqb (v_pos r) i
             | None => false
             end)
          (seq 0 agn).

Theorem args_ck_sound :
  forall vs l agst agn,
    args_ck vs l agst agn = true ->
    forall i, i < agn -> nth_error vs (agst + i) = Some (MkVRow KArg l i).
Proof.
  intros vs l agst agn Hck i Hi.
  unfold args_ck in Hck.
  rewrite forallb_forall in Hck.
  assert (Hin : In i (seq 0 agn)) by (apply in_seq; lia).
  pose proof (Hck i Hin) as Hrow.
  destruct (nth_error vs (agst + i)) as [r |] eqn:Hnth; [| discriminate].
  apply andb_true_iff in Hrow as [Hrow Hpos].
  apply andb_true_iff in Hrow as [Hkind Hdef].
  apply Nat.eqb_eq in Hdef. apply Nat.eqb_eq in Hpos.
  destruct r as [k d p]. unfold is_arg in Hkind. simpl in *.
  destruct k; [| discriminate].
  congruence.
Qed.

(* fun.f:98-101, and it holds: two different blocks cannot share an argument
   value, because the value row names exactly one block. *)
Theorem arg_values_are_not_shared :
  forall vs l1 a1 n1 l2 a2 n2,
    args_ck vs l1 a1 n1 = true ->
    args_ck vs l2 a2 n2 = true ->
    l1 <> l2 ->
    forall i j, i < n1 -> j < n2 -> a1 + i <> a2 + j.
Proof.
  intros vs l1 a1 n1 l2 a2 n2 H1 H2 Hne i j Hi Hj Heq.
  pose proof (args_ck_sound vs l1 a1 n1 H1 i Hi) as R1.
  pose proof (args_ck_sound vs l2 a2 n2 H2 j Hj) as R2.
  rewrite Heq in R1. rewrite R1 in R2.
  inversion R2. contradiction.
Qed.

(* FINDING F3.  The dual is false.  Two DIFFERENT values can both claim to
   be block 0's argument at position 0, and IR-FUN:ARGS-CK accepts the run
   that starts at the second one.  Value 0 here is what an abandoned block
   leaves behind: IR-FUN:ADD-BLOCK-ARG names the block as `b BCNT`
   (fun.f:1010), which is the ordinal the NEXT block receives. *)
Definition duplicated_args : list vrow :=
  [MkVRow KArg 0 0; MkVRow KArg 0 0].

Theorem arg_identity_is_not_injective :
  args_ck duplicated_args 0 1 1 = true
  /\ nth_error duplicated_args 0 = Some (MkVRow KArg 0 0)
  /\ nth_error duplicated_args 1 = Some (MkVRow KArg 0 0)
  /\ 0 <> 1.
Proof.
  unfold duplicated_args.
  split; [| split; [| split]].
  - vm_compute. reflexivity.
  - vm_compute. reflexivity.
  - vm_compute. reflexivity.
  - discriminate.
Qed.

(* The check that closes it: every value row of argument kind must sit at
   exactly the offset its own block's argument window puts it at.  That is a
   whole-table comparison, not an O(1) read, and nothing in IR-FUN performs
   it — it belongs to the section 6.5 freeze verifier alongside the
   operation coverage check of F1. *)
Definition arg_rows_owned (vs : list vrow) (bs : list blk) : Prop :=
  forall v r,
    nth_error vs v = Some r -> is_arg r = true ->
    v_def r < length bs
    /\ v_pos r < wlen (b_args (nthb bs (v_def r)))
    /\ wst (b_args (nthb bs (v_def r))) + v_pos r = v.

Theorem arg_ownership_forces_injectivity :
  forall vs bs,
    arg_rows_owned vs bs ->
    forall v1 v2 r1 r2,
      nth_error vs v1 = Some r1 -> nth_error vs v2 = Some r2 ->
      is_arg r1 = true -> is_arg r2 = true ->
      v_def r1 = v_def r2 -> v_pos r1 = v_pos r2 ->
      v1 = v2.
Proof.
  intros vs bs Howned v1 v2 r1 r2 H1 H2 A1 A2 Hdef Hpos.
  destruct (Howned v1 r1 H1 A1) as [_ [_ Ho1]].
  destruct (Howned v2 r2 H2 A2) as [_ [_ Ho2]].
  rewrite <- Ho1, <- Ho2, Hdef, Hpos. reflexivity.
Qed.

Definition dup_arg_blocks : list blk := [MkBlk (MkWin 0 1) 0 0 (MkWin 1 1)].

Theorem duplicated_args_violate_ownership :
  ~ arg_rows_owned duplicated_args dup_arg_blocks.
Proof.
  intros Howned.
  destruct (Howned 0 (MkVRow KArg 0 0) eq_refl eq_refl) as [_ [_ Ho]].
  unfold dup_arg_blocks, nthb in Ho. simpl in Ho. discriminate.
Qed.

(* ================================================================== *)
(* What every published result rests on.                               *)
(* ================================================================== *)

Print Assumptions tiling_no_overlap.
Print Assumptions tiling_partition.
Print Assumptions neighbour_check_is_contiguity.
Print Assumptions neighbour_check_gives_non_overlap.
Print Assumptions non_overlap_does_not_imply_neighbour_check.
Print Assumptions single_row_check_is_not_enough.
Print Assumptions tiling_says_nothing_past_the_span.
Print Assumptions coverage_check_is_last_row_end.
Print Assumptions coverage_closes_the_gap.
Print Assumptions coverage_is_necessary.
Print Assumptions op_row_windows_tile.
Print Assumptions op_pool_windows_tile.
Print Assumptions op_pool_cell_has_one_owner.
Print Assumptions non_tiling_admits_double_ownership.
Print Assumptions ceiling_fail_closed.
Print Assumptions ssa_violation_fail_closed.
Print Assumptions write_order_is_load_bearing.
Print Assumptions operand_strictly_below_own_results.
Print Assumptions op_cannot_take_own_result.
Print Assumptions use_well_founded.
Print Assumptions no_definitional_cycle.
Print Assumptions off_by_one_admits_self_definition.
Print Assumptions no_check_admits_definitional_cycle.
Print Assumptions strictly_below_check_is_load_bearing.
Print Assumptions end_block_preserves_tiling.
Print Assumptions end_fun_preserves_tiling.
Print Assumptions op_belongs_to_exactly_one_block.
Print Assumptions orphan_op_outside_every_block.
Print Assumptions orphan_block_outside_every_function.
Print Assumptions orphan_op_counterexample_is_real.
Print Assumptions operation_parent_is_single_authority.
Print Assumptions parents_forward_and_coverage_give_backward.
Print Assumptions parent_bound_check_does_not_imply_agreement.
Print Assumptions term_ck_rejects_empty_block.
Print Assumptions term_ck_sound.
Print Assumptions term_ck_complete.
Print Assumptions terminator_is_unique_and_last.
Print Assumptions stored_terminator_is_derived.
Print Assumptions args_ck_sound.
Print Assumptions arg_values_are_not_shared.
Print Assumptions arg_identity_is_not_injective.
Print Assumptions arg_ownership_forces_injectivity.
Print Assumptions duplicated_args_violate_ownership.
