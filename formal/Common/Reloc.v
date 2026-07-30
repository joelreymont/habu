(* Habu.Common.Reloc — a model of the snapshot relocation round trip.

   A snapshot image has to boot in a process that is not the one that wrote
   it.  The run that writes an image and the run that restores it get the
   engine's loaded __text base from the loader and the JIT region's base from
   the kernel, independently of one another.  Two classes of persisted value
   depend on those two bases:

     - the immediate of every call that leaves the region for the engine's
       __text.  A call from one region address to another keeps its distance
       wherever the region lands; a call from the region into __text does not.
       Those sites are RECORDED when the call is created (the call-site map in
       src/habu/layout.f, package SNAP-RELOC), never recognised afterwards by
       decoding region bytes, because a compiled word may carry inline data
       that looks exactly like an instruction.
     - every persisted DATA cell that was DECLARED to hold a region address:
       a deferred word's dispatch cell, and the three engine hook cells.  The
       declared set is the address-cell table in the same band.

   For both classes the writer rewrites the value into a CANONICAL form and
   the loader rewrites it again for the run it is actually in.  This file
   states, and proves, the one invariant that both halves exist to keep:

     for every recorded site, writer-side canonicalization composed with
     loader-side rebase is the IDENTITY, for any writer base and any loader
     base within BL's reach.

   Nothing said that before.  The defects this campaign paid for — a deleted
   BL relocation pass, a four-value displacement lottery, stale persisted
   cells — were each a way of breaking it.

   What the shipped code does, and what is modelled here:

     - src/habu/habu2.f SNAP-RELOC:EMIT-CALLS is the emitted call pass, run
       once by the writer (live distance -> canonical distance) and once by
       the loader (canonical distance -> live distance).  Its arithmetic is
       `imm26 := (imm26 + (delta >>a 2)) mod 2^26`, with the opcode field
       carried through untouched, and a recorded site that does not hold a
       BL is refused with exit status CALLMAP-RC rather than rewritten.
       Modelled as `shift_instr`, `reloc_walk` and `callmap_rc` below.
     - src/habu/snap-lib.f SND-CANON-XT-CELL is the writer's half of the
       address-cell pass, `cell := cell - dbase + RBASE-VA` with zero left
       alone; src/habu/habu2.f SNAP-RELOC:EMIT-XT is the loader's half,
       `cell := cell + delta` with zero left alone.  Modelled as `xt_canon`
       and `xt_rebase`.
     - src/habu/layout.f pins the constants: REGION-OFF, RBASE-VA, BL-REACH,
       REGION, CALLMAP-RC; src/habu/habu2.f pins BL-OP-HI.  Each is written
       down here as a literal and held equal to the shipped literal by the
       capacity rows of the parity gate, test/compiler/reloc-proof.f.

   The canonical forms, in words.  A call site's canonical displacement is
   the displacement it would have IF THE REGION SAT EXACTLY REGION-OFF ABOVE
   __TEXT, so an image is byte-identical whatever base the writing run got
   (`canonical_form_is_base_independent`).  A declared address cell is stored
   relative to the RBASE-VA sentinel for the same reason.

   ------------------------------------------------------------------------
   MODEL GAPS

   Places where this model and the shipped pass deliberately differ.  Each is
   named rather than idealised away in silence.

   1. Registers are unbounded integers here, not 64-bit two's complement
      words.  The shipped pass computes the delta in a 64-bit register and
      arithmetic-shifts it right by two; that is exactly division by four
      rounding towards minus infinity, which is `Z.div`, as long as no
      intermediate exceeds 63 bits.  Every quantity involved — a text base, a
      region base, their difference, a region offset — is bounded by BL-REACH
      plus an image size, some thirty bits, so the gap is real but empty.  A
      64-bit overflow is out of scope for this model.

   2. The 26-bit immediate field is modelled as `w mod 2^26` and the opcode
      as `w / 2^26`.  The shipped pass reaches the same two values with a
      shift pair (`<< 38 >> 38` for the field, `>> 26 << 26` for the opcode).
      That the shipped instruction sequence really computes these is not
      assumed here: the Habu half of this gate DECODES AND RUNS the shipped
      instruction sequence, read out of src/habu/habu2.f, over the same
      vector rows this model is asked about.

   3. The recorded set is modelled as a boolean beside each word, and the
      pass as a walk over that list.  The shipped pass derives the same
      boolean from a bitmap indexed by region word offset, and stops at the
      image's payload length.  The bitmap indexing itself is not modelled
      here; it is likewise covered by the Habu half, which builds a real map
      band and runs the shipped instruction sequence over it.

   4. A refusal is modelled as the walk stopping and reporting CALLMAP-RC.
      The shipped pass writes a diagnostic to file descriptor two and calls
      exit-group with that status.  Sites already rewritten before the bad
      one stay rewritten in both, which is why `reloc_walk` returns the
      partially rewritten list beside the status.

   5. Nothing here models concurrency, a partially written image file, or the
      dictionary-pointer pass that runs beside the call pass.  Those are
      other invariants with other owners.

   6. The completeness half below enumerates the EMITTER VOCABULARY that can
      bake an address-bearing value into region bytes, and classifies each
      member.  Two members are the shared four-instruction MOVZ/MOVK carrier
      itself (`C-ADDR-RAW` and `C-ADDR-PUSH`), whose class is decided by
      whichever named wrapper calls it.  They are recorded here as
      `Named_at_site` and treated as not region-dependent, and that is sound
      only because nothing calls them except those wrappers.  That is not
      assumed: the Habu half pins the exact caller set of both, and pins that
      the chain's scaffold constant appears in exactly one definition, so a
      second hand-built chain or a direct call to the carrier fails the gate.

   7. What the vocabulary pin cannot see.  Every address form the compiler
      emits today is built either by that one carrier, by the direct-BL
      chokepoint, or by the PC-relative `C-ADR` word, and each of those is
      pinned by name.  An emitter that assembled an address out of ordinary
      instructions through the generic word-emitter `LCEMIT` would be
      invisible to this binding: `LCEMIT` has some forty callers and is how
      every instruction reaches the region, so its caller set says nothing.
      Closing that would need the emitted bytes themselves to be classified,
      which this gate does not do.  It is a real gap and it is named here
      rather than implied.

   8. The classification records what the SNAPSHOT loader replays.  The AOT
      seed passes (`EM-AOT-RELOC-DATA`, `EM-AOT-RELOC-CODE`) relocate the same
      MOVZ/MOVK chains from their own capture-time site lists, but only inside
      the AOT capture blob and only on the seed path, so they do not make a
      producer snapshot-safe.  `code_address_chain_is_the_open_gap` below is
      the standing negative result that says so.
*)

From Stdlib Require Import ZArith Lia List Bool.
Import ListNotations.

Open Scope Z_scope.

(* ------------------------------------------------------------------ *)
(* The pinned constants.                                              *)
(*                                                                    *)
(* Every one of these is a literal here and a literal in the shipped   *)
(* source; the parity gate's capacity rows hold the two equal, so a    *)
(* renumbering on either side is a failure rather than a divergence.   *)
(* ------------------------------------------------------------------ *)

(* $25, src/habu/habu2.f: the top six bits of an AArch64 BL. *)
Definition bl_op_hi : Z := 37.

(* The imm26 field: twenty-six bits of signed instruction count. *)
Definition imm_span : Z := 67108864.
Definition imm_half : Z := 33554432.

(* $1000000, src/habu/layout.f: the canonical distance from __text to the
   region base.  A region that sat exactly here would need no call pass. *)
Definition region_off : Z := 16777216.

(* $800000, src/habu/layout.f: the JIT region's size. *)
Definition region_bytes : Z := 8388608.

(* $300000000, src/habu/layout.f: the canonical region base a declared
   address cell is stored relative to.  A portability sentinel, never a
   runtime address. *)
Definition rbase_va : Z := 12884901888.

(* $8000000, src/habu/layout.f: BL's +/- 128 MiB reach. *)
Definition bl_reach : Z := 134217728.

(* 95, src/habu/layout.f: the exit status for a recorded site that does not
   hold a call. *)
Definition callmap_rc : Z := 95.

(* ------------------------------------------------------------------ *)
(* One instruction word.                                              *)
(* ------------------------------------------------------------------ *)

Definition imm_of (w : Z) : Z := w mod imm_span.
Definition op_of (w : Z) : Z := w / imm_span.
Definition is_bl (w : Z) : bool := Z.eqb (op_of w) bl_op_hi.

(* Put a new field back under the same opcode, re-masked into imm26. *)
Definition put_imm (w f : Z) : Z := op_of w * imm_span + f mod imm_span.

(* The shipped pass's whole arithmetic, in instruction units.  x7 in
   EMIT-CALLS is the byte delta arithmetic-shifted right by two; `shift_call`
   applies that shift and `shift_instr` takes the shifted amount directly, so
   the two halves of the pass can be composed without the shift entering the
   algebra twice. *)
Definition asr2 (d : Z) : Z := d / 4.
Definition shift_instr (w k : Z) : Z := put_imm w (imm_of w + k).
Definition shift_call (w d : Z) : Z := shift_instr w (asr2 d).

(* A BL carrying this displacement, in instruction units. *)
Definition bl (d : Z) : Z := bl_op_hi * imm_span + d mod imm_span.

(* The signed displacement a word carries. *)
Definition sxt26 (f : Z) : Z := if f <? imm_half then f else f - imm_span.
Definition disp_of (w : Z) : Z := sxt26 (imm_of w).

(* Representable in imm26 at all. *)
Definition in_reach (d : Z) : Prop := - imm_half <= d < imm_half.

(* ------------------------------------------------------------------ *)
(* The two canonical directions.                                      *)
(*                                                                    *)
(* `off` is the live region base measured above the engine's __text    *)
(* base, which is what both call sites compute into x10:               *)
(* habu2.f BSNAPREBASE for the writer, EM-SNAPSHOT-RESTORE for the     *)
(* loader.                                                             *)
(* ------------------------------------------------------------------ *)

Definition wdelta (off : Z) : Z := off - region_off.
Definition ldelta (off : Z) : Z := region_off - off.
Definition canon (w off : Z) : Z := shift_call w (wdelta off).
Definition rebase (w off : Z) : Z := shift_call w (ldelta off).

(* ------------------------------------------------------------------ *)
(* Word-level lemmas.                                                 *)
(* ------------------------------------------------------------------ *)

Ltac span := try unfold imm_span in *; lia.

Lemma imm_span_pos : 0 < imm_span.
Proof. unfold imm_span; lia. Qed.

Lemma imm_bounds : forall w, 0 <= imm_of w < imm_span.
Proof. intros w. unfold imm_of. apply Z.mod_pos_bound. apply imm_span_pos. Qed.

Lemma split_word : forall w, w = op_of w * imm_span + imm_of w.
Proof.
  intros w. unfold op_of, imm_of.
  rewrite Z.mul_comm. apply Z.div_mod. span.
Qed.

Lemma op_of_put : forall w f, op_of (put_imm w f) = op_of w.
Proof.
  intros w f. unfold op_of at 1, put_imm.
  rewrite Z.div_add_l by span.
  rewrite Z.div_small by (apply Z.mod_pos_bound; span).
  lia.
Qed.

Lemma imm_of_put : forall w f, imm_of (put_imm w f) = f mod imm_span.
Proof.
  intros w f. unfold imm_of at 1, put_imm.
  rewrite Z.add_comm, Z.mod_add by span.
  apply Z.mod_mod. span.
Qed.

Lemma shift_instr_0 : forall w, shift_instr w 0 = w.
Proof.
  intros w. unfold shift_instr, put_imm.
  rewrite Z.add_0_r.
  rewrite Z.mod_small by apply imm_bounds.
  symmetry. apply split_word.
Qed.

Lemma op_of_shift : forall w k, op_of (shift_instr w k) = op_of w.
Proof. intros w k. unfold shift_instr. apply op_of_put. Qed.

Lemma imm_of_shift : forall w k,
  imm_of (shift_instr w k) = (imm_of w + k) mod imm_span.
Proof. intros w k. unfold shift_instr. apply imm_of_put. Qed.

Lemma shift_instr_compose : forall w k1 k2,
  shift_instr (shift_instr w k1) k2 = shift_instr w (k1 + k2).
Proof.
  intros w k1 k2. unfold shift_instr at 1. unfold put_imm.
  rewrite op_of_shift, imm_of_shift.
  unfold shift_instr, put_imm. f_equal.
  rewrite Z.add_mod_idemp_l by span.
  f_equal. lia.
Qed.

Lemma sxt26_of_mod : forall x, in_reach x -> sxt26 (x mod imm_span) = x.
Proof.
  intros x [Hlo Hhi]. unfold sxt26.
  destruct (Z.le_gt_cases 0 x) as [Hx | Hx].
  - rewrite Z.mod_small by (unfold imm_span, imm_half in *; lia).
    destruct (Z.ltb_spec x imm_half); [reflexivity | lia].
  - replace (x mod imm_span) with (x + imm_span).
    + destruct (Z.ltb_spec (x + imm_span) imm_half);
        unfold imm_span, imm_half in *; lia.
    + rewrite <- (Z.mod_add x 1 imm_span) by span.
      rewrite Z.mod_small by (unfold imm_span, imm_half in *; lia).
      lia.
Qed.

Lemma imm_disp_congruent : forall w k,
  (imm_of w + k) mod imm_span = (disp_of w + k) mod imm_span.
Proof.
  intros w k. unfold disp_of, sxt26.
  destruct (Z.ltb_spec (imm_of w) imm_half) as [_ | _]; [reflexivity |].
  rewrite <- (Z.mod_add (imm_of w - imm_span + k) 1 imm_span) by span.
  f_equal. lia.
Qed.

Lemma disp_of_shift_instr : forall w k,
  in_reach (disp_of w + k) -> disp_of (shift_instr w k) = disp_of w + k.
Proof.
  intros w k H. unfold disp_of at 1, shift_instr.
  rewrite imm_of_put.
  rewrite imm_disp_congruent.
  apply sxt26_of_mod. exact H.
Qed.

Lemma opp_div4 : forall x, (4 | x) -> (- x) / 4 = - (x / 4).
Proof.
  intros x [k Hk]. subst x.
  rewrite Z.div_mul by span.
  replace (- (k * 4)) with ((- k) * 4) by span.
  rewrite Z.div_mul by span. reflexivity.
Qed.

Lemma sum_div4 : forall x y, (4 | x) -> (4 | y) -> (x + y) / 4 = x / 4 + y / 4.
Proof.
  intros x y [a Ha] [b Hb]. subst x y.
  replace (a * 4 + b * 4) with ((a + b) * 4) by span.
  rewrite !Z.div_mul by span. reflexivity.
Qed.

(* ------------------------------------------------------------------ *)
(* The image pass.                                                    *)
(*                                                                    *)
(* A site is a recorded flag beside a word.  `k` is the delta already  *)
(* in instruction units, which is x7 in the shipped loop.  A recorded  *)
(* site that does not hold a BL stops the walk: that site and every    *)
(* site after it keep the bytes they had, and the status is the        *)
(* CALLMAP-RC the shipped pass exits with.                             *)
(* ------------------------------------------------------------------ *)

Definition site : Type := (bool * Z)%type.

Fixpoint reloc_walk (s : list site) (k : Z) : list site * option Z :=
  match s with
  | [] => ([], None)
  | (rec, w) :: rest =>
      if andb rec (negb (is_bl w))
      then ((rec, w) :: rest, Some callmap_rc)
      else let p := reloc_walk rest k in
           ((rec, if rec then shift_instr w k else w) :: fst p, snd p)
  end.

Definition canon_pass (s : list site) (off : Z) : list site * option Z :=
  reloc_walk s (asr2 (wdelta off)).

Definition rebase_pass (s : list site) (off : Z) : list site * option Z :=
  reloc_walk s (asr2 (ldelta off)).

(* The same walk with the call guard removed, used only to exhibit what the
   guard buys.  It is never a description of the shipped pass. *)
Definition reloc_unguarded (s : list site) (k : Z) : list site :=
  map (fun p : site => (fst p, if fst p then shift_instr (snd p) k else snd p)) s.

(* ------------------------------------------------------------------ *)
(* Results.                                                           *)
(* ------------------------------------------------------------------ *)

(* A word built as a BL really is one, so the vector rows below are asking
   the pass about call sites and not about arbitrary integers. *)
Theorem bl_word_is_call : forall d, is_bl (bl d) = true.
Proof.
  intros d. unfold is_bl, op_of, bl.
  rewrite Z.div_add_l by span.
  rewrite Z.div_small by (apply Z.mod_pos_bound; span).
  rewrite Z.add_0_r. apply Z.eqb_refl.
Qed.

(* Relocating a call leaves a call.  The shipped pass writes the opcode field
   back unchanged (`9 9 26 LSRI, 9 9 26 LSLI, 9 9 3 ORR,`), so a relocated
   site still passes the very check that let it be relocated — which is what
   makes running the pass twice, once per direction, meaningful. *)
Theorem shift_call_stays_a_call : forall w k, is_bl (shift_instr w k) = is_bl w.
Proof.
  intros w k. unfold is_bl, shift_instr. rewrite op_of_put. reflexivity.
Qed.

(* The displacement read back out of a freshly built BL is the one put in,
   provided it fits.  This is what ties the vector rows, which are written in
   displacements, to the words the pass actually sees. *)
Theorem disp_of_bl : forall d, in_reach d -> disp_of (bl d) = d.
Proof.
  intros d H. unfold disp_of, imm_of, bl.
  rewrite Z.add_comm, Z.mod_add by span.
  rewrite Z.mod_mod by span.
  apply sxt26_of_mod. exact H.
Qed.

(* The pass adds its delta in instruction units and does nothing else. *)
Theorem shift_call_adds_instruction_units : forall w d,
  in_reach (disp_of w + d / 4) -> disp_of (shift_call w d) = disp_of w + d / 4.
Proof.
  intros w d H. unfold shift_call, asr2.
  apply disp_of_shift_instr. exact H.
Qed.

(* THE ROUND TRIP, at one site, when the restoring run happens to get the
   base the writing run had.  The image comes back byte-identical: the whole
   word, not merely its displacement, and with no reach hypothesis at all,
   because the two deltas cancel exactly.

   What the statement does need is that the region base is a multiple of four
   away from REGION-OFF.  It is: both are page-aligned.  Drop that and the
   two arithmetic shifts stop cancelling, which is
   `misaligned_base_breaks_round_trip` below. *)
Theorem call_round_trip_identity : forall w off,
  (4 | off - region_off) -> rebase (canon w off) off = w.
Proof.
  intros w off Hdiv.
  unfold rebase, canon, shift_call, asr2, wdelta, ldelta.
  rewrite shift_instr_compose.
  replace (region_off - off) with (- (off - region_off)) by span.
  rewrite opp_div4 by exact Hdiv.
  rewrite Z.add_opp_diag_r.
  apply shift_instr_0.
Qed.

(* THE ROUND TRIP, at one site, for an arbitrary pair of bases.  The image
   carries the canonical displacement; the restoring run adds back the
   difference between the canonical base and the one it actually got, and
   what it ends up with is exactly the displacement a site at that region
   offset must have in that run.

   `in_reach` on the answer is the hypothesis the boot assertion in
   habu2.f EM-MMAP-CODE-REGION exists to discharge: it dies BL-RANGE-RC when
   the region the kernel handed back is further than BL-REACH from __text. *)
Theorem call_round_trip_rebases : forall w wo lo,
  (4 | wo - region_off) -> (4 | lo - region_off) ->
  in_reach (disp_of w + (wo - lo) / 4) ->
  disp_of (rebase (canon w wo) lo) = disp_of w + (wo - lo) / 4.
Proof.
  intros w wo lo Hw Hl Hreach.
  unfold rebase, canon, shift_call, asr2, wdelta, ldelta.
  rewrite shift_instr_compose.
  replace ((wo - region_off) / 4 + (region_off - lo) / 4)
    with ((wo - lo) / 4).
  - apply disp_of_shift_instr. exact Hreach.
  - rewrite <- sum_div4 by (try exact Hw; apply Z.divide_opp_r in Hl;
      replace (region_off - lo) with (- (lo - region_off)) by span; exact Hl).
    f_equal. lia.
Qed.

(* Why an image is byte-identical across runs.  Whatever base a writing run
   got, canonicalizing the word that base produced yields the same word.
   This is the portability claim the snapshot format rests on, and it holds
   with no reach hypothesis: the canonical form is a function of the site and
   the callee alone. *)
Theorem canonical_form_is_base_independent : forall d off,
  (4 | off - region_off) -> canon (bl (d + (region_off - off) / 4)) off = bl d.
Proof.
  intros d off Hdiv.
  unfold canon, shift_call, asr2, wdelta, shift_instr, put_imm, bl.
  assert (Hop : op_of (bl_op_hi * imm_span + (d + (region_off - off) / 4) mod imm_span) = bl_op_hi).
  { unfold op_of. rewrite Z.div_add_l by span.
    rewrite Z.div_small by (apply Z.mod_pos_bound; span). lia. }
  assert (Himm : imm_of (bl_op_hi * imm_span + (d + (region_off - off) / 4) mod imm_span)
                 = (d + (region_off - off) / 4) mod imm_span).
  { unfold imm_of. rewrite Z.add_comm, Z.mod_add by span. apply Z.mod_mod. span. }
  rewrite Hop, Himm. f_equal.
  rewrite Z.add_mod_idemp_l by span.
  replace (region_off - off) with (- (off - region_off)) by span.
  rewrite opp_div4 by exact Hdiv.
  f_equal. lia.
Qed.

(* A site the map does not record is never visited, so a call that stays
   inside the region — which keeps its distance wherever the region lands —
   and any inline data word that happens to look like an instruction both
   come through untouched.  True even after a refusal, because a refusal
   stops the walk and leaves the rest of the image alone. *)
Theorem reloc_leaves_unrecorded : forall s k i w,
  nth_error s i = Some (false, w) ->
  nth_error (fst (reloc_walk s k)) i = Some (false, w).
Proof.
  intros s. induction s as [| [rec w0] rest IH]; intros k i w H.
  - destruct i; simpl in H; discriminate.
  - simpl. destruct (andb rec (negb (is_bl w0))) eqn:Ebad.
    + simpl. exact H.
    + destruct i as [| i']; simpl in *.
      * inversion H; subst. reflexivity.
      * apply IH. exact H.
Qed.

(* A recorded site that does not hold a call is REFUSED, not rewritten.  The
   site keeps its bytes and the walk reports the exit status the shipped pass
   dies with, so an image whose region bytes and call map come from different
   builds is stopped instead of having a wild branch written into it. *)
Theorem reloc_bad_site_not_rewritten : forall s k i w,
  nth_error s i = Some (true, w) ->
  is_bl w = false ->
  (forall j w', (j < i)%nat -> nth_error s j = Some (true, w') -> is_bl w' = true) ->
  nth_error (fst (reloc_walk s k)) i = Some (true, w)
  /\ snd (reloc_walk s k) = Some callmap_rc.
Proof.
  intros s. induction s as [| [rec w0] rest IH]; intros k i w H Hbl Hbefore.
  - destruct i; simpl in H; discriminate.
  - destruct i as [| i'].
    + simpl in H. inversion H; subst rec w0.
      simpl. rewrite Hbl. simpl. split; reflexivity.
    + simpl in H.
      assert (Ehead : andb rec (negb (is_bl w0)) = false).
      { destruct rec; [| reflexivity]. simpl.
        assert (Hcall : is_bl w0 = true).
        { apply (Hbefore 0%nat w0); [lia | reflexivity]. }
        rewrite Hcall. reflexivity. }
      assert (Hb' : forall j w', (j < i')%nat ->
                    nth_error rest j = Some (true, w') -> is_bl w' = true).
      { intros j w' Hj Hnth. apply (Hbefore (S j) w'); [lia | exact Hnth]. }
      specialize (IH k i' w H Hbl Hb'). destruct IH as [IH1 IH2].
      simpl. rewrite Ehead. simpl. split; [exact IH1 | exact IH2].
Qed.

(* And an image whose recorded sites all hold calls is never refused, so the
   guard is a refusal of corruption and not a refusal of ordinary work. *)
Theorem reloc_clean_never_refuses : forall s k,
  (forall w, In (true, w) s -> is_bl w = true) -> snd (reloc_walk s k) = None.
Proof.
  intros s. induction s as [| [rec w0] rest IH]; intros k Hclean.
  - reflexivity.
  - simpl. destruct rec eqn:Erec.
    + rewrite (Hclean w0) by (simpl; left; reflexivity). simpl.
      apply IH. intros w Hin. apply Hclean. simpl. right. exact Hin.
    + simpl. apply IH. intros w Hin. apply Hclean. simpl. right. exact Hin.
Qed.

(* THE HEADLINE.  Over a whole image, writer-side canonicalization composed
   with loader-side rebase is the identity, when the restoring run gets the
   writing run's base.  Every recorded site comes back to the word it had,
   every unrecorded site was never touched, and no site is lost or gained. *)
Theorem image_round_trip : forall s off,
  (4 | off - region_off) ->
  snd (canon_pass s off) = None ->
  fst (rebase_pass (fst (canon_pass s off)) off) = s.
Proof.
  intros s off Hdiv.
  unfold canon_pass, rebase_pass, wdelta, ldelta, asr2.
  set (a := (off - region_off) / 4).
  assert (Hopp : (region_off - off) / 4 = - a).
  { unfold a. replace (region_off - off) with (- (off - region_off)) by span.
    apply opp_div4. exact Hdiv. }
  rewrite Hopp. clear Hopp.
  induction s as [| [rec w0] rest IH]; intros Hclean.
  - reflexivity.
  - simpl in Hclean |- *.
    destruct rec eqn:Erec.
    + destruct (is_bl w0) eqn:Ebl; simpl in Hclean; [| discriminate].
      simpl. rewrite shift_call_stays_a_call, Ebl. simpl.
      rewrite shift_instr_compose, Z.add_opp_diag_r, shift_instr_0.
      rewrite (IH Hclean). reflexivity.
    + simpl in Hclean |- *. rewrite (IH Hclean). reflexivity.
Qed.

(* Every canonical displacement a region call can carry is inside BL's reach.
   A call leaves the region for __text, so its callee is below REGION-OFF and
   its site is at most the region's own size above it; both bounds come from
   the pinned layout constants. *)
Theorem canonical_disp_in_reach : forall c i,
  0 <= c < region_off -> 0 <= i -> 4 * i < region_bytes ->
  in_reach ((c - region_off) / 4 - i).
Proof.
  intros c i [Hc0 Hc1] Hi0 Hi1.
  unfold in_reach, imm_half, region_off, region_bytes in *.
  assert (Hlo : (-16777216) / 4 <= (c - 16777216) / 4)
    by (apply Z.div_le_mono; lia).
  assert (Hhi : (c - 16777216) / 4 <= (-1) / 4)
    by (apply Z.div_le_mono; lia).
  change ((-16777216) / 4) with (-4194304) in Hlo.
  change ((-1) / 4) with (-1) in Hhi.
  lia.
Qed.

(* The layout leaves room for it: a region placed at the canonical offset
   ends well inside BL's reach of __text.  Renumber any of the three pinned
   constants and this stops holding. *)
Theorem canonical_region_within_reach : region_off + region_bytes <= bl_reach.
Proof. unfold region_off, region_bytes, bl_reach. lia. Qed.

(* ------------------------------------------------------------------ *)
(* Declared address cells.                                            *)
(* ------------------------------------------------------------------ *)

Definition xt_canon (c dbase : Z) : Z :=
  if Z.eqb c 0 then 0 else c - dbase + rbase_va.

Definition xt_rebase (c dbase : Z) : Z :=
  if Z.eqb c 0 then 0 else c + (dbase - rbase_va).

(* A declared cell holds either nothing installed yet, or an address at or
   above the region base. *)
Definition xt_ok (c dbase : Z) : Prop := c = 0 \/ dbase <= c.

Theorem xt_round_trip : forall c dbase,
  0 < dbase -> xt_ok c dbase -> xt_rebase (xt_canon c dbase) dbase = c.
Proof.
  intros c dbase Hdb [Hz | Hge].
  - subst c. reflexivity.
  - unfold xt_canon, xt_rebase.
    destruct (Z.eqb_spec c 0) as [Hc | Hc]; [lia |].
    destruct (Z.eqb_spec (c - dbase + rbase_va) 0) as [Hs | Hs];
      unfold rbase_va in *; lia.
Qed.

(* A cleared hook survives a snapshot as a cleared hook.  Both halves skip
   zero rather than relocating it, which is the whole reason a hook that was
   never installed does not come back as a wild address. *)
Theorem xt_zero_preserved : forall dbase,
  xt_canon 0 dbase = 0 /\ xt_rebase 0 dbase = 0.
Proof. intros dbase. split; reflexivity. Qed.

(* And for an arbitrary pair of bases: the cell arrives holding the address
   this run's region gives it. *)
Theorem xt_rebase_general_base : forall c dbw dbl,
  0 < dbw -> dbw <= c -> xt_rebase (xt_canon c dbw) dbl = c - dbw + dbl.
Proof.
  intros c dbw dbl Hdb Hge.
  unfold xt_canon, xt_rebase.
  destruct (Z.eqb_spec c 0) as [Hc | Hc]; [lia |].
  destruct (Z.eqb_spec (c - dbw + rbase_va) 0) as [Hs | Hs];
    unfold rbase_va in *; lia.
Qed.

(* ------------------------------------------------------------------ *)
(* Completeness: every emitter that bakes an address into region bytes. *)
(*                                                                    *)
(* The round trip above says that every RECORDED site survives.  That  *)
(* is worth nothing on its own, because it is vacuously true of an     *)
(* address class nobody ever recorded — which is exactly how a literal *)
(* address baked into region code as a MOVZ/MOVK chain came to crash a *)
(* restored image.  So the emitter vocabulary is enumerated here and   *)
(* classified by a TOTAL function: `classify` is a match over an       *)
(* inductive type, so a producer added without an arm is a Rocq error  *)
(* rather than an omission, and the Habu half pins the vocabulary to   *)
(* what src/habu/habu2.f actually contains.                            *)
(* ------------------------------------------------------------------ *)

(* The emit vocabulary of src/habu/habu2.f that can put an address-bearing
   value into region bytes, or declare a persisted cell that holds one. *)
Inductive producer : Type :=
  | P_scalar_lit        (* C-LIT: minimal chain into x16, pushed *)
  | P_scalar_raw_lit    (* C-RAW-LIT: minimal chain into x16 *)
  | P_addr_carrier      (* C-ADDR-RAW: the fixed four-instruction x9 chain *)
  | P_addr_carrier_push (* C-ADDR-PUSH: that chain plus the push stencil *)
  | P_data_addr         (* C-DATA-ADDR: a DATA-region address, pushed *)
  | P_data_addr_raw     (* C-DATA-ADDR-RAW: a DATA-region address, no push *)
  | P_code_addr         (* C-CODE-ADDR: a JIT-region address *)
  | P_pc_relative_adr   (* C-ADR: a PC-relative ADR to inline region data *)
  | P_direct_call       (* EMIT-CEMITBL: one direct BL imm26 *)
  | P_aot_call_patch    (* EM-AOT-PATCH-SITES: rewrites BL imm26 sites *)
  | P_defer_cell        (* SNAP-RELOC:EMIT-MARK: declares a persisted cell *)
  .

(* The tables that name a site so a later pass can rewrite it. *)
Inductive recorder : Type :=
  | R_callmap          (* the call-site bitmap; replayed by EMIT-CALLS *)
  | R_xtcell           (* the address-cell table; replayed by EMIT-XT *)
  | R_aot_code_sites   (* the AOT capture-time code-literal site list *)
  .

Inductive klass : Type :=
  | Not_an_address        (* the bytes carry no address at all *)
  | Named_at_site         (* a shared carrier; its class is its caller's *)
  | Fixed_mapping         (* an address in a band mapped at a fixed VA *)
  | Position_independent  (* a distance, the same wherever the region lands *)
  | Recorded (r : recorder)
  .

Definition classify (p : producer) : klass :=
  match p with
  | P_scalar_lit => Not_an_address
  | P_scalar_raw_lit => Not_an_address
  | P_addr_carrier => Named_at_site
  | P_addr_carrier_push => Named_at_site
  | P_data_addr => Fixed_mapping
  | P_data_addr_raw => Fixed_mapping
  | P_code_addr => Recorded R_aot_code_sites
  | P_pc_relative_adr => Position_independent
  | P_direct_call => Recorded R_callmap
  | P_aot_call_patch => Recorded R_callmap
  | P_defer_cell => Recorded R_xtcell
  end.

(* Does the value this producer bakes change when the JIT region moves?  The
   two carriers answer false because nothing calls them but the three named
   wrappers, which is MODEL GAP 6 and is pinned on the Habu side. *)
Definition region_dependent (p : producer) : bool :=
  match p with
  | P_scalar_lit => false
  | P_scalar_raw_lit => false
  | P_addr_carrier => false
  | P_addr_carrier_push => false
  | P_data_addr => false
  | P_data_addr_raw => false
  | P_code_addr => true
  | P_pc_relative_adr => false
  | P_direct_call => true
  | P_aot_call_patch => true
  | P_defer_cell => true
  end.

(* Which recorders a snapshot RESTORE actually replays.  The AOT code-literal
   site list is not one of them: it lives in the seed blob and runs on the
   seed path only.  MODEL GAP 8. *)
Definition snapshot_replays (r : recorder) : bool :=
  match r with
  | R_callmap => true
  | R_xtcell => true
  | R_aot_code_sites => false
  end.

Definition snapshot_covers (p : producer) : bool :=
  match classify p with
  | Recorded r => snapshot_replays r
  | Not_an_address => true
  | Named_at_site => true
  | Fixed_mapping => true
  | Position_independent => true
  end.

(* Every producer whose bytes move with the region is named by some table.
   Nothing is left to be found by disassembly. *)
Theorem every_region_dependent_producer_is_recorded :
  forall p, region_dependent p = true -> exists r, classify p = Recorded r.
Proof.
  intros p H. destruct p; simpl in H; try discriminate;
    simpl; eexists; reflexivity.
Qed.

(* And a snapshot restore replays the table of every one of them but one. *)
Theorem snapshot_covers_every_producer_but_code_addr :
  forall p, p <> P_code_addr -> snapshot_covers p = true.
Proof.
  intros p Hne. destruct p; simpl; try reflexivity. congruence.
Qed.

(* The one that is left: a JIT-region address baked into region code as a
   MOVZ/MOVK chain is recorded only in the AOT capture-time site list, which a
   snapshot restore never replays.  A restored image therefore carries the
   WRITING run's address there.  This is a standing negative result, not a
   description of intended behaviour: when the recording and relocation for
   this class land, this theorem stops holding and must be replaced by the
   general form of the one above. *)
Theorem code_address_chain_is_the_open_gap :
  region_dependent P_code_addr = true /\ snapshot_covers P_code_addr = false.
Proof. split; reflexivity. Qed.

(* ------------------------------------------------------------------ *)
(* Negative results.                                                  *)
(*                                                                    *)
(* What the hypotheses above are buying, exhibited rather than argued. *)
(* ------------------------------------------------------------------ *)

(* Out of reach the field wraps, and the pass silently produces a
   displacement of the wrong sign.  This is the failure the boot assertion in
   EM-MMAP-CODE-REGION (exit BL-RANGE-RC) exists to make impossible, and it
   is why `call_round_trip_rebases` carries a reach hypothesis instead of
   holding unconditionally. *)
Theorem shift_out_of_reach_wraps :
  disp_of (shift_instr (bl (imm_half - 1)) 1) = - imm_half.
Proof. vm_compute. reflexivity. Qed.

(* A region base that is not a multiple of four away from the canonical
   offset breaks the round trip outright: the two arithmetic shifts round the
   same way, towards minus infinity, so they no longer cancel. *)
Theorem misaligned_base_breaks_round_trip :
  rebase (canon (bl 0) (region_off + 2)) (region_off + 2) <> bl 0.
Proof. vm_compute. discriminate. Qed.

(* What the call guard buys.  A recorded site holding a data word is refused
   with CALLMAP-RC and keeps its bytes; the same walk without the guard
   rewrites that data word, which is a corrupted image restored in silence. *)
Theorem dropping_the_call_guard_corrupts_data :
  reloc_walk [(true, 5)] 1 = ([(true, 5)], Some callmap_rc)
  /\ reloc_unguarded [(true, 5)] 1 = [(true, 6)].
Proof. split; vm_compute; reflexivity. Qed.

(* ------------------------------------------------------------------ *)
(* What every result above rests on.  Nothing: each reports closed under *)
(* the global context, and test/compiler/reloc-proof.f reads this list   *)
(* back and refuses an assumption twice over.                           *)
(* ------------------------------------------------------------------ *)

Print Assumptions bl_word_is_call.
Print Assumptions shift_call_stays_a_call.
Print Assumptions disp_of_bl.
Print Assumptions shift_call_adds_instruction_units.
Print Assumptions call_round_trip_identity.
Print Assumptions call_round_trip_rebases.
Print Assumptions canonical_form_is_base_independent.
Print Assumptions reloc_leaves_unrecorded.
Print Assumptions reloc_bad_site_not_rewritten.
Print Assumptions reloc_clean_never_refuses.
Print Assumptions image_round_trip.
Print Assumptions canonical_disp_in_reach.
Print Assumptions canonical_region_within_reach.
Print Assumptions xt_round_trip.
Print Assumptions xt_zero_preserved.
Print Assumptions xt_rebase_general_base.
Print Assumptions every_region_dependent_producer_is_recorded.
Print Assumptions snapshot_covers_every_producer_but_code_addr.
Print Assumptions code_address_chain_is_the_open_gap.
Print Assumptions shift_out_of_reach_wraps.
Print Assumptions misaligned_base_breaks_round_trip.
Print Assumptions dropping_the_call_guard_corrupts_data.
