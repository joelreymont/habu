(* Habu.Common.Reloc — a model of the snapshot relocation round trip.

   A snapshot image has to boot in a process that is not the one that wrote
   it.  The run that writes an image and the run that restores it get the
   engine's loaded __text base from the loader and the JIT region's base from
   the kernel, independently of one another.  Three classes of persisted value
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
     - every ADDRESS LITERAL the compiler bakes into region code as the fixed
       four-instruction MOVZ/MOVK chain: a quotation's entry address, and the
       target of a `[']` or a `postpone`.  The word such a chain names lives
       either in the JIT region or in the engine's loaded __text, and both
       move independently between the two runs.  Those sites too are RECORDED
       when the chain is created (the address-literal map in
       src/habu/layout.f, package SNAP-RELOC), never recognised afterwards:
       the sibling DATA literals share the chain's exact shape, so no decode
       of region bytes could tell the two apart.

   For all three classes the writer rewrites the value into a CANONICAL form
   and the loader rewrites it again for the run it is actually in.  This file
   states, and proves, the one invariant that all of them exist to keep:

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
     - src/habu/habu2.f SNAP-RELOC:EMIT-ADDRS is the address-literal pass,
       run once per BAND by the writer and once per band by the loader.  A
       band is the extent of an object that is moving — the JIT region, or
       the engine's loaded __text — given as a base, a length and the base
       those addresses are moving to; a chain whose value lies inside the
       band is rewritten to `value - band base + target base` and one that
       does not is left for the other band's call.  All four immediates are
       rewritten together, and a recorded site whose four words are not the
       chain is refused with exit status ADDRMAP-RC rather than rewritten.
       Modelled as `chain_value`, `chain_put`, `addr_move`, `addr_walk` and
       `addrmap_rc` below.
     - src/habu/layout.f pins the constants: REGION-OFF, RBASE-VA, BL-REACH,
       REGION, CALLMAP-RC, ADDRMAP-RC; src/habu/habu2.f pins BL-OP-HI,
       ADDR-OPC-MASK, ADDR-IMM-MASK and ADDR-CHAIN-BYTES; src/habu/habu1.f
       pins the chain's four scaffold words W-MOVZ0, W-MOVK1, W-MOVK2 and
       W-MOVK3.  Each is written down here as a literal and held equal to the
       shipped literal by the capacity rows of the parity gate,
       test/compiler/reloc-proof.f.

   The canonical forms, in words.  A call site's canonical displacement is
   the displacement it would have IF THE REGION SAT EXACTLY REGION-OFF ABOVE
   __TEXT, so an image is byte-identical whatever base the writing run got
   (`canonical_form_is_base_independent`).  A declared address cell is stored
   relative to the RBASE-VA sentinel for the same reason, and so is an
   address literal whose chain names the region: what the image carries is
   the offset within the band, not the address the writing run happened to
   get (`canonical_chain_is_base_independent`).

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
      64-bit overflow is out of scope for this model.  The address-literal
      pass compares a chain's value against its band with UNSIGNED compares
      (C-CC, C-CS, C-HI); the model compares the same quantities in Z, which
      agrees with the shipped compare exactly while every address stays below
      2^63.  `addr_ok` carries the 64-bit bound where it is needed.

   2. A field of an instruction word is modelled arithmetically rather than
      as a mask.  The 26-bit call immediate is `w mod 2^26` and its opcode is
      `w / 2^26`; the 16-bit immediate of a MOVZ or MOVK is `(w / 32) mod
      2^16` and everything else in the word is `scaffold_of`.  The shipped
      passes reach the same values with shifts and with the mask constant
      ADDR-OPC-MASK.  That the mask really is the complement of that field is
      not left to a reader: `opc_mask_is_the_field_complement` states the
      arithmetic relation between the shipped ADDR-OPC-MASK, ADDR-IMM-MASK
      and the modelled field position.  That the shipped INSTRUCTION SEQUENCE
      computes these is not assumed either: the Habu half of this gate
      DECODES AND RUNS that sequence, read out of src/habu/habu2.f, over the
      same vector rows this model is asked about.

   3. The recorded set is modelled as a boolean beside each word — beside
      each four-word chain, for the address-literal pass — and the pass as a
      walk over that list.  The shipped passes derive the same boolean from a
      bitmap indexed by region word offset, and stop at the image's payload
      length.  The bitmap indexing itself, and the four-word stride the
      address-literal pass reads at a recorded bit, are not modelled here;
      they are likewise covered by the Habu half, which builds a real map
      band and runs the shipped instruction sequence over it.

   4. A refusal is modelled as the walk stopping and reporting CALLMAP-RC,
      or ADDRMAP-RC for the address-literal pass.  The shipped passes write a
      diagnostic to file descriptor two and call exit-group with that status.
      Sites already rewritten before the bad one stay rewritten in both,
      which is why `reloc_walk` and `addr_walk` return the partially
      rewritten list beside the status.

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

   8. The classification records what the SNAPSHOT loader replays.  A
      capture-time site list that only the AOT seed path walks does not make
      a producer snapshot-safe, which is why `R_aot_code_sites` is a recorder
      the restore does NOT replay.  `EM-AOT-RELOC-CODE` is the second place
      that writes a code-address chain into region bytes, and it is covered
      because it records each chain it rewrites in the same address-literal
      map the compiler writes — not because the seed walks its own list.
      `EM-AOT-RELOC-DATA` writes chains that name DATA, which is mapped at a
      fixed address in every run and therefore needs no table to be CORRECT
      after a restore.  The compiler's own DATA producers are recorded anyway,
      and the note above `classify` says why: a table answers "where is the
      chain", which the AOT capture needs, and not only "does it move".

   9. The band a chain is measured against is a parameter here, exactly as it
      is in the shipped pass.  That the two bands the engine passes — the JIT
      region and the loaded __text — are disjoint at both write and restore
      time is a fact about the layout, not about this arithmetic; what the
      model states is the consequence the design rests on, that a chain
      inside one band is left untouched by the other band's call
      (`each_chain_moves_under_exactly_one_band`).
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

(* $A00000, src/habu/layout.f: the JIT region's size - the dictionary band and
   the code band together, which is what CODE-BAND:BYTES splits. *)
Definition region_bytes : Z := 10485760.

(* $300000000, src/habu/layout.f: the canonical region base a declared
   address cell is stored relative to.  A portability sentinel, never a
   runtime address. *)
Definition rbase_va : Z := 12884901888.

(* $8000000, src/habu/layout.f: BL's +/- 128 MiB reach. *)
Definition bl_reach : Z := 134217728.

(* 95, src/habu/layout.f: the exit status for a recorded site that does not
   hold a call. *)
Definition callmap_rc : Z := 95.

(* 97, src/habu/layout.f: the exit status for a recorded address-literal site
   that does not hold the four-instruction chain. *)
Definition addrmap_rc : Z := 97.

(* The four scaffold words of the chain habu2.f C-ADDR-RAW emits, from
   src/habu/habu1.f: $D2800009, $F2A00009, $F2C00009, $F2E00009.  Each pins
   the destination register x9 and the shift as well as the opcode, and each
   carries a zero immediate. *)
Definition w_movz0 : Z := 3531603977.
Definition w_movk1 : Z := 4070572041.
Definition w_movk2 : Z := 4072669193.
Definition w_movk3 : Z := 4074766345.

(* $1F and 5, src/habu/habu2.f: the destination-register field of a move-wide
   word, and its width.  x9 is the register the ONE carrier C-ADDR-RAW writes
   into, and it is the register all four scaffolds above name; a chain the
   native compiler emits names whichever register its allocator chose. *)
Definition addr_rd_mask : Z := 31.
Definition addr_rd_bits : Z := 5.
Definition rd_span : Z := 32.
Definition addr_rd : Z := 9.

(* $FFE0001F, $FFFF and 16, src/habu/habu2.f: an instruction word minus its
   16-bit immediate, that immediate once shifted down by five, and the whole
   chain's length in bytes. *)
Definition addr_opc_mask : Z := 4292870175.
Definition addr_imm_mask : Z := 65535.
Definition addr_chain_bytes : Z := 16.

(* The immediate of a MOVZ or MOVK sits at bit five and is sixteen bits wide,
   so a chain of four spells out a 64-bit address in four steps. *)
Definition imm_scale : Z := 32.
Definition imm16_span : Z := 65536.
Definition imm32_span : Z := 4294967296.
Definition imm48_span : Z := 281474976710656.
Definition word32_span : Z := 4294967296.
Definition addr_span : Z := 18446744073709551616.

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
(* Address literals: the four-instruction MOVZ/MOVK chain.            *)
(*                                                                    *)
(* A quotation's entry address, and the target of a `[']` or a         *)
(* `postpone`, are compiled into region code as four instructions that *)
(* spell out a 64-bit address sixteen bits at a time.  The word the    *)
(* chain names lives either in the JIT region or in the engine's       *)
(* loaded __text, and both land somewhere else in the run that         *)
(* restores an image, so the pass is given the extent of the object    *)
(* that is moving — a BAND — and is called once per band.              *)
(* ------------------------------------------------------------------ *)

(* One 16-bit immediate, at bit five of a MOVZ or MOVK word, and everything
   else in that word.  The shipped pass reads the field with `>> 5` and a mask
   against ADDR-IMM-MASK and keeps the rest with a mask against
   ADDR-OPC-MASK; MODEL GAP 2 names the difference and
   `opc_mask_is_the_field_complement` below states the relation. *)
Definition imm16_of (w : Z) : Z := (w / imm_scale) mod imm16_span.
Definition scaffold_of (w : Z) : Z := w - imm16_of w * imm_scale.
Definition put_imm16 (w v : Z) : Z := scaffold_of w + (v mod imm16_span) * imm_scale.

Lemma imm16_bounds : forall w, 0 <= imm16_of w < imm16_span.
Proof.
  intros w. unfold imm16_of. apply Z.mod_pos_bound. unfold imm16_span; lia.
Qed.

Lemma imm16_of_put : forall w v, imm16_of (put_imm16 w v) = v mod imm16_span.
Proof.
  intros w v. unfold imm16_of, put_imm16, scaffold_of, imm16_of.
  replace (w - (w / imm_scale) mod imm16_span * imm_scale
             + v mod imm16_span * imm_scale)
    with (w + (v mod imm16_span - (w / imm_scale) mod imm16_span) * imm_scale)
    by lia.
  rewrite Z.div_add by (unfold imm_scale; lia).
  rewrite (Z.mod_eq (w / imm_scale) imm16_span) by (unfold imm16_span; lia).
  replace (w / imm_scale
           + (v mod imm16_span
              - (w / imm_scale - imm16_span * (w / imm_scale / imm16_span))))
    with (v mod imm16_span + (w / imm_scale / imm16_span) * imm16_span) by lia.
  rewrite Z.mod_add by (unfold imm16_span; lia).
  apply Z.mod_mod. unfold imm16_span; lia.
Qed.

Lemma put_imm16_eq : forall w v,
  v mod imm16_span = imm16_of w -> put_imm16 w v = w.
Proof.
  intros w v H. unfold put_imm16, scaffold_of. rewrite H. lia.
Qed.

Lemma scaffold_of_put : forall w v, scaffold_of (put_imm16 w v) = scaffold_of w.
Proof.
  intros w v. unfold scaffold_of at 1. rewrite imm16_of_put.
  unfold put_imm16. lia.
Qed.

Lemma put_imm16_compose : forall w a b,
  put_imm16 (put_imm16 w a) b = put_imm16 w b.
Proof.
  intros w a b. unfold put_imm16 at 1. rewrite scaffold_of_put. reflexivity.
Qed.

Lemma add_mul_split : forall a b, 0 <= a < 65536 ->
  (a + b * 65536) / 65536 = b /\ (a + b * 65536) mod 65536 = a.
Proof.
  intros a b Ha. split.
  - rewrite Z.div_add by lia. rewrite Z.div_small by lia. lia.
  - rewrite Z.mod_add by lia. apply Z.mod_small. lia.
Qed.

(* The four words at one site, the address they spell out, and the same four
   words carrying a different address.  The shipped pass ORs the four shifted
   fields together and the model adds them; the fields are disjoint, so the
   two agree (MODEL GAP 2). *)
Definition chain : Type := (Z * Z * Z * Z)%type.

Definition chain_value (c : chain) : Z :=
  let '(w0, w1, w2, w3) := c in
  imm16_of w0 + imm16_of w1 * imm16_span
  + imm16_of w2 * imm32_span + imm16_of w3 * imm48_span.

Definition chain_put (c : chain) (v : Z) : chain :=
  let '(w0, w1, w2, w3) := c in
  (put_imm16 w0 v, put_imm16 w1 (v / imm16_span),
   put_imm16 w2 (v / imm32_span), put_imm16 w3 (v / imm48_span)).

(* The destination register a move-wide word names, and the same word with that
   register replaced.  `rd_of` is the shipped `9 9 ADDR-RD-MASK ANDI,` and
   `with_rd` is the shipped `LSRI, LSLI, ORR,` triple that clears a scaffold's
   register and puts the site's own back. *)
Definition rd_of (w : Z) : Z := w mod rd_span.
Definition with_rd (w rd : Z) : Z := w - rd_of w + rd.

(* A chain as the compiler first emits it, and the same chain with one word
   damaged.  These are what the vector rows are written in, so neither the
   Habu side nor the generated obligations carries a word of its own.
   `scaffolds_rd` is the four scaffolds naming a register other than x9, which
   is what the native compiler's allocator produces and what the widened guard
   exists to accept. *)
Definition scaffolds : chain := (w_movz0, w_movk1, w_movk2, w_movk3).
Definition mk_chain (v : Z) : chain := chain_put scaffolds v.

Definition scaffolds_rd (rd : Z) : chain :=
  (with_rd w_movz0 rd, with_rd w_movk1 rd,
   with_rd w_movk2 rd, with_rd w_movk3 rd).
Definition mk_chain_rd (rd v : Z) : chain := chain_put (scaffolds_rd rd) v.

Definition break_chain (v k bad : Z) : chain :=
  let '(w0, w1, w2, w3) := mk_chain v in
  if Z.eqb k 0 then (bad, w1, w2, w3)
  else if Z.eqb k 1 then (w0, bad, w2, w3)
  else if Z.eqb k 2 then (w0, w1, bad, w3)
  else (w0, w1, w2, bad).

(* Four move-wide words that are each the right instruction but do NOT agree on
   one destination register: lane `odd` names rd+1 and the other three name rd.
   No compiler emits this and no relocation could repair it, so the guard has to
   refuse it.  Written the way `break_chain` is, so a row names a case rather
   than carrying instruction words. *)
Definition mix_chain (rd odd v : Z) : chain :=
  let '(w0, w1, w2, w3) := mk_chain_rd rd v in
  if Z.eqb odd 0 then (with_rd w0 (rd + 1), w1, w2, w3)
  else if Z.eqb odd 1 then (w0, with_rd w1 (rd + 1), w2, w3)
  else if Z.eqb odd 2 then (w0, w1, with_rd w2 (rd + 1), w3)
  else (w0, w1, w2, with_rd w3 (rd + 1)).

(* The check the shipped pass makes before it writes: all four words must still
   be the chain's own instructions, and all four must name ONE register — the
   one word 0 names.  That register is read off the site rather than pinned to
   x9, because the carrier in habu2.f is not the only emitter of the chain any
   more; the agreement requirement is what keeps the widening from degenerating
   into "any four move-wide words at all". *)
Definition is_chain (c : chain) : bool :=
  let '(w0, w1, w2, w3) := c in
  let rd := rd_of w0 in
  Z.eqb (scaffold_of w0) (with_rd w_movz0 rd)
  && Z.eqb (scaffold_of w1) (with_rd w_movk1 rd)
  && Z.eqb (scaffold_of w2) (with_rd w_movk2 rd)
  && Z.eqb (scaffold_of w3) (with_rd w_movk3 rd).

(* The widening this design REJECTED: mask the register out of the comparison
   altogether.  Kept only to exhibit what the agreement requirement buys. *)
Definition is_chain_rd_blind (c : chain) : bool :=
  let '(w0, w1, w2, w3) := c in
  Z.eqb (with_rd (scaffold_of w0) 0) (with_rd w_movz0 0)
  && Z.eqb (with_rd (scaffold_of w1) 0) (with_rd w_movk1 0)
  && Z.eqb (with_rd (scaffold_of w2) 0) (with_rd w_movk2 0)
  && Z.eqb (with_rd (scaffold_of w3) 0) (with_rd w_movk3 0).

(* An address a chain can carry at all: sixty-four unsigned bits. *)
Definition addr_ok (v : Z) : Prop := 0 <= v < addr_span.

(* The band test, and the rewrite.  x21/x22/x25 in the shipped pass. *)
Definition in_band (v base len : Z) : Prop := base <= v < base + len.
Definition band_hit (v base len : Z) : bool := (base <=? v) && (v <? base + len).

Definition addr_move (c : chain) (base len tgt : Z) : chain :=
  if band_hit (chain_value c) base len
  then chain_put c (chain_value c - base + tgt)
  else c.

Lemma band_hit_true : forall v base len,
  in_band v base len -> band_hit v base len = true.
Proof.
  intros v base len [H1 H2]. unfold band_hit.
  apply andb_true_intro. split.
  - apply Z.leb_le. exact H1.
  - apply Z.ltb_lt. exact H2.
Qed.

Lemma band_hit_false : forall v base len,
  ~ in_band v base len -> band_hit v base len = false.
Proof.
  intros v base len H. unfold band_hit.
  destruct (Z.leb_spec base v); [| reflexivity].
  destruct (Z.ltb_spec v (base + len)); [| reflexivity].
  exfalso. apply H. split; assumption.
Qed.

(* ---- the destination-register field ---------------------------------- *)

Lemma rd_span_pos : 0 < rd_span.
Proof. unfold rd_span; lia. Qed.

Lemma rd_bounds : forall w, 0 <= rd_of w < rd_span.
Proof. intros w. unfold rd_of. apply Z.mod_pos_bound. apply rd_span_pos. Qed.

Lemma rd_split : forall w, w - rd_of w = rd_span * (w / rd_span).
Proof.
  intros w. unfold rd_of. rewrite (Z.mod_eq w rd_span) by (unfold rd_span; lia).
  lia.
Qed.

Lemma rd_of_with_rd : forall w rd,
  0 <= rd < rd_span -> rd_of (with_rd w rd) = rd.
Proof.
  intros w rd H. unfold rd_of at 1, with_rd.
  replace (w - rd_of w + rd) with (rd + (w / rd_span) * rd_span)
    by (pose proof (rd_split w); unfold rd_span in *; lia).
  rewrite Z.mod_add by (unfold rd_span; lia).
  apply Z.mod_small. exact H.
Qed.

Lemma with_rd_inj : forall w a b, with_rd w a = with_rd w b -> a = b.
Proof. intros w a b H. unfold with_rd in H. lia. Qed.

(* Replacing the register does not disturb the immediate: the register field
   sits entirely below the immediate's bit five, so no carry crosses. *)
Lemma imm16_of_with_rd : forall w rd,
  0 <= rd < rd_span -> imm16_of (with_rd w rd) = imm16_of w.
Proof.
  intros w rd H. unfold imm16_of, imm_scale, with_rd.
  replace (w - rd_of w + rd) with (rd + (w / rd_span) * rd_span)
    by (pose proof (rd_split w); unfold rd_span in *; lia).
  unfold rd_span in *.
  rewrite Z.div_add by lia.
  rewrite (Z.div_small rd 32) by lia.
  reflexivity.
Qed.

Lemma rd_of_scaffold : forall w, rd_of (scaffold_of w) = rd_of w.
Proof.
  intros w. unfold rd_of, scaffold_of, imm_scale, rd_span.
  replace (w - imm16_of w * 32) with (w + (- imm16_of w) * 32) by lia.
  rewrite Z.mod_add by lia. reflexivity.
Qed.

Lemma scaffold_of_with_rd : forall w rd,
  0 <= rd < rd_span -> scaffold_of (with_rd w rd) = with_rd (scaffold_of w) rd.
Proof.
  intros w rd H. unfold scaffold_of at 1. rewrite imm16_of_with_rd by exact H.
  unfold with_rd. rewrite rd_of_scaffold. unfold scaffold_of, imm_scale. lia.
Qed.

Lemma rd_of_put : forall w v, rd_of (put_imm16 w v) = rd_of w.
Proof.
  intros w v. unfold rd_of, put_imm16, scaffold_of, imm_scale, rd_span.
  replace (w - imm16_of w * 32 + v mod imm16_span * 32)
    with (w + (v mod imm16_span - imm16_of w) * 32) by lia.
  rewrite Z.mod_add by lia. reflexivity.
Qed.

Lemma with_rd_id : forall w, with_rd w (rd_of w) = w.
Proof. intros w. unfold with_rd. lia. Qed.

(* Reduce the guard's own pattern match and nothing else.  A bare `cbn` here
   unfolds Z division and diverges, so every proof about `is_chain` names the
   constants it may reduce. *)
Ltac open_chain := cbv beta iota zeta delta [is_chain is_chain_rd_blind].

Lemma chain_stays_a_chain : forall c v, is_chain (chain_put c v) = is_chain c.
Proof.
  intros [[[w0 w1] w2] w3] v. open_chain. unfold chain_put.
  rewrite !scaffold_of_put, !rd_of_put. reflexivity.
Qed.

Lemma chain_put_compose : forall c a b,
  chain_put (chain_put c a) b = chain_put c b.
Proof.
  intros [[[w0 w1] w2] w3] a b. cbn. rewrite !put_imm16_compose. reflexivity.
Qed.

(* ---- results about one chain ---------------------------------------- *)

(* The shipped mask really is the complement of the modelled field, and the
   chain really is four words long.  This is what buys MODEL GAP 2 for the
   address pass: renumber ADDR-OPC-MASK, ADDR-IMM-MASK or ADDR-CHAIN-BYTES on
   either side and the two sides stop describing the same instruction. *)
Theorem opc_mask_is_the_field_complement :
  addr_opc_mask = word32_span - 1 - addr_imm_mask * imm_scale
  /\ addr_imm_mask + 1 = imm16_span
  /\ addr_chain_bytes = 4 * 4.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* And the shipped register mask really is the field `rd_of` reads: ADDR-RD-MASK
   is every bit below the immediate's bit five, ADDR-RD-BITS is its width, and
   the register the shipped scaffolds name is inside it.  Renumber either on
   either side and the two stop describing the same field — which matters here
   because the shipped pass clears a scaffold's register by shifting down by
   ADDR-RD-BITS and back, and that is the same operation only while the two agree.
   The mask sits directly below the immediate, so replacing a register can never
   disturb one (`imm16_of_with_rd`). *)
Theorem rd_mask_is_the_register_field :
  addr_rd_mask = rd_span - 1
  /\ 2 ^ addr_rd_bits = rd_span
  /\ addr_opc_mask mod imm_scale = addr_rd_mask
  /\ 0 <= addr_rd < rd_span.
Proof.
  repeat split; try (vm_compute; reflexivity);
    unfold addr_rd, rd_span; lia.
Qed.

(* A chain built the way the compiler builds one really is one, so the vector
   rows below are asking the pass about chains and not about arbitrary words. *)
Theorem mk_chain_is_a_chain : forall v, is_chain (mk_chain v) = true.
Proof.
  intros v. unfold mk_chain. rewrite chain_stays_a_chain.
  vm_compute. reflexivity.
Qed.

(* All four shipped scaffolds name ONE register, and it is x9.  That is what
   makes the four-lane agreement requirement below a generalisation of the old
   x9 comparison rather than a different check: renumber any one of the four
   literals to name another register and this stops holding. *)
Theorem the_shipped_scaffolds_name_one_register :
  rd_of w_movz0 = addr_rd /\ rd_of w_movk1 = addr_rd
  /\ rd_of w_movk2 = addr_rd /\ rd_of w_movk3 = addr_rd.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* THE WIDENING.  The chain the native compiler emits names whichever register
   its allocator chose, and the guard accepts it — for every register, and for
   every address.  Before this, only x9 passed, so a DATA address literal
   recorded in the address map would have been refused as a corrupt image. *)
Theorem chain_in_any_register_is_a_chain : forall rd v,
  0 <= rd < rd_span -> is_chain (mk_chain_rd rd v) = true.
Proof.
  intros rd v H. unfold mk_chain_rd. rewrite chain_stays_a_chain.
  unfold scaffolds_rd. open_chain.
  rewrite (rd_of_with_rd w_movz0 rd H).
  rewrite !scaffold_of_with_rd by exact H.
  assert (S0 : scaffold_of w_movz0 = w_movz0) by (vm_compute; reflexivity).
  assert (S1 : scaffold_of w_movk1 = w_movk1) by (vm_compute; reflexivity).
  assert (S2 : scaffold_of w_movk2 = w_movk2) by (vm_compute; reflexivity).
  assert (S3 : scaffold_of w_movk3 = w_movk3) by (vm_compute; reflexivity).
  rewrite S0, S1, S2, S3, !Z.eqb_refl. reflexivity.
Qed.

(* THE STRENGTH.  Accepting any register would be worthless if the four lanes
   could name four different ones: the pass writes one address across all four
   immediates, so lanes that disagree are not a chain in any register, and the
   value they spell out belongs to no site.  A chain the guard accepts names one
   register in all four lanes — stated over ARBITRARY words, so it is a property
   of the guard and not of the way the vector rows happen to build a chain. *)
Theorem a_chain_names_one_register : forall w0 w1 w2 w3,
  is_chain (w0, w1, w2, w3) = true ->
  rd_of w1 = rd_of w0 /\ rd_of w2 = rd_of w0 /\ rd_of w3 = rd_of w0.
Proof.
  intros w0 w1 w2 w3 H.
  cbv beta iota zeta delta [is_chain] in H.
  apply andb_prop in H as [H H3]. apply andb_prop in H as [H H2].
  apply andb_prop in H as [_ H1].
  apply Z.eqb_eq in H1. apply Z.eqb_eq in H2. apply Z.eqb_eq in H3.
  pose proof (rd_bounds w0) as Hb.
  repeat split.
  - rewrite <- (rd_of_scaffold w1), H1. apply rd_of_with_rd. exact Hb.
  - rewrite <- (rd_of_scaffold w2), H2. apply rd_of_with_rd. exact Hb.
  - rewrite <- (rd_of_scaffold w3), H3. apply rd_of_with_rd. exact Hb.
Qed.

(* The address read back out of a rewritten chain is the one written in.  All
   four immediates carry their sixteen bits and nothing is lost between them:
   this is the statement the falsification of the shipped pass attacks, and
   `skipping_the_last_movk_loses_the_top_field` below is what its failure
   looks like. *)
Theorem chain_value_of_put : forall c v,
  addr_ok v -> chain_value (chain_put c v) = v.
Proof.
  intros [[[w0 w1] w2] w3] v [Hlo Hhi]. cbn. rewrite !imm16_of_put.
  unfold imm16_span, imm32_span, imm48_span, addr_span in *.
  assert (Hd1 : v / 65536 / 65536 = v / 4294967296)
    by (rewrite Z.div_div by lia; reflexivity).
  assert (Hd2 : v / 4294967296 / 65536 = v / 281474976710656)
    by (rewrite Z.div_div by lia; reflexivity).
  assert (Hq3lo : 0 <= v / 281474976710656) by (apply Z.div_pos; lia).
  assert (Hq3hi : v / 281474976710656 < 65536)
    by (apply Z.div_lt_upper_bound; lia).
  pose proof (Z.div_mod v 65536 ltac:(lia)) as A.
  pose proof (Z.div_mod (v / 65536) 65536 ltac:(lia)) as B.
  pose proof (Z.div_mod (v / 4294967296) 65536 ltac:(lia)) as C.
  rewrite Hd1 in B. rewrite Hd2 in C.
  rewrite (Z.mod_small (v / 281474976710656) 65536) by lia.
  lia.
Qed.

(* Writing back the address a chain already carries leaves the four words
   exactly as they were.  That is what makes the shipped pass's two arms
   agree: a chain outside the band is branched over, and one whose value did
   not move would come back byte-identical anyway. *)
Theorem chain_put_value : forall c, chain_put c (chain_value c) = c.
Proof.
  intros [[[w0 w1] w2] w3]. cbn [chain_put chain_value].
  pose proof (imm16_bounds w0) as B0. pose proof (imm16_bounds w1) as B1.
  pose proof (imm16_bounds w2) as B2. pose proof (imm16_bounds w3) as B3.
  unfold imm16_span, imm32_span, imm48_span in *.
  set (i0 := imm16_of w0) in *. set (i1 := imm16_of w1) in *.
  set (i2 := imm16_of w2) in *. set (i3 := imm16_of w3) in *.
  set (v := i0 + i1 * 65536 + i2 * 4294967296 + i3 * 281474976710656).
  assert (Hv1 : v = i0 + (i1 + i2 * 65536 + i3 * 4294967296) * 65536)
    by (unfold v; lia).
  assert (Hv2 : i1 + i2 * 65536 + i3 * 4294967296
                = i1 + (i2 + i3 * 65536) * 65536) by lia.
  destruct (add_mul_split i0 (i1 + i2 * 65536 + i3 * 4294967296) B0)
    as [Dv0 Mv0].
  destruct (add_mul_split i1 (i2 + i3 * 65536) B1) as [Dv1 Mv1].
  destruct (add_mul_split i2 i3 B2) as [Dv2 Mv2].
  assert (M0 : v mod 65536 = i0) by (rewrite Hv1; exact Mv0).
  assert (Q1 : v / 65536 = i1 + i2 * 65536 + i3 * 4294967296)
    by (rewrite Hv1; exact Dv0).
  assert (Q2 : v / 4294967296 = i2 + i3 * 65536).
  { replace 4294967296 with (65536 * 65536) by lia.
    rewrite <- Z.div_div by lia. rewrite Q1, Hv2. exact Dv1. }
  assert (Q3 : v / 281474976710656 = i3).
  { replace 281474976710656 with (4294967296 * 65536) by lia.
    rewrite <- Z.div_div by lia. rewrite Q2. exact Dv2. }
  assert (M1 : (v / 65536) mod 65536 = i1) by (rewrite Q1, Hv2; exact Mv1).
  assert (M2 : (v / 4294967296) mod 65536 = i2) by (rewrite Q2; exact Mv2).
  assert (M3 : (v / 281474976710656) mod 65536 = i3)
    by (rewrite Q3; apply Z.mod_small; lia).
  rewrite (put_imm16_eq w0 v M0), (put_imm16_eq w1 _ M1),
          (put_imm16_eq w2 _ M2), (put_imm16_eq w3 _ M3).
  reflexivity.
Qed.

(* Relocating a chain leaves a chain.  The shipped pass writes each scaffold
   back untouched, so a relocated site still passes the very check that let it
   be relocated — which is what makes running the pass twice, once per
   direction, meaningful. *)
Theorem addr_move_stays_a_chain : forall c base len tgt,
  is_chain (addr_move c base len tgt) = is_chain c.
Proof.
  intros c base len tgt. unfold addr_move.
  destruct (band_hit (chain_value c) base len).
  - apply chain_stays_a_chain.
  - reflexivity.
Qed.

(* A chain whose address lies outside the band being moved is left alone.
   This is the whole reason a `[']` naming a PRIMITIVE — whose code is in the
   engine's loaded __text, not in the region — survives the region band's
   call and is rewritten by the text band's. *)
Theorem addr_out_of_band_untouched : forall c base len tgt,
  ~ in_band (chain_value c) base len -> addr_move c base len tgt = c.
Proof.
  intros c base len tgt H. unfold addr_move.
  rewrite band_hit_false by exact H. reflexivity.
Qed.

(* Why an image is byte-identical across runs.  Whatever base the writing run
   got, what the canonical image carries is the OFFSET within the band and the
   canonical base — the writing run's base appears only in the hypothesis. *)
Theorem canonical_chain_is_base_independent : forall c wb len cb k,
  0 <= k < len -> chain_value c = wb + k -> addr_ok (cb + k) ->
  chain_value (addr_move c wb len cb) = cb + k.
Proof.
  intros c wb len cb k Hk Hv Hok. unfold addr_move.
  rewrite band_hit_true by (unfold in_band; lia).
  replace (chain_value c - wb + cb) with (cb + k) by lia.
  apply chain_value_of_put. exact Hok.
Qed.

(* THE ROUND TRIP, at one chain, when the restoring run gets the base the
   writing run had.  All four words come back exactly as they were. *)
Theorem addr_round_trip_identity : forall c wb len cb,
  in_band (chain_value c) wb len ->
  addr_ok (chain_value c - wb + cb) ->
  addr_move (addr_move c wb len cb) cb len wb = c.
Proof.
  intros c wb len cb Hband Hok.
  assert (Hstep : addr_move c wb len cb = chain_put c (chain_value c - wb + cb))
    by (unfold addr_move; rewrite band_hit_true by exact Hband; reflexivity).
  assert (Hval : chain_value (chain_put c (chain_value c - wb + cb))
                 = chain_value c - wb + cb)
    by (apply chain_value_of_put; exact Hok).
  rewrite Hstep. unfold addr_move. rewrite Hval.
  rewrite band_hit_true by (unfold in_band in *; lia).
  replace (chain_value c - wb + cb - cb + wb) with (chain_value c) by lia.
  rewrite chain_put_compose. apply chain_put_value.
Qed.

(* THE ROUND TRIP, at one chain, for an arbitrary pair of bases.  The image
   carries the canonical address; the restoring run gets back the same word at
   the base its own band landed on. *)
Theorem addr_round_trip_rebases : forall c wb len cb lb,
  in_band (chain_value c) wb len ->
  addr_ok (chain_value c - wb + cb) ->
  addr_ok (chain_value c - wb + lb) ->
  chain_value (addr_move (addr_move c wb len cb) cb len lb)
    = chain_value c - wb + lb.
Proof.
  intros c wb len cb lb Hband Hokc Hokl.
  assert (Hstep : addr_move c wb len cb = chain_put c (chain_value c - wb + cb))
    by (unfold addr_move; rewrite band_hit_true by exact Hband; reflexivity).
  assert (Hval : chain_value (chain_put c (chain_value c - wb + cb))
                 = chain_value c - wb + cb)
    by (apply chain_value_of_put; exact Hokc).
  rewrite Hstep. unfold addr_move. rewrite Hval.
  rewrite band_hit_true by (unfold in_band in *; lia).
  rewrite chain_put_compose.
  replace (chain_value c - wb + cb - cb + lb)
    with (chain_value c - wb + lb) by lia.
  apply chain_value_of_put. exact Hokl.
Qed.

(* What the per-band parameterization buys.  The engine calls the pass once
   for each band that is moving; a chain inside one of them is moved by that
   call and left untouched by the other, so no chain is relocated twice and
   none is missed.  MODEL GAP 9. *)
Theorem each_chain_moves_under_exactly_one_band :
  forall c a1 l1 t1 a2 l2 t2,
  in_band (chain_value c) a1 l1 ->
  (a1 + l1 <= a2 \/ a2 + l2 <= a1) ->
  addr_move c a2 l2 t2 = c
  /\ addr_move c a1 l1 t1 = chain_put c (chain_value c - a1 + t1).
Proof.
  intros c a1 l1 t1 a2 l2 t2 Hband Hdis. split.
  - apply addr_out_of_band_untouched. unfold in_band in *. lia.
  - unfold addr_move. rewrite band_hit_true by exact Hband. reflexivity.
Qed.

(* ---- the image pass over a whole address-literal map ---------------- *)

Definition asite : Type := (bool * chain)%type.

Fixpoint addr_walk (s : list asite) (base len tgt : Z)
  : list asite * option Z :=
  match s with
  | [] => ([], None)
  | (rec, c) :: rest =>
      if andb rec (negb (is_chain c))
      then ((rec, c) :: rest, Some addrmap_rc)
      else let p := addr_walk rest base len tgt in
           ((rec, if rec then addr_move c base len tgt else c) :: fst p, snd p)
  end.

(* The same walk with the chain guard removed, used only to exhibit what the
   guard buys.  It is never a description of the shipped pass. *)
Definition addr_unguarded (s : list asite) (base len tgt : Z) : list asite :=
  map (fun p : asite =>
         (fst p, if fst p then addr_move (snd p) base len tgt else snd p)) s.

(* A site the map does not record is never visited, so the sibling DATA
   literals — which share the chain's exact shape and are deliberately not
   recorded, because DATA is mapped at a fixed address in every run — come
   through untouched. *)
Theorem addr_walk_leaves_unrecorded : forall s base len tgt i c,
  nth_error s i = Some (false, c) ->
  nth_error (fst (addr_walk s base len tgt)) i = Some (false, c).
Proof.
  intros s. induction s as [| [rec c0] rest IH]; intros base len tgt i c H.
  - destruct i; simpl in H; discriminate.
  - simpl. destruct (andb rec (negb (is_chain c0))) eqn:Ebad.
    + simpl. exact H.
    + destruct i as [| i']; simpl in *.
      * inversion H; subst. reflexivity.
      * apply IH. exact H.
Qed.

(* A recorded site that does not hold the chain is REFUSED, not rewritten.
   The site keeps its bytes and the walk reports the exit status the shipped
   pass dies with, so an image whose region bytes and address map come from
   different builds is stopped instead of having four wild immediates written
   into it. *)
Theorem addr_walk_bad_site_not_rewritten : forall s base len tgt i c,
  nth_error s i = Some (true, c) ->
  is_chain c = false ->
  (forall j c', (j < i)%nat -> nth_error s j = Some (true, c') ->
                is_chain c' = true) ->
  nth_error (fst (addr_walk s base len tgt)) i = Some (true, c)
  /\ snd (addr_walk s base len tgt) = Some addrmap_rc.
Proof.
  intros s. induction s as [| [rec c0] rest IH];
    intros base len tgt i c H Hch Hbefore.
  - destruct i; simpl in H; discriminate.
  - destruct i as [| i'].
    + simpl in H. inversion H; subst rec c0.
      simpl. rewrite Hch. simpl. split; reflexivity.
    + simpl in H.
      assert (Ehead : andb rec (negb (is_chain c0)) = false).
      { destruct rec; [| reflexivity]. simpl.
        assert (Hgood : is_chain c0 = true).
        { apply (Hbefore 0%nat c0); [lia | reflexivity]. }
        rewrite Hgood. reflexivity. }
      assert (Hb' : forall j c', (j < i')%nat ->
                    nth_error rest j = Some (true, c') -> is_chain c' = true).
      { intros j c' Hj Hnth. apply (Hbefore (S j) c'); [lia | exact Hnth]. }
      specialize (IH base len tgt i' c H Hch Hb'). destruct IH as [IH1 IH2].
      simpl. rewrite Ehead. simpl. split; [exact IH1 | exact IH2].
Qed.

(* And an image whose recorded sites all hold chains is never refused, so the
   guard is a refusal of corruption and not a refusal of ordinary work. *)
Theorem addr_walk_clean_never_refuses : forall s base len tgt,
  (forall c, In (true, c) s -> is_chain c = true) ->
  snd (addr_walk s base len tgt) = None.
Proof.
  intros s. induction s as [| [rec c0] rest IH]; intros base len tgt Hclean.
  - reflexivity.
  - simpl. destruct rec eqn:Erec.
    + rewrite (Hclean c0) by (simpl; left; reflexivity). simpl.
      apply IH. intros c Hin. apply Hclean. simpl. right. exact Hin.
    + simpl. apply IH. intros c Hin. apply Hclean. simpl. right. exact Hin.
Qed.

(* THE HEADLINE for this class.  Over a whole image, writer-side
   canonicalization composed with loader-side rebase is the identity when the
   restoring run's band lands where the writing run's did.  Every recorded
   chain comes back to the four words it had, every unrecorded site was never
   touched, and no site is lost or gained. *)
Theorem addr_image_round_trip : forall s wb len cb,
  (forall c, In (true, c) s ->
     in_band (chain_value c) wb len /\ addr_ok (chain_value c - wb + cb)) ->
  snd (addr_walk s wb len cb) = None ->
  fst (addr_walk (fst (addr_walk s wb len cb)) cb len wb) = s.
Proof.
  intros s. induction s as [| [rec c0] rest IH]; intros wb len cb Hin Hclean.
  - reflexivity.
  - simpl in Hclean |- *.
    destruct rec eqn:Erec.
    + destruct (is_chain c0) eqn:Ech; simpl in Hclean; [| discriminate].
      simpl. rewrite addr_move_stays_a_chain, Ech. simpl.
      destruct (Hin c0 ltac:(simpl; left; reflexivity)) as [Hband Hok].
      rewrite addr_round_trip_identity by assumption.
      rewrite IH;
        [reflexivity
        | intros c Hc; apply Hin; simpl; right; exact Hc
        | exact Hclean].
    + simpl. rewrite IH;
        [reflexivity
        | intros c Hc; apply Hin; simpl; right; exact Hc
        | exact Hclean].
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
  | P_aot_code_reloc    (* EM-AOT-RELOC-CODE: rebases seeded code chains *)
  | P_aot_xt_patch      (* AOT-XTSITE:PATCH-CHAINS: writes a NAMED word's entry
                           into a seeded code chain.  Its answer comes from
                           LFIND in the booting engine rather than from a
                           delta, which is what a chain naming a word OUTSIDE
                           the captured window needs; the value it bakes is
                           still a region address, so it is region_dependent
                           and it marks the address map exactly as
                           EM-AOT-RELOC-CODE does. *)
  | P_aot_data_reloc    (* EM-AOT-RELOC-DATA: rebases seeded DATA chains *)
  | P_defer_cell        (* SNAP-RELOC:EMIT-MARK: declares a persisted cell *)
  .

(* The tables that name a site so a later pass can rewrite it. *)
Inductive recorder : Type :=
  | R_callmap          (* the call-site bitmap; replayed by EMIT-CALLS *)
  | R_xtcell           (* the address-cell table; replayed by EMIT-XT *)
  | R_addrmap          (* the address-literal bitmap; replayed by EMIT-ADDRS *)
  | R_aot_code_sites   (* the AOT capture-time code-literal site list *)
  .

Inductive klass : Type :=
  | Not_an_address        (* the bytes carry no address at all *)
  | Named_at_site         (* a shared carrier; its class is its caller's *)
  | Fixed_mapping         (* an address in a band mapped at a fixed VA *)
  | Position_independent  (* a distance, the same wherever the region lands *)
  | Recorded (r : recorder)
  .

(* WHY A DATA ADDRESS IS `Recorded` AND NOT `Fixed_mapping`, THOUGH IT DOES NOT
   MOVE.  The two questions this file asks about a producer are independent, and
   the compiler's DATA-address chains are the pair that separates them:

     classify         — is this site NAMED IN A TABLE, so a later pass can find
                        it without decoding region bytes?
     region_dependent — does the value it bakes CHANGE when the region moves?

   A DATA address answers yes to the first and no to the second.  It is recorded
   because a consumer has to FIND it: the AOT capture copies a blob to a
   different DP in the seeded engine, so a chain-compiled word's DATA address is
   the metabuild host's and is wrong there, and the alternative to a record is
   recognising one by the value it carries — which is a guess, because an
   ordinary integer may hold any value at all.  It is not region-dependent
   because DATA is mapped at a fixed VA in every run.

   `Fixed_mapping` therefore stops meaning "not in a table" and keeps meaning
   what it says: an address in a band mapped at a fixed VA.  Being recorded costs
   the snapshot pass nothing, because that pass is parameterised by band and
   rewrites a chain only when its value is inside the band it was handed
   (`addr_out_of_band_untouched`); a DATA address is inside neither band the
   engine passes, so a recorded DATA site is visited by both calls and rewritten
   by neither.  `region_dependent` below is what still says so, and
   `every_region_dependent_producer_is_recorded` is the implication that matters
   — it does not run the other way, and this pair is the witness. *)
Definition classify (p : producer) : klass :=
  match p with
  | P_scalar_lit => Not_an_address
  | P_scalar_raw_lit => Not_an_address
  | P_addr_carrier => Named_at_site
  | P_addr_carrier_push => Named_at_site
  | P_data_addr => Recorded R_addrmap
  | P_data_addr_raw => Recorded R_addrmap
  | P_code_addr => Recorded R_addrmap
  | P_pc_relative_adr => Position_independent
  | P_direct_call => Recorded R_callmap
  | P_aot_call_patch => Recorded R_callmap
  | P_aot_code_reloc => Recorded R_addrmap
  | P_aot_xt_patch => Recorded R_addrmap
  | P_aot_data_reloc => Fixed_mapping
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
  | P_aot_code_reloc => true
  | P_aot_xt_patch => true
  | P_aot_data_reloc => false
  | P_defer_cell => true
  end.

(* Which recorders a snapshot RESTORE actually replays.  The AOT code-literal
   site list is not one of them: it lives in the seed blob and runs on the
   seed path only, so a producer named THERE and nowhere else would still
   carry the writing run's address into a restored image.  That is the trap
   this campaign fell into, and it is kept here rather than deleted, because
   it is what gives `snapshot_covers_every_producer` below its content.
   MODEL GAP 8. *)
Definition snapshot_replays (r : recorder) : bool :=
  match r with
  | R_callmap => true
  | R_xtcell => true
  | R_addrmap => true
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

(* And a snapshot restore replays the table of EVERY one of them.  There is no
   longer an address class that a restored image carries in the writing run's
   terms.  The last one to be closed was the JIT-region address baked into
   region code as a MOVZ/MOVK chain: it used to be named only by the AOT
   capture-time site list, which a restore never walks, and a restored image
   therefore executed the writing run's address and died.  It is now recorded
   in the address-literal map at the emit site, by C-CODE-ADDR from the
   compiler and by EM-AOT-RELOC-CODE from the seed, and rewritten per band by
   EMIT-ADDRS — the pass modelled above.

   This is the general form the earlier `code_address_chain_is_the_open_gap`
   promised.  It is not vacuous: classify any producer to a recorder the
   restore does not replay and it stops holding. *)
Theorem snapshot_covers_every_producer :
  forall p, snapshot_covers p = true.
Proof. intros p. destruct p; reflexivity. Qed.

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

(* What writing ALL FOUR immediates buys.  A pass that rewrote only three of
   them would leave the top sixteen bits of the old address in place, and the
   chain would push an address that is neither the one it had nor the one it
   should have.  `chain_put3` is that mutant, exhibited on the smallest value
   that separates the two: 2^48, which lives entirely in the fourth
   immediate.  This is the model's copy of the falsification the shipped pass
   was put through. *)
Definition chain_put3 (c : chain) (v : Z) : chain :=
  let '(w0, w1, w2, w3) := c in
  (put_imm16 w0 v, put_imm16 w1 (v / imm16_span),
   put_imm16 w2 (v / imm32_span), w3).

Theorem skipping_the_last_movk_loses_the_top_field :
  chain_value (chain_put3 (mk_chain 0) imm48_span) = 0
  /\ chain_value (chain_put (mk_chain 0) imm48_span) = imm48_span.
Proof. split; vm_compute; reflexivity. Qed.

(* What the four-lane agreement buys.  The other way to widen the guard was to
   mask the destination register out of the comparison; that reads every lane's
   opcode and shift and nothing else, so it accepts four words that name four
   different registers.  Such a site is not a chain any compiler emits, its four
   immediates do not spell out one register's address, and rewriting them writes
   a rebased value into code that never pushed one.  The shipped guard refuses
   it. *)
Theorem a_register_blind_guard_admits_mismatched_lanes :
  is_chain (mix_chain addr_rd 1 0) = false
  /\ is_chain_rd_blind (mix_chain addr_rd 1 0) = true.
Proof. split; vm_compute; reflexivity. Qed.

(* And it is not one lucky lane: whichever of the four disagrees, the guard
   refuses.  `mix_chain` moves the odd lane, and every choice is refused for
   every register the allocator can pick and every address. *)
Theorem any_mismatched_lane_is_refused : forall rd odd v,
  0 <= rd < rd_span - 1 -> 0 <= odd < 4 ->
  is_chain (mix_chain rd odd v) = false.
Proof.
  intros rd odd v Hrd Hodd.
  assert (Hr : 0 <= rd < rd_span) by (unfold rd_span in *; lia).
  assert (Hr1 : 0 <= rd + 1 < rd_span) by (unfold rd_span in *; lia).
  (* what register each lane of the mixed chain names *)
  assert (A0 : rd_of (put_imm16 (with_rd w_movz0 rd) v) = rd)
    by (rewrite rd_of_put; apply rd_of_with_rd; exact Hr).
  assert (A1 : rd_of (put_imm16 (with_rd w_movk1 rd) (v / imm16_span)) = rd)
    by (rewrite rd_of_put; apply rd_of_with_rd; exact Hr).
  assert (A2 : rd_of (put_imm16 (with_rd w_movk2 rd) (v / imm32_span)) = rd)
    by (rewrite rd_of_put; apply rd_of_with_rd; exact Hr).
  assert (A3 : rd_of (put_imm16 (with_rd w_movk3 rd) (v / imm48_span)) = rd)
    by (rewrite rd_of_put; apply rd_of_with_rd; exact Hr).
  assert (B0 : rd_of (with_rd (put_imm16 (with_rd w_movz0 rd) v) (rd + 1))
               = rd + 1) by (apply rd_of_with_rd; exact Hr1).
  assert (B1 : rd_of (with_rd (put_imm16 (with_rd w_movk1 rd) (v / imm16_span))
                        (rd + 1)) = rd + 1) by (apply rd_of_with_rd; exact Hr1).
  assert (B2 : rd_of (with_rd (put_imm16 (with_rd w_movk2 rd) (v / imm32_span))
                        (rd + 1)) = rd + 1) by (apply rd_of_with_rd; exact Hr1).
  assert (B3 : rd_of (with_rd (put_imm16 (with_rd w_movk3 rd) (v / imm48_span))
                        (rd + 1)) = rd + 1) by (apply rd_of_with_rd; exact Hr1).
  destruct (is_chain (mix_chain rd odd v)) eqn:E; [exfalso | reflexivity].
  assert (Hodd4 : odd = 0 \/ odd = 1 \/ odd = 2 \/ odd = 3) by lia.
  (* the guard accepted it, so by a_chain_names_one_register all four lanes name
     word 0's register — and the odd lane names one more than the rest. *)
  destruct Hodd4 as [Eo | [Eo | [Eo | Eo]]]; subst odd;
    unfold mix_chain, mk_chain_rd, scaffolds_rd, chain_put in E;
    cbv beta iota zeta delta [Z.eqb] in E;
    apply a_chain_names_one_register in E as [E1 [E2 E3]];
    rewrite ?A0, ?A1, ?A2, ?A3, ?B0, ?B1, ?B2, ?B3 in E1, E2, E3;
    lia.
Qed.

(* What the chain guard buys.  A recorded site holding a data word is refused
   with ADDRMAP-RC and keeps its four words; the same walk without the guard
   reads a value out of those words, decides it lies inside the band, and
   writes a rebased one back — a corrupted image restored in silence. *)
Theorem dropping_the_chain_guard_corrupts_data :
  addr_walk [(true, (5, 0, 0, 0))] 0 1 7
    = ([(true, (5, 0, 0, 0))], Some addrmap_rc)
  /\ addr_unguarded [(true, (5, 0, 0, 0))] 0 1 7 = [(true, (229, 0, 0, 0))].
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
Print Assumptions opc_mask_is_the_field_complement.
Print Assumptions rd_mask_is_the_register_field.
Print Assumptions mk_chain_is_a_chain.
Print Assumptions the_shipped_scaffolds_name_one_register.
Print Assumptions chain_in_any_register_is_a_chain.
Print Assumptions a_chain_names_one_register.
Print Assumptions chain_value_of_put.
Print Assumptions chain_put_value.
Print Assumptions addr_move_stays_a_chain.
Print Assumptions addr_out_of_band_untouched.
Print Assumptions canonical_chain_is_base_independent.
Print Assumptions addr_round_trip_identity.
Print Assumptions addr_round_trip_rebases.
Print Assumptions each_chain_moves_under_exactly_one_band.
Print Assumptions addr_walk_leaves_unrecorded.
Print Assumptions addr_walk_bad_site_not_rewritten.
Print Assumptions addr_walk_clean_never_refuses.
Print Assumptions addr_image_round_trip.
Print Assumptions every_region_dependent_producer_is_recorded.
Print Assumptions snapshot_covers_every_producer.
Print Assumptions shift_out_of_reach_wraps.
Print Assumptions misaligned_base_breaks_round_trip.
Print Assumptions dropping_the_call_guard_corrupts_data.
Print Assumptions skipping_the_last_movk_loses_the_top_field.
Print Assumptions a_register_blind_guard_admits_mismatched_lanes.
Print Assumptions any_mismatched_lane_is_refused.
Print Assumptions dropping_the_chain_guard_corrupts_data.
