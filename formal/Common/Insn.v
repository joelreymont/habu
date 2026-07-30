(* Habu.Common.Insn - what the engine's ARM64 assembler emits.

   src/arch/arm64/asm.f packs operands into 32-bit words and
   src/arch/arm64/{icode,mnem}.f wrap those encoders in the mnemonic and
   label vocabulary the engine builder writes (src/habu/habu1.f,
   src/habu/habu2.f).  Nothing in the tree says what a correct packing is, so
   a misplaced field shifts silently until the emitted code crashes.

   This file is that statement.  It carries one inductive of the emitted
   forms with their operands as the shipped mnemonics take them, an encode
   function to a 32-bit word, a decoder for the same subset, and the results
   below.  test/compiler/insn-proof.f binds it to the shipped assembler: one
   shared table of (form, operands, expected word) rows in
   test/compiler/insn-schema.f drives both a Habu test that calls the real
   emitter words and a generated Rocq obligation about this model.

   Published results:

     decode_encode                 - decoding an encoded well-formed
                                     instruction gives that instruction back.
     enc_injective                 - two different well-formed instructions
                                     never encode to the same word.
     decoder_rows_exclusive        - no 32-bit word satisfies two decoder
                                     rows, so the decoder does not depend on
                                     the order its rows are written in.
     opcode_and_operands_tile_the_word - for every form the opcode bits and
                                     the operand fields cover the whole word
                                     and never overlap, so no bit is left
                                     unexamined and no field can move without
                                     the opcode mask moving too.
     mask_never_truncates          - the shipped `MSK` ($FFFFFFFF and) is the
                                     identity on every well-formed encoding,
                                     so it never silently drops a bit.
     unguarded_x_register_forms    - LDAR, STLR, CBZ and CBNZ are exactly the
                                     modelled forms with an X-register
                                     operand the shipped encoder does not
                                     pass through its reserved-register
                                     check.  A FINDING, not a design: the
                                     header of src/arch/arm64/asm.f says the
                                     check runs on every X-register operand
                                     field.
     overflow_aliases_another_instruction    - FINDING.  An out-of-range
                                     16-bit move immediate, or an
                                     out-of-range condition code, is not
                                     refused: it runs into the neighbouring
                                     field and encodes a DIFFERENT
                                     well-formed instruction.
     truncating_scale_aliases_another_offset - FINDING.  The mnemonics that
                                     divide a byte operand by its scale
                                     (MOVK by 16, LDR by 8) round down
                                     without checking, so a misaligned
                                     operand encodes a different, aligned
                                     one.
     overflow_escapes_the_vocabulary         - FINDING.  An out-of-range
                                     12-bit immediate or register number
                                     leaves the modelled vocabulary
                                     altogether: the emitted word is not the
                                     encoding of any form here.
     lsli_lsri_alias_at_zero       - a shift-left by zero and a shift-right
                                     by zero are the same word, which is why
                                     a left shift of zero is not well formed
                                     here.

   Operand conventions follow the shipped words, not the architecture
   manual, because the shipped words are what this has to constrain.  So a
   load carries a BYTE offset (the encoder divides by the access size), MOVK
   carries a BYTE shift (the encoder divides by 16) while the MOVZ/MOVN
   encoders carry the raw two-bit hw field, and a branch carries the
   instruction-relative delta in WORDS that src/arch/arm64/icode.f computes
   from the label.

   MODEL GAPS.  The floating-point encoders (FMOVXD, FMOVDX, FMOVDD, FADD,
   FSUB, FMUL, FDIV, FNEG, FABS, FSQRT, FCMP, FCMP0, SCVTF, FCVTZS) are not
   modelled here; neither is `>LIMM`, the logical-immediate mask synthesis,
   whose packed result the Andi/Orri/Eori forms take as an operand.  The
   out-of-reach refusals for B/BL (?REL26) cannot be reached through the
   shipped 2 MB code window at all.  Each is recorded as a dot. *)

From Stdlib Require Import ZArith Bool List Lia Btauto.
Import ListNotations.
Local Open Scope Z_scope.

(* ---- bit fields ---------------------------------------------------------

   A field is a value shifted to a position.  `msk p w` is the mask of the
   w-bit field at position p, `fld v p` puts v there, and `get p w x` reads it
   back.  Everything below is built from these three, so a field position is
   written once in `enc` and once in the decoder row, and the lemmas here are
   what make the two agree. *)

Definition msk (p w : Z) : Z := Z.shiftl (Z.ones w) p.
Definition fld (v p : Z) : Z := Z.shiftl v p.
Definition get (p w x : Z) : Z := Z.shiftr (Z.land x (msk p w)) p.

(* The low n bits of a possibly negative delta, and the sign extension that
   reads it back - what a two's-complement branch field stores. *)
Definition low (n x : Z) : Z := Z.land x (Z.ones n).
Definition sext (n x : Z) : Z := if x <? 2 ^ (n - 1) then x else x - 2 ^ n.

Lemma land_lor : forall x y m, Z.land (Z.lor x y) m = Z.lor (Z.land x m) (Z.land y m).
Proof. intros. apply Z.land_lor_distr_l. Qed.

Lemma land_lxor : forall a b m,
  Z.land (Z.lxor a b) m = Z.lxor (Z.land a m) (Z.land b m).
Proof.
  intros. apply Z.bits_inj'. intros n Hn.
  rewrite !Z.land_spec, !Z.lxor_spec, !Z.land_spec. btauto.
Qed.

Lemma get_lor : forall p w x y, get p w (Z.lor x y) = Z.lor (get p w x) (get p w y).
Proof. intros. unfold get. rewrite Z.land_lor_distr_l, Z.shiftr_lor. reflexivity. Qed.

Lemma fld_sub : forall v q k, 0 <= q -> 0 <= v < 2 ^ k -> 0 <= k ->
  Z.land (fld v q) (msk q k) = fld v q.
Proof.
  intros v q k Hq Hv Hk. unfold fld, msk. rewrite <- Z.shiftl_land. f_equal.
  rewrite Z.land_ones by lia. apply Z.mod_small. lia.
Qed.

Lemma fld_land_disjoint : forall v q k m, 0 <= q -> 0 <= v < 2 ^ k -> 0 <= k ->
  Z.land (msk q k) m = 0 -> Z.land (fld v q) m = 0.
Proof.
  intros v q k m Hq Hv Hk H. rewrite <- (fld_sub v q k) by assumption.
  rewrite <- Z.land_assoc, H. apply Z.land_0_r.
Qed.

Lemma fld_land_super : forall v q k m, 0 <= q -> 0 <= v < 2 ^ k -> 0 <= k ->
  Z.land (msk q k) m = msk q k -> Z.land (fld v q) m = fld v q.
Proof.
  intros v q k m Hq Hv Hk H.
  rewrite <- (fld_sub v q k) at 1 by assumption.
  rewrite <- Z.land_assoc, H. apply (fld_sub v q k); assumption.
Qed.

Lemma get_fld_same : forall v p w, 0 <= p -> 0 <= v < 2 ^ w -> 0 <= w ->
  get p w (fld v p) = v.
Proof.
  intros v p w Hp Hv Hw. unfold get. rewrite (fld_sub v p w) by assumption.
  unfold fld. rewrite Z.shiftr_shiftl_l by lia. rewrite Z.sub_diag.
  apply Z.shiftl_0_r.
Qed.

Lemma get_fld_zero : forall v p w q k, 0 <= p -> 0 <= q -> 0 <= v < 2 ^ k ->
  0 <= k -> Z.land (msk q k) (msk p w) = 0 -> get p w (fld v q) = 0.
Proof.
  intros. unfold get. rewrite (fld_land_disjoint v q k (msk p w)) by assumption.
  apply Z.shiftr_0_l.
Qed.

Lemma low_bnd : forall n x, 0 <= n -> 0 <= low n x < 2 ^ n.
Proof.
  intros n x Hn. unfold low. rewrite Z.land_ones by lia. apply Z.mod_pos_bound.
  apply Z.pow_pos_nonneg; lia.
Qed.

Lemma lxor_bnd : forall a b n, 0 <= n -> 0 <= a < 2 ^ n -> 0 <= b < 2 ^ n ->
  0 <= Z.lxor a b < 2 ^ n.
Proof.
  intros a b n Hn Ha Hb.
  assert (Hx : 0 <= Z.lxor a b) by (apply Z.lxor_nonneg; lia).
  split; [lia|].
  assert (E : Z.land (Z.lxor a b) (Z.ones n) = Z.lxor a b).
  { rewrite land_lxor, !Z.land_ones by lia. rewrite !Z.mod_small by lia.
    reflexivity. }
  rewrite Z.land_ones in E by lia. rewrite <- E. apply Z.mod_pos_bound.
  apply Z.pow_pos_nonneg; lia.
Qed.

Lemma mod4_bnd : forall x, 0 <= x mod 4 < 2 ^ 2.
Proof. intros. change (2 ^ 2) with 4. apply Z.mod_pos_bound. lia. Qed.

Lemma mod64_bnd : forall x, 0 <= x mod 64 < 2 ^ 6.
Proof. intros. change (2 ^ 6) with 64. apply Z.mod_pos_bound. lia. Qed.

(* Reading a two's-complement field back is the identity on the deltas that
   fit it.  This is what makes a backward branch survive the round trip. *)
Lemma sext_low : forall n d, 0 < n -> - (2 ^ (n - 1)) <= d < 2 ^ (n - 1) ->
  sext n (low n d) = d.
Proof.
  intros n d Hn Hd. unfold sext, low. rewrite Z.land_ones by lia.
  assert (Hq : 0 < 2 ^ (n - 1)) by (apply Z.pow_pos_nonneg; lia).
  assert (Hp : 2 ^ n = 2 * 2 ^ (n - 1)).
  { replace n with ((n - 1) + 1) at 1 by lia. rewrite Z.pow_add_r by lia. lia. }
  destruct (Z_lt_le_dec d 0) as [Hneg | Hpos].
  - assert (Hm : d mod 2 ^ n = d + 2 ^ n).
    { rewrite <- (Z.mod_add d 1 (2 ^ n)) by lia. rewrite Z.mul_1_l.
      apply Z.mod_small. lia. }
    rewrite Hm.
    destruct (d + 2 ^ n <? 2 ^ (n - 1)) eqn:E; [ apply Z.ltb_lt in E; lia | lia ].
  - rewrite Z.mod_small by lia.
    destruct (d <? 2 ^ (n - 1)) eqn:E; [ reflexivity | apply Z.ltb_ge in E; lia ].
Qed.

Lemma div4_bnd : forall bd, - 1048576 <= bd < 1048576 ->
  - (2 ^ 18) <= bd / 4 < 2 ^ 18.
Proof.
  intros bd H. change (2 ^ 18) with 262144. split.
  - apply Z.div_le_lower_bound; lia.
  - apply Z.div_lt_upper_bound; lia.
Qed.

Lemma mul_div_exact : forall a b, 0 < b -> a mod b = 0 -> b * (a / b) = a.
Proof. intros a b Hb H. symmetry. apply Z.div_exact; lia. Qed.

(* ---- the emitted vocabulary --------------------------------------------- *)

Inductive insn : Type :=
| Movz (rd imm hw : Z)
| Movn (rd imm hw : Z)
| Movk (rd imm sh : Z)
| Add (rd rn rm : Z)
| Sub (rd rn rm : Z)
| And (rd rn rm : Z)
| Orr (rd rn rm : Z)
| Eor (rd rn rm : Z)
| Mul (rd rn rm : Z)
| Sdiv (rd rn rm : Z)
| Udiv (rd rn rm : Z)
| Lslv (rd rn rm : Z)
| Lsrv (rd rn rm : Z)
| Addi (rd rn imm : Z)
| Subi (rd rn imm : Z)
| Andi (rd rn nis : Z)
| Orri (rd rn nis : Z)
| Eori (rd rn nis : Z)
| Lsli (rd rn sh : Z)
| Lsri (rd rn sh : Z)
| Asri (rd rn sh : Z)
| Ldr (rt rn off : Z)
| Str (rt rn off : Z)
| Ldrb (rt rn off : Z)
| Strb (rt rn off : Z)
| Ldrw (rt rn off : Z)
| Strw (rt rn off : Z)
| Ldar (rt rn : Z)
| Stlr (rt rn : Z)
| Cmp (rn rm : Z)
| Cmpi (rn imm : Z)
| Cset (rd cond : Z)
| B (d : Z)
| Bl (d : Z)
| Bcond (cond d : Z)
| Cbz (rt d : Z)
| Cbnz (rt d : Z)
| Adr (rd bd : Z)
| Svc (imm : Z)
| Ret
| Brk
| Nop
| DsbIsh
| Isb
| Blr (rn : Z)
| Br (rn : Z)
| IcIvau (rt : Z)
| DcCvau (rt : Z).

Definition enc (i : insn) : Z :=
  match i with
  | Movz rd imm hw => Z.lor 0xD2800000 (Z.lor (fld (rd) 0) (Z.lor (fld (imm) 5) (fld (hw) 21)))
  | Movn rd imm hw => Z.lor 0x92800000 (Z.lor (fld (rd) 0) (Z.lor (fld (imm) 5) (fld (hw) 21)))
  | Movk rd imm sh => Z.lor 0xF2800000 (Z.lor (fld (rd) 0) (Z.lor (fld (imm) 5) (fld (sh / 16) 21)))
  | Add rd rn rm => Z.lor 0x8B000000 (Z.lor (fld (rd) 0) (Z.lor (fld (rn) 5) (fld (rm) 16)))
  | Sub rd rn rm => Z.lor 0xCB000000 (Z.lor (fld (rd) 0) (Z.lor (fld (rn) 5) (fld (rm) 16)))
  | And rd rn rm => Z.lor 0x8A000000 (Z.lor (fld (rd) 0) (Z.lor (fld (rn) 5) (fld (rm) 16)))
  | Orr rd rn rm => Z.lor 0xAA000000 (Z.lor (fld (rd) 0) (Z.lor (fld (rn) 5) (fld (rm) 16)))
  | Eor rd rn rm => Z.lor 0xCA000000 (Z.lor (fld (rd) 0) (Z.lor (fld (rn) 5) (fld (rm) 16)))
  | Mul rd rn rm => Z.lor 0x9B007C00 (Z.lor (fld (rd) 0) (Z.lor (fld (rn) 5) (fld (rm) 16)))
  | Sdiv rd rn rm => Z.lor 0x9AC00C00 (Z.lor (fld (rd) 0) (Z.lor (fld (rn) 5) (fld (rm) 16)))
  | Udiv rd rn rm => Z.lor 0x9AC00800 (Z.lor (fld (rd) 0) (Z.lor (fld (rn) 5) (fld (rm) 16)))
  | Lslv rd rn rm => Z.lor 0x9AC02000 (Z.lor (fld (rd) 0) (Z.lor (fld (rn) 5) (fld (rm) 16)))
  | Lsrv rd rn rm => Z.lor 0x9AC02400 (Z.lor (fld (rd) 0) (Z.lor (fld (rn) 5) (fld (rm) 16)))
  | Addi rd rn imm => Z.lor 0x91000000 (Z.lor (fld (rd) 0) (Z.lor (fld (rn) 5) (fld (imm) 10)))
  | Subi rd rn imm => Z.lor 0xD1000000 (Z.lor (fld (rd) 0) (Z.lor (fld (rn) 5) (fld (imm) 10)))
  | Andi rd rn nis => Z.lor 0x92000000 (Z.lor (fld (rd) 0) (Z.lor (fld (rn) 5) (fld (nis) 10)))
  | Orri rd rn nis => Z.lor 0xB2000000 (Z.lor (fld (rd) 0) (Z.lor (fld (rn) 5) (fld (nis) 10)))
  | Eori rd rn nis => Z.lor 0xD2000000 (Z.lor (fld (rd) 0) (Z.lor (fld (rn) 5) (fld (nis) 10)))
  | Lsli rd rn sh => Z.lor 0xD3400000 (Z.lor (fld (rd) 0) (Z.lor (fld (rn) 5) (Z.lor (fld ((64 - sh) mod 64) 16) (fld (63 - sh) 10))))
  | Lsri rd rn sh => Z.lor 0xD340FC00 (Z.lor (fld (rd) 0) (Z.lor (fld (rn) 5) (fld (sh) 16)))
  | Asri rd rn sh => Z.lor 0x9340FC00 (Z.lor (fld (rd) 0) (Z.lor (fld (rn) 5) (fld (sh) 16)))
  | Ldr rt rn off => Z.lor 0xF9400000 (Z.lor (fld (rt) 0) (Z.lor (fld (rn) 5) (fld (off / 8) 10)))
  | Str rt rn off => Z.lor 0xF9000000 (Z.lor (fld (rt) 0) (Z.lor (fld (rn) 5) (fld (off / 8) 10)))
  | Ldrb rt rn off => Z.lor 0x39400000 (Z.lor (fld (rt) 0) (Z.lor (fld (rn) 5) (fld (off) 10)))
  | Strb rt rn off => Z.lor 0x39000000 (Z.lor (fld (rt) 0) (Z.lor (fld (rn) 5) (fld (off) 10)))
  | Ldrw rt rn off => Z.lor 0xB9400000 (Z.lor (fld (rt) 0) (Z.lor (fld (rn) 5) (fld (off / 4) 10)))
  | Strw rt rn off => Z.lor 0xB9000000 (Z.lor (fld (rt) 0) (Z.lor (fld (rn) 5) (fld (off / 4) 10)))
  | Ldar rt rn => Z.lor 0xC8DFFC00 (Z.lor (fld (rt) 0) (fld (rn) 5))
  | Stlr rt rn => Z.lor 0xC89FFC00 (Z.lor (fld (rt) 0) (fld (rn) 5))
  | Cmp rn rm => Z.lor 0xEB00001F (Z.lor (fld (rn) 5) (fld (rm) 16))
  | Cmpi rn imm => Z.lor 0xF100001F (Z.lor (fld (rn) 5) (fld (imm) 10))
  | Cset rd cond => Z.lor 0x9A9F07E0 (Z.lor (fld (rd) 0) (fld (Z.lxor cond 1) 12))
  | B d => Z.lor 0x14000000 (fld (low 26 d) 0)
  | Bl d => Z.lor 0x94000000 (fld (low 26 d) 0)
  | Bcond cond d => Z.lor 0x54000000 (Z.lor (fld (cond) 0) (fld (low 19 d) 5))
  | Cbz rt d => Z.lor 0xB4000000 (Z.lor (fld (rt) 0) (fld (low 19 d) 5))
  | Cbnz rt d => Z.lor 0xB5000000 (Z.lor (fld (rt) 0) (fld (low 19 d) 5))
  | Adr rd bd => Z.lor 0x10000000 (Z.lor (fld (rd) 0) (Z.lor (fld (low 19 (bd / 4)) 5) (fld (bd mod 4) 29)))
  | Svc imm => Z.lor 0xD4000001 (fld (imm) 5)
  | Ret => 0xD65F03C0
  | Brk => 0xD4200000
  | Nop => 0xD503201F
  | DsbIsh => 0xD5033B9F
  | Isb => 0xD5033FDF
  | Blr rn => Z.lor 0xD63F0000 (fld (rn) 5)
  | Br rn => Z.lor 0xD61F0000 (fld (rn) 5)
  | IcIvau rt => Z.lor 0xD50B7520 (fld (rt) 0)
  | DcCvau rt => Z.lor 0xD50B7B20 (fld (rt) 0)
  end.

(* The shipped encoders end in MSK ($FFFFFFFF and); this is that step. *)
Definition emit (i : insn) : Z := Z.land (enc i) 0xFFFFFFFF.

(* Well formed: every operand fits the field the encoder drops it into, and
   every scaled operand really is a multiple of its scale.  These are the
   ranges the ARM64 encodings allow - NOT the ranges the shipped words check,
   which is the point of the findings below. *)
Definition uok (w v : Z) : bool := (0 <=? v) && (v <? 2 ^ w).
Definition sok (w v : Z) : bool := (- (2 ^ (w - 1)) <=? v) && (v <? 2 ^ (w - 1)).
Definition rok (r : Z) : bool := uok 5 r.

Definition wf (i : insn) : bool :=
  match i with
  | Movz rd imm hw => rok rd && uok 16 imm && uok 2 hw
  | Movn rd imm hw => rok rd && uok 16 imm && uok 2 hw
  | Movk rd imm sh => rok rd && uok 16 imm && (sh mod 16 =? 0) && uok 2 (sh / 16)
  | Add rd rn rm => rok rd && rok rn && rok rm
  | Sub rd rn rm => rok rd && rok rn && rok rm
  | And rd rn rm => rok rd && rok rn && rok rm
  | Orr rd rn rm => rok rd && rok rn && rok rm
  | Eor rd rn rm => rok rd && rok rn && rok rm
  | Mul rd rn rm => rok rd && rok rn && rok rm
  | Sdiv rd rn rm => rok rd && rok rn && rok rm
  | Udiv rd rn rm => rok rd && rok rn && rok rm
  | Lslv rd rn rm => rok rd && rok rn && rok rm
  | Lsrv rd rn rm => rok rd && rok rn && rok rm
  | Addi rd rn imm => rok rd && rok rn && uok 12 imm
  | Subi rd rn imm => rok rd && rok rn && uok 12 imm
  | Andi rd rn nis => rok rd && rok rn && uok 13 nis
  | Orri rd rn nis => rok rd && rok rn && uok 13 nis
  | Eori rd rn nis => rok rd && rok rn && uok 13 nis
  | Lsli rd rn sh => rok rd && rok rn && (1 <=? sh) && (sh <? 64)
  | Lsri rd rn sh => rok rd && rok rn && (0 <=? sh) && (sh <? 64)
  | Asri rd rn sh => rok rd && rok rn && (0 <=? sh) && (sh <? 64)
  | Ldr rt rn off => rok rt && rok rn && (off mod 8 =? 0) && uok 12 (off / 8)
  | Str rt rn off => rok rt && rok rn && (off mod 8 =? 0) && uok 12 (off / 8)
  | Ldrb rt rn off => rok rt && rok rn && uok 12 off
  | Strb rt rn off => rok rt && rok rn && uok 12 off
  | Ldrw rt rn off => rok rt && rok rn && (off mod 4 =? 0) && uok 12 (off / 4)
  | Strw rt rn off => rok rt && rok rn && (off mod 4 =? 0) && uok 12 (off / 4)
  | Ldar rt rn => rok rt && rok rn
  | Stlr rt rn => rok rt && rok rn
  | Cmp rn rm => rok rn && rok rm
  | Cmpi rn imm => rok rn && uok 12 imm
  | Cset rd cond => rok rd && uok 4 cond
  | B d => sok 26 d
  | Bl d => sok 26 d
  | Bcond cond d => uok 4 cond && sok 19 d
  | Cbz rt d => rok rt && sok 19 d
  | Cbnz rt d => rok rt && sok 19 d
  | Adr rd bd => rok rd && (bd mod 4 =? 0) && sok 21 bd
  | Svc imm => uok 16 imm
  | Ret => true
  | Brk => true
  | Nop => true
  | DsbIsh => true
  | Isb => true
  | Blr rn => rok rn
  | Br rn => rok rn
  | IcIvau rt => rok rt
  | DcCvau rt => rok rt
  end.

(* Which operands the shipped encoder passes through XREG?, the
   reserved-register refusal in src/arch/arm64/asm.f, and which operands are
   X registers at all.  Read off the shipped code: XR3 checks all three, XRDI
   the destination and base, XRD3 and XR2ND the first, XR2 both, and the
   branch emitters in src/arch/arm64/icode.f build their words without
   calling the asm.f encoders at all. *)
Definition checked_regs (i : insn) : list Z :=
  match i with
  | Movz rd imm hw => [rd]
  | Movn rd imm hw => [rd]
  | Movk rd imm sh => [rd]
  | Add rd rn rm => [rd; rn; rm]
  | Sub rd rn rm => [rd; rn; rm]
  | And rd rn rm => [rd; rn; rm]
  | Orr rd rn rm => [rd; rn; rm]
  | Eor rd rn rm => [rd; rn; rm]
  | Mul rd rn rm => [rd; rn; rm]
  | Sdiv rd rn rm => [rd; rn; rm]
  | Udiv rd rn rm => [rd; rn; rm]
  | Lslv rd rn rm => [rd; rn; rm]
  | Lsrv rd rn rm => [rd; rn; rm]
  | Addi rd rn imm => [rd; rn]
  | Subi rd rn imm => [rd; rn]
  | Andi rd rn nis => [rd; rn]
  | Orri rd rn nis => [rd; rn]
  | Eori rd rn nis => [rd; rn]
  | Lsli rd rn sh => [rd; rn]
  | Lsri rd rn sh => [rd; rn]
  | Asri rd rn sh => [rd; rn]
  | Ldr rt rn off => [rt; rn]
  | Str rt rn off => [rt; rn]
  | Ldrb rt rn off => [rt; rn]
  | Strb rt rn off => [rt; rn]
  | Ldrw rt rn off => [rt; rn]
  | Strw rt rn off => [rt; rn]
  | Ldar rt rn => []
  | Stlr rt rn => []
  | Cmp rn rm => [rn; rm]
  | Cmpi rn imm => [rn]
  | Cset rd cond => [rd]
  | B d => []
  | Bl d => []
  | Bcond cond d => []
  | Cbz rt d => []
  | Cbnz rt d => []
  | Adr rd bd => [rd]
  | Svc imm => []
  | Ret => []
  | Brk => []
  | Nop => []
  | DsbIsh => []
  | Isb => []
  | Blr rn => [rn]
  | Br rn => [rn]
  | IcIvau rt => [rt]
  | DcCvau rt => [rt]
  end.

Definition xregs (i : insn) : list Z :=
  match i with
  | Movz rd imm hw => [rd]
  | Movn rd imm hw => [rd]
  | Movk rd imm sh => [rd]
  | Add rd rn rm => [rd; rn; rm]
  | Sub rd rn rm => [rd; rn; rm]
  | And rd rn rm => [rd; rn; rm]
  | Orr rd rn rm => [rd; rn; rm]
  | Eor rd rn rm => [rd; rn; rm]
  | Mul rd rn rm => [rd; rn; rm]
  | Sdiv rd rn rm => [rd; rn; rm]
  | Udiv rd rn rm => [rd; rn; rm]
  | Lslv rd rn rm => [rd; rn; rm]
  | Lsrv rd rn rm => [rd; rn; rm]
  | Addi rd rn imm => [rd; rn]
  | Subi rd rn imm => [rd; rn]
  | Andi rd rn nis => [rd; rn]
  | Orri rd rn nis => [rd; rn]
  | Eori rd rn nis => [rd; rn]
  | Lsli rd rn sh => [rd; rn]
  | Lsri rd rn sh => [rd; rn]
  | Asri rd rn sh => [rd; rn]
  | Ldr rt rn off => [rt; rn]
  | Str rt rn off => [rt; rn]
  | Ldrb rt rn off => [rt; rn]
  | Strb rt rn off => [rt; rn]
  | Ldrw rt rn off => [rt; rn]
  | Strw rt rn off => [rt; rn]
  | Ldar rt rn => [rt; rn]
  | Stlr rt rn => [rt; rn]
  | Cmp rn rm => [rn; rm]
  | Cmpi rn imm => [rn]
  | Cset rd cond => [rd]
  | B d => []
  | Bl d => []
  | Bcond cond d => []
  | Cbz rt d => [rt]
  | Cbnz rt d => [rt]
  | Adr rd bd => [rd]
  | Svc imm => []
  | Ret => []
  | Brk => []
  | Nop => []
  | DsbIsh => []
  | Isb => []
  | Blr rn => [rn]
  | Br rn => [rn]
  | IcIvau rt => [rt]
  | DcCvau rt => [rt]
  end.

(* x18 is Darwin platform-reserved: the kernel zeroes it on any synchronous
   trap return, so emitted code must never hold live state there. *)
Definition reserved : Z := 18.
Definition refused (i : insn) : bool := existsb (Z.eqb reserved) (checked_regs i).

(* ---- the decoder --------------------------------------------------------

   One row per opcode: the bits that identify it, the value those bits take,
   and how to read the operands back out.  A row's mask never covers an
   operand field, so `Z.land w (rmask r) = rval r` is a question about the
   opcode alone.  `decoder_rows_exclusive` below is what lets the decoder take
   the first matching row without the answer depending on the order. *)
Record row := R { rmask : Z; rval : Z; rmk : Z -> insn }.

Definition row_Movz := R 0xFF800000 0xD2800000 (fun w => Movz (get 0 5 w) (get 5 16 w) (get 21 2 w)).
Definition row_Movn := R 0xFF800000 0x92800000 (fun w => Movn (get 0 5 w) (get 5 16 w) (get 21 2 w)).
Definition row_Movk := R 0xFF800000 0xF2800000 (fun w => Movk (get 0 5 w) (get 5 16 w) (16 * get 21 2 w)).
Definition row_Add := R 0xFFE0FC00 0x8B000000 (fun w => Add (get 0 5 w) (get 5 5 w) (get 16 5 w)).
Definition row_Sub := R 0xFFE0FC00 0xCB000000 (fun w => Sub (get 0 5 w) (get 5 5 w) (get 16 5 w)).
Definition row_And := R 0xFFE0FC00 0x8A000000 (fun w => And (get 0 5 w) (get 5 5 w) (get 16 5 w)).
Definition row_Orr := R 0xFFE0FC00 0xAA000000 (fun w => Orr (get 0 5 w) (get 5 5 w) (get 16 5 w)).
Definition row_Eor := R 0xFFE0FC00 0xCA000000 (fun w => Eor (get 0 5 w) (get 5 5 w) (get 16 5 w)).
Definition row_Mul := R 0xFFE0FC00 0x9B007C00 (fun w => Mul (get 0 5 w) (get 5 5 w) (get 16 5 w)).
Definition row_Sdiv := R 0xFFE0FC00 0x9AC00C00 (fun w => Sdiv (get 0 5 w) (get 5 5 w) (get 16 5 w)).
Definition row_Udiv := R 0xFFE0FC00 0x9AC00800 (fun w => Udiv (get 0 5 w) (get 5 5 w) (get 16 5 w)).
Definition row_Lslv := R 0xFFE0FC00 0x9AC02000 (fun w => Lslv (get 0 5 w) (get 5 5 w) (get 16 5 w)).
Definition row_Lsrv := R 0xFFE0FC00 0x9AC02400 (fun w => Lsrv (get 0 5 w) (get 5 5 w) (get 16 5 w)).
Definition row_Addi := R 0xFFC00000 0x91000000 (fun w => Addi (get 0 5 w) (get 5 5 w) (get 10 12 w)).
Definition row_Subi := R 0xFFC00000 0xD1000000 (fun w => Subi (get 0 5 w) (get 5 5 w) (get 10 12 w)).
Definition row_Andi := R 0xFF800000 0x92000000 (fun w => Andi (get 0 5 w) (get 5 5 w) (get 10 13 w)).
Definition row_Orri := R 0xFF800000 0xB2000000 (fun w => Orri (get 0 5 w) (get 5 5 w) (get 10 13 w)).
Definition row_Eori := R 0xFF800000 0xD2000000 (fun w => Eori (get 0 5 w) (get 5 5 w) (get 10 13 w)).
Definition row_Asri := R 0xFFC0FC00 0x9340FC00 (fun w => Asri (get 0 5 w) (get 5 5 w) (get 16 6 w)).
Definition row_Ldr := R 0xFFC00000 0xF9400000 (fun w => Ldr (get 0 5 w) (get 5 5 w) (8 * get 10 12 w)).
Definition row_Str := R 0xFFC00000 0xF9000000 (fun w => Str (get 0 5 w) (get 5 5 w) (8 * get 10 12 w)).
Definition row_Ldrb := R 0xFFC00000 0x39400000 (fun w => Ldrb (get 0 5 w) (get 5 5 w) (get 10 12 w)).
Definition row_Strb := R 0xFFC00000 0x39000000 (fun w => Strb (get 0 5 w) (get 5 5 w) (get 10 12 w)).
Definition row_Ldrw := R 0xFFC00000 0xB9400000 (fun w => Ldrw (get 0 5 w) (get 5 5 w) (4 * get 10 12 w)).
Definition row_Strw := R 0xFFC00000 0xB9000000 (fun w => Strw (get 0 5 w) (get 5 5 w) (4 * get 10 12 w)).
Definition row_Ldar := R 0xFFFFFC00 0xC8DFFC00 (fun w => Ldar (get 0 5 w) (get 5 5 w)).
Definition row_Stlr := R 0xFFFFFC00 0xC89FFC00 (fun w => Stlr (get 0 5 w) (get 5 5 w)).
Definition row_Cmp := R 0xFFE0FC1F 0xEB00001F (fun w => Cmp (get 5 5 w) (get 16 5 w)).
Definition row_Cmpi := R 0xFFC0001F 0xF100001F (fun w => Cmpi (get 5 5 w) (get 10 12 w)).
Definition row_Cset := R 0xFFFF0FE0 0x9A9F07E0 (fun w => Cset (get 0 5 w) (Z.lxor (get 12 4 w) 1)).
Definition row_B := R 0xFC000000 0x14000000 (fun w => B (sext 26 (get 0 26 w))).
Definition row_Bl := R 0xFC000000 0x94000000 (fun w => Bl (sext 26 (get 0 26 w))).
Definition row_Bcond := R 0xFF000010 0x54000000 (fun w => Bcond (get 0 4 w) (sext 19 (get 5 19 w))).
Definition row_Cbz := R 0xFF000000 0xB4000000 (fun w => Cbz (get 0 5 w) (sext 19 (get 5 19 w))).
Definition row_Cbnz := R 0xFF000000 0xB5000000 (fun w => Cbnz (get 0 5 w) (sext 19 (get 5 19 w))).
Definition row_Adr := R 0x9F000000 0x10000000 (fun w => Adr (get 0 5 w) (4 * sext 19 (get 5 19 w))).
Definition row_Svc := R 0xFFE0001F 0xD4000001 (fun w => Svc (get 5 16 w)).
Definition row_Ret := R 0xFFFFFFFF 0xD65F03C0 (fun w => Ret).
Definition row_Brk := R 0xFFFFFFFF 0xD4200000 (fun w => Brk).
Definition row_Nop := R 0xFFFFFFFF 0xD503201F (fun w => Nop).
Definition row_DsbIsh := R 0xFFFFFFFF 0xD5033B9F (fun w => DsbIsh).
Definition row_Isb := R 0xFFFFFFFF 0xD5033FDF (fun w => Isb).
Definition row_Blr := R 0xFFFFFC1F 0xD63F0000 (fun w => Blr (get 5 5 w)).
Definition row_Br := R 0xFFFFFC1F 0xD61F0000 (fun w => Br (get 5 5 w)).
Definition row_IcIvau := R 0xFFFFFFE0 0xD50B7520 (fun w => IcIvau (get 0 5 w)).
Definition row_DcCvau := R 0xFFFFFFE0 0xD50B7B20 (fun w => DcCvau (get 0 5 w)).

(* LSL and LSR by an immediate are the same UBFM opcode; the shift amount
   field is what tells them apart, so they share one row.  A right shift
   stores imms = 63, a left shift by sh stores 63 - sh. *)
Definition row_Lsli := R 0xFFC00000 0xD3400000 (fun w =>
  if get 10 6 w =? 63
  then Lsri (get 0 5 w) (get 5 5 w) (get 16 6 w)
  else Lsli (get 0 5 w) (get 5 5 w) (63 - get 10 6 w)).

Definition table : list row :=
  [ row_Movz
  ; row_Movn
  ; row_Movk
  ; row_Add
  ; row_Sub
  ; row_And
  ; row_Orr
  ; row_Eor
  ; row_Mul
  ; row_Sdiv
  ; row_Udiv
  ; row_Lslv
  ; row_Lsrv
  ; row_Addi
  ; row_Subi
  ; row_Andi
  ; row_Orri
  ; row_Eori
  ; row_Lsli
  ; row_Asri
  ; row_Ldr
  ; row_Str
  ; row_Ldrb
  ; row_Strb
  ; row_Ldrw
  ; row_Strw
  ; row_Ldar
  ; row_Stlr
  ; row_Cmp
  ; row_Cmpi
  ; row_Cset
  ; row_B
  ; row_Bl
  ; row_Bcond
  ; row_Cbz
  ; row_Cbnz
  ; row_Adr
  ; row_Svc
  ; row_Ret
  ; row_Brk
  ; row_Nop
  ; row_DsbIsh
  ; row_Isb
  ; row_Blr
  ; row_Br
  ; row_IcIvau
  ; row_DcCvau ].

Definition excl (a b : row) : bool :=
  negb (Z.land (rval a) (Z.land (rmask a) (rmask b)) =?
        Z.land (rval b) (Z.land (rmask a) (rmask b))).

Fixpoint all_excl (t : list row) : bool :=
  match t with
  | [] => true
  | r :: rest => forallb (excl r) rest && all_excl rest
  end.

Definition decode1 (t : list row) (w : Z) : option insn :=
  match find (fun r => Z.land w (rmask r) =? rval r) t with
  | Some r => Some (rmk r w)
  | None => None
  end.

Definition decode (w : Z) : option insn := decode1 table w.

Lemma excl_sound : forall a b w, excl a b = true ->
  Z.land w (rmask a) = rval a -> Z.land w (rmask b) = rval b -> False.
Proof.
  intros a b w He Ha Hb. unfold excl in He.
  apply negb_true_iff, Z.eqb_neq in He. apply He. rewrite <- Ha, <- Hb.
  apply Z.bits_inj'. intros n Hn. rewrite !Z.land_spec. btauto.
Qed.

Lemma decode1_of_match : forall t w r, all_excl t = true -> In r t ->
  Z.land w (rmask r) = rval r -> decode1 t w = Some (rmk r w).
Proof.
  induction t as [| r0 rest IH]; intros w r Hx Hin Hm; [ destruct Hin |].
  simpl in Hx. apply andb_true_iff in Hx as [Hhead Hrest].
  unfold decode1. simpl.
  destruct (Z.land w (rmask r0) =? rval r0) eqn:E0.
  - apply Z.eqb_eq in E0. simpl in Hin. destruct Hin as [-> | Hin]; [reflexivity |].
    exfalso. eapply excl_sound; [ | exact E0 | exact Hm ].
    rewrite forallb_forall in Hhead. apply Hhead. exact Hin.
  - simpl in Hin. destruct Hin as [-> | Hin].
    + apply Z.eqb_neq in E0. contradiction.
    + specialize (IH w r Hrest Hin Hm). unfold decode1 in IH. exact IH.
Qed.

(* ---- proof plumbing -----------------------------------------------------

   Three steps, the same for every form: turn `wf` into arithmetic facts,
   push a field read through the encoding, and push an opcode mask through
   it.  The side conditions about positions and masks are closed arithmetic
   and are decided by computation. *)

Ltac wfsplit H :=
  cbv [wf rok uok sok] in H;
  repeat match goal with
  | [ K : andb _ _ = true |- _ ] => apply andb_true_iff in K; destruct K as [? ?]
  end;
  repeat match goal with
  | [ K : (_ <=? _) = true |- _ ] => apply Z.leb_le in K
  | [ K : (_ <? _) = true |- _ ] => apply Z.ltb_lt in K
  | [ K : (_ =? _) = true |- _ ] => apply Z.eqb_eq in K
  end.

(* Every side condition a field rewrite raises is one of five shapes: a closed
   mask identity, the range of a truncated delta, of an exclusive-or, of a
   remainder, or plain arithmetic.  The closed ones are decided first so the
   arithmetic decision procedure never has to look at a bitwise term. *)
Ltac side :=
  lazymatch goal with
  | [ |- Z.land _ _ = 0 ] => vm_compute; reflexivity
  | [ |- Z.land _ _ = msk _ _ ] => vm_compute; reflexivity
  | [ |- 0 <= low _ _ < _ ] => apply low_bnd; lia
  | [ |- 0 <= Z.lxor _ _ < _ ] => apply lxor_bnd; lia
  | [ |- 0 <= _ mod 4 < _ ] => apply mod4_bnd
  | [ |- 0 <= _ mod 64 < _ ] => apply mod64_bnd
  | _ => lia
  end.

Ltac gz := first
  [ rewrite (get_fld_zero _ _ _ _ 2) by side
  | rewrite (get_fld_zero _ _ _ _ 4) by side
  | rewrite (get_fld_zero _ _ _ _ 5) by side
  | rewrite (get_fld_zero _ _ _ _ 6) by side
  | rewrite (get_fld_zero _ _ _ _ 12) by side
  | rewrite (get_fld_zero _ _ _ _ 13) by side
  | rewrite (get_fld_zero _ _ _ _ 16) by side
  | rewrite (get_fld_zero _ _ _ _ 19) by side
  | rewrite (get_fld_zero _ _ _ _ 26) by side ].

Ltac lz := first
  [ rewrite (fld_land_disjoint _ _ 2) by side
  | rewrite (fld_land_disjoint _ _ 4) by side
  | rewrite (fld_land_disjoint _ _ 5) by side
  | rewrite (fld_land_disjoint _ _ 6) by side
  | rewrite (fld_land_disjoint _ _ 12) by side
  | rewrite (fld_land_disjoint _ _ 13) by side
  | rewrite (fld_land_disjoint _ _ 16) by side
  | rewrite (fld_land_disjoint _ _ 19) by side
  | rewrite (fld_land_disjoint _ _ 26) by side ].

Ltac ls := first
  [ rewrite (fld_land_super _ _ 2) by side
  | rewrite (fld_land_super _ _ 4) by side
  | rewrite (fld_land_super _ _ 5) by side
  | rewrite (fld_land_super _ _ 6) by side
  | rewrite (fld_land_super _ _ 12) by side
  | rewrite (fld_land_super _ _ 13) by side
  | rewrite (fld_land_super _ _ 16) by side
  | rewrite (fld_land_super _ _ 19) by side
  | rewrite (fld_land_super _ _ 26) by side ].

Ltac constfold :=
  repeat match goal with
  | [ |- context [ get ?p ?w ?c ] ] =>
      lazymatch c with
      | Z0 => let v := eval vm_compute in (get p w c) in change (get p w c) with v
      | Zpos _ => let v := eval vm_compute in (get p w c) in change (get p w c) with v
      end
  end.

Ltac gfields :=
  repeat rewrite get_lor;
  repeat first [ rewrite get_fld_same by side | gz ];
  constfold;
  repeat rewrite Z.lor_0_l; repeat rewrite Z.lor_0_r;
  try reflexivity;
  try (rewrite Z.lxor_assoc, Z.lxor_nilpotent, Z.lxor_0_r; reflexivity);
  try (apply sext_low; [ lia | lia ]);
  try (apply mul_div_exact; [ lia | assumption ]);
  try (rewrite sext_low; [ apply mul_div_exact; [ lia | assumption ]
                         | lia | apply div4_bnd; lia ]).

Ltac lfields :=
  repeat rewrite land_lor;
  repeat lz;
  repeat rewrite Z.lor_0_l; repeat rewrite Z.lor_0_r;
  vm_compute; reflexivity.

Ltac maskid :=
  repeat rewrite land_lor;
  repeat ls;
  repeat match goal with
  | [ |- context [ Z.land ?c 0xFFFFFFFF ] ] =>
      lazymatch c with
      | Z0 => change (Z.land c 0xFFFFFFFF) with c
      | Zpos _ => let v := eval vm_compute in (Z.land c 0xFFFFFFFF) in
                  change (Z.land c 0xFFFFFFFF) with v
      end
  end;
  reflexivity.

(* ---- decoding an encoded instruction ---------------------------------- *)

Lemma dec_Movz : forall rd imm hw, wf (Movz rd imm hw) = true ->
  decode (enc (Movz rd imm hw)) = Some (Movz rd imm hw).
Proof.
  intros rd imm hw H. wfsplit H.
  unfold decode. rewrite (decode1_of_match table _ row_Movz).
  - cbv [row_Movz rmk enc]. f_equal. f_equal; gfields.
  - vm_compute; reflexivity.
  - cbv [table]; simpl; tauto.
  - cbv [row_Movz rmask rval enc]. lfields.
Qed.

Lemma dec_Movn : forall rd imm hw, wf (Movn rd imm hw) = true ->
  decode (enc (Movn rd imm hw)) = Some (Movn rd imm hw).
Proof.
  intros rd imm hw H. wfsplit H.
  unfold decode. rewrite (decode1_of_match table _ row_Movn).
  - cbv [row_Movn rmk enc]. f_equal. f_equal; gfields.
  - vm_compute; reflexivity.
  - cbv [table]; simpl; tauto.
  - cbv [row_Movn rmask rval enc]. lfields.
Qed.

Lemma dec_Movk : forall rd imm sh, wf (Movk rd imm sh) = true ->
  decode (enc (Movk rd imm sh)) = Some (Movk rd imm sh).
Proof.
  intros rd imm sh H. wfsplit H.
  unfold decode. rewrite (decode1_of_match table _ row_Movk).
  - cbv [row_Movk rmk enc]. f_equal. f_equal; gfields.
  - vm_compute; reflexivity.
  - cbv [table]; simpl; tauto.
  - cbv [row_Movk rmask rval enc]. lfields.
Qed.

Lemma dec_Add : forall rd rn rm, wf (Add rd rn rm) = true ->
  decode (enc (Add rd rn rm)) = Some (Add rd rn rm).
Proof.
  intros rd rn rm H. wfsplit H.
  unfold decode. rewrite (decode1_of_match table _ row_Add).
  - cbv [row_Add rmk enc]. f_equal. f_equal; gfields.
  - vm_compute; reflexivity.
  - cbv [table]; simpl; tauto.
  - cbv [row_Add rmask rval enc]. lfields.
Qed.

Lemma dec_Sub : forall rd rn rm, wf (Sub rd rn rm) = true ->
  decode (enc (Sub rd rn rm)) = Some (Sub rd rn rm).
Proof.
  intros rd rn rm H. wfsplit H.
  unfold decode. rewrite (decode1_of_match table _ row_Sub).
  - cbv [row_Sub rmk enc]. f_equal. f_equal; gfields.
  - vm_compute; reflexivity.
  - cbv [table]; simpl; tauto.
  - cbv [row_Sub rmask rval enc]. lfields.
Qed.

Lemma dec_And : forall rd rn rm, wf (And rd rn rm) = true ->
  decode (enc (And rd rn rm)) = Some (And rd rn rm).
Proof.
  intros rd rn rm H. wfsplit H.
  unfold decode. rewrite (decode1_of_match table _ row_And).
  - cbv [row_And rmk enc]. f_equal. f_equal; gfields.
  - vm_compute; reflexivity.
  - cbv [table]; simpl; tauto.
  - cbv [row_And rmask rval enc]. lfields.
Qed.

Lemma dec_Orr : forall rd rn rm, wf (Orr rd rn rm) = true ->
  decode (enc (Orr rd rn rm)) = Some (Orr rd rn rm).
Proof.
  intros rd rn rm H. wfsplit H.
  unfold decode. rewrite (decode1_of_match table _ row_Orr).
  - cbv [row_Orr rmk enc]. f_equal. f_equal; gfields.
  - vm_compute; reflexivity.
  - cbv [table]; simpl; tauto.
  - cbv [row_Orr rmask rval enc]. lfields.
Qed.

Lemma dec_Eor : forall rd rn rm, wf (Eor rd rn rm) = true ->
  decode (enc (Eor rd rn rm)) = Some (Eor rd rn rm).
Proof.
  intros rd rn rm H. wfsplit H.
  unfold decode. rewrite (decode1_of_match table _ row_Eor).
  - cbv [row_Eor rmk enc]. f_equal. f_equal; gfields.
  - vm_compute; reflexivity.
  - cbv [table]; simpl; tauto.
  - cbv [row_Eor rmask rval enc]. lfields.
Qed.

Lemma dec_Mul : forall rd rn rm, wf (Mul rd rn rm) = true ->
  decode (enc (Mul rd rn rm)) = Some (Mul rd rn rm).
Proof.
  intros rd rn rm H. wfsplit H.
  unfold decode. rewrite (decode1_of_match table _ row_Mul).
  - cbv [row_Mul rmk enc]. f_equal. f_equal; gfields.
  - vm_compute; reflexivity.
  - cbv [table]; simpl; tauto.
  - cbv [row_Mul rmask rval enc]. lfields.
Qed.

Lemma dec_Sdiv : forall rd rn rm, wf (Sdiv rd rn rm) = true ->
  decode (enc (Sdiv rd rn rm)) = Some (Sdiv rd rn rm).
Proof.
  intros rd rn rm H. wfsplit H.
  unfold decode. rewrite (decode1_of_match table _ row_Sdiv).
  - cbv [row_Sdiv rmk enc]. f_equal. f_equal; gfields.
  - vm_compute; reflexivity.
  - cbv [table]; simpl; tauto.
  - cbv [row_Sdiv rmask rval enc]. lfields.
Qed.

Lemma dec_Udiv : forall rd rn rm, wf (Udiv rd rn rm) = true ->
  decode (enc (Udiv rd rn rm)) = Some (Udiv rd rn rm).
Proof.
  intros rd rn rm H. wfsplit H.
  unfold decode. rewrite (decode1_of_match table _ row_Udiv).
  - cbv [row_Udiv rmk enc]. f_equal. f_equal; gfields.
  - vm_compute; reflexivity.
  - cbv [table]; simpl; tauto.
  - cbv [row_Udiv rmask rval enc]. lfields.
Qed.

Lemma dec_Lslv : forall rd rn rm, wf (Lslv rd rn rm) = true ->
  decode (enc (Lslv rd rn rm)) = Some (Lslv rd rn rm).
Proof.
  intros rd rn rm H. wfsplit H.
  unfold decode. rewrite (decode1_of_match table _ row_Lslv).
  - cbv [row_Lslv rmk enc]. f_equal. f_equal; gfields.
  - vm_compute; reflexivity.
  - cbv [table]; simpl; tauto.
  - cbv [row_Lslv rmask rval enc]. lfields.
Qed.

Lemma dec_Lsrv : forall rd rn rm, wf (Lsrv rd rn rm) = true ->
  decode (enc (Lsrv rd rn rm)) = Some (Lsrv rd rn rm).
Proof.
  intros rd rn rm H. wfsplit H.
  unfold decode. rewrite (decode1_of_match table _ row_Lsrv).
  - cbv [row_Lsrv rmk enc]. f_equal. f_equal; gfields.
  - vm_compute; reflexivity.
  - cbv [table]; simpl; tauto.
  - cbv [row_Lsrv rmask rval enc]. lfields.
Qed.

Lemma dec_Addi : forall rd rn imm, wf (Addi rd rn imm) = true ->
  decode (enc (Addi rd rn imm)) = Some (Addi rd rn imm).
Proof.
  intros rd rn imm H. wfsplit H.
  unfold decode. rewrite (decode1_of_match table _ row_Addi).
  - cbv [row_Addi rmk enc]. f_equal. f_equal; gfields.
  - vm_compute; reflexivity.
  - cbv [table]; simpl; tauto.
  - cbv [row_Addi rmask rval enc]. lfields.
Qed.

Lemma dec_Subi : forall rd rn imm, wf (Subi rd rn imm) = true ->
  decode (enc (Subi rd rn imm)) = Some (Subi rd rn imm).
Proof.
  intros rd rn imm H. wfsplit H.
  unfold decode. rewrite (decode1_of_match table _ row_Subi).
  - cbv [row_Subi rmk enc]. f_equal. f_equal; gfields.
  - vm_compute; reflexivity.
  - cbv [table]; simpl; tauto.
  - cbv [row_Subi rmask rval enc]. lfields.
Qed.

Lemma dec_Andi : forall rd rn nis, wf (Andi rd rn nis) = true ->
  decode (enc (Andi rd rn nis)) = Some (Andi rd rn nis).
Proof.
  intros rd rn nis H. wfsplit H.
  unfold decode. rewrite (decode1_of_match table _ row_Andi).
  - cbv [row_Andi rmk enc]. f_equal. f_equal; gfields.
  - vm_compute; reflexivity.
  - cbv [table]; simpl; tauto.
  - cbv [row_Andi rmask rval enc]. lfields.
Qed.

Lemma dec_Orri : forall rd rn nis, wf (Orri rd rn nis) = true ->
  decode (enc (Orri rd rn nis)) = Some (Orri rd rn nis).
Proof.
  intros rd rn nis H. wfsplit H.
  unfold decode. rewrite (decode1_of_match table _ row_Orri).
  - cbv [row_Orri rmk enc]. f_equal. f_equal; gfields.
  - vm_compute; reflexivity.
  - cbv [table]; simpl; tauto.
  - cbv [row_Orri rmask rval enc]. lfields.
Qed.

Lemma dec_Eori : forall rd rn nis, wf (Eori rd rn nis) = true ->
  decode (enc (Eori rd rn nis)) = Some (Eori rd rn nis).
Proof.
  intros rd rn nis H. wfsplit H.
  unfold decode. rewrite (decode1_of_match table _ row_Eori).
  - cbv [row_Eori rmk enc]. f_equal. f_equal; gfields.
  - vm_compute; reflexivity.
  - cbv [table]; simpl; tauto.
  - cbv [row_Eori rmask rval enc]. lfields.
Qed.

Lemma dec_Asri : forall rd rn sh, wf (Asri rd rn sh) = true ->
  decode (enc (Asri rd rn sh)) = Some (Asri rd rn sh).
Proof.
  intros rd rn sh H. wfsplit H.
  unfold decode. rewrite (decode1_of_match table _ row_Asri).
  - cbv [row_Asri rmk enc]. f_equal. f_equal; gfields.
  - vm_compute; reflexivity.
  - cbv [table]; simpl; tauto.
  - cbv [row_Asri rmask rval enc]. lfields.
Qed.

Lemma dec_Ldr : forall rt rn off, wf (Ldr rt rn off) = true ->
  decode (enc (Ldr rt rn off)) = Some (Ldr rt rn off).
Proof.
  intros rt rn off H. wfsplit H.
  unfold decode. rewrite (decode1_of_match table _ row_Ldr).
  - cbv [row_Ldr rmk enc]. f_equal. f_equal; gfields.
  - vm_compute; reflexivity.
  - cbv [table]; simpl; tauto.
  - cbv [row_Ldr rmask rval enc]. lfields.
Qed.

Lemma dec_Str : forall rt rn off, wf (Str rt rn off) = true ->
  decode (enc (Str rt rn off)) = Some (Str rt rn off).
Proof.
  intros rt rn off H. wfsplit H.
  unfold decode. rewrite (decode1_of_match table _ row_Str).
  - cbv [row_Str rmk enc]. f_equal. f_equal; gfields.
  - vm_compute; reflexivity.
  - cbv [table]; simpl; tauto.
  - cbv [row_Str rmask rval enc]. lfields.
Qed.

Lemma dec_Ldrb : forall rt rn off, wf (Ldrb rt rn off) = true ->
  decode (enc (Ldrb rt rn off)) = Some (Ldrb rt rn off).
Proof.
  intros rt rn off H. wfsplit H.
  unfold decode. rewrite (decode1_of_match table _ row_Ldrb).
  - cbv [row_Ldrb rmk enc]. f_equal. f_equal; gfields.
  - vm_compute; reflexivity.
  - cbv [table]; simpl; tauto.
  - cbv [row_Ldrb rmask rval enc]. lfields.
Qed.

Lemma dec_Strb : forall rt rn off, wf (Strb rt rn off) = true ->
  decode (enc (Strb rt rn off)) = Some (Strb rt rn off).
Proof.
  intros rt rn off H. wfsplit H.
  unfold decode. rewrite (decode1_of_match table _ row_Strb).
  - cbv [row_Strb rmk enc]. f_equal. f_equal; gfields.
  - vm_compute; reflexivity.
  - cbv [table]; simpl; tauto.
  - cbv [row_Strb rmask rval enc]. lfields.
Qed.

Lemma dec_Ldrw : forall rt rn off, wf (Ldrw rt rn off) = true ->
  decode (enc (Ldrw rt rn off)) = Some (Ldrw rt rn off).
Proof.
  intros rt rn off H. wfsplit H.
  unfold decode. rewrite (decode1_of_match table _ row_Ldrw).
  - cbv [row_Ldrw rmk enc]. f_equal. f_equal; gfields.
  - vm_compute; reflexivity.
  - cbv [table]; simpl; tauto.
  - cbv [row_Ldrw rmask rval enc]. lfields.
Qed.

Lemma dec_Strw : forall rt rn off, wf (Strw rt rn off) = true ->
  decode (enc (Strw rt rn off)) = Some (Strw rt rn off).
Proof.
  intros rt rn off H. wfsplit H.
  unfold decode. rewrite (decode1_of_match table _ row_Strw).
  - cbv [row_Strw rmk enc]. f_equal. f_equal; gfields.
  - vm_compute; reflexivity.
  - cbv [table]; simpl; tauto.
  - cbv [row_Strw rmask rval enc]. lfields.
Qed.

Lemma dec_Ldar : forall rt rn, wf (Ldar rt rn) = true ->
  decode (enc (Ldar rt rn)) = Some (Ldar rt rn).
Proof.
  intros rt rn H. wfsplit H.
  unfold decode. rewrite (decode1_of_match table _ row_Ldar).
  - cbv [row_Ldar rmk enc]. f_equal. f_equal; gfields.
  - vm_compute; reflexivity.
  - cbv [table]; simpl; tauto.
  - cbv [row_Ldar rmask rval enc]. lfields.
Qed.

Lemma dec_Stlr : forall rt rn, wf (Stlr rt rn) = true ->
  decode (enc (Stlr rt rn)) = Some (Stlr rt rn).
Proof.
  intros rt rn H. wfsplit H.
  unfold decode. rewrite (decode1_of_match table _ row_Stlr).
  - cbv [row_Stlr rmk enc]. f_equal. f_equal; gfields.
  - vm_compute; reflexivity.
  - cbv [table]; simpl; tauto.
  - cbv [row_Stlr rmask rval enc]. lfields.
Qed.

Lemma dec_Cmp : forall rn rm, wf (Cmp rn rm) = true ->
  decode (enc (Cmp rn rm)) = Some (Cmp rn rm).
Proof.
  intros rn rm H. wfsplit H.
  unfold decode. rewrite (decode1_of_match table _ row_Cmp).
  - cbv [row_Cmp rmk enc]. f_equal. f_equal; gfields.
  - vm_compute; reflexivity.
  - cbv [table]; simpl; tauto.
  - cbv [row_Cmp rmask rval enc]. lfields.
Qed.

Lemma dec_Cmpi : forall rn imm, wf (Cmpi rn imm) = true ->
  decode (enc (Cmpi rn imm)) = Some (Cmpi rn imm).
Proof.
  intros rn imm H. wfsplit H.
  unfold decode. rewrite (decode1_of_match table _ row_Cmpi).
  - cbv [row_Cmpi rmk enc]. f_equal. f_equal; gfields.
  - vm_compute; reflexivity.
  - cbv [table]; simpl; tauto.
  - cbv [row_Cmpi rmask rval enc]. lfields.
Qed.

Lemma dec_Cset : forall rd cond, wf (Cset rd cond) = true ->
  decode (enc (Cset rd cond)) = Some (Cset rd cond).
Proof.
  intros rd cond H. wfsplit H.
  unfold decode. rewrite (decode1_of_match table _ row_Cset).
  - cbv [row_Cset rmk enc]. f_equal. f_equal; gfields.
  - vm_compute; reflexivity.
  - cbv [table]; simpl; tauto.
  - cbv [row_Cset rmask rval enc]. lfields.
Qed.

Lemma dec_B : forall d, wf (B d) = true ->
  decode (enc (B d)) = Some (B d).
Proof.
  intros d H. wfsplit H.
  unfold decode. rewrite (decode1_of_match table _ row_B).
  - cbv [row_B rmk enc]. f_equal. f_equal; gfields.
  - vm_compute; reflexivity.
  - cbv [table]; simpl; tauto.
  - cbv [row_B rmask rval enc]. lfields.
Qed.

Lemma dec_Bl : forall d, wf (Bl d) = true ->
  decode (enc (Bl d)) = Some (Bl d).
Proof.
  intros d H. wfsplit H.
  unfold decode. rewrite (decode1_of_match table _ row_Bl).
  - cbv [row_Bl rmk enc]. f_equal. f_equal; gfields.
  - vm_compute; reflexivity.
  - cbv [table]; simpl; tauto.
  - cbv [row_Bl rmask rval enc]. lfields.
Qed.

Lemma dec_Bcond : forall cond d, wf (Bcond cond d) = true ->
  decode (enc (Bcond cond d)) = Some (Bcond cond d).
Proof.
  intros cond d H. wfsplit H.
  unfold decode. rewrite (decode1_of_match table _ row_Bcond).
  - cbv [row_Bcond rmk enc]. f_equal. f_equal; gfields.
  - vm_compute; reflexivity.
  - cbv [table]; simpl; tauto.
  - cbv [row_Bcond rmask rval enc]. lfields.
Qed.

Lemma dec_Cbz : forall rt d, wf (Cbz rt d) = true ->
  decode (enc (Cbz rt d)) = Some (Cbz rt d).
Proof.
  intros rt d H. wfsplit H.
  unfold decode. rewrite (decode1_of_match table _ row_Cbz).
  - cbv [row_Cbz rmk enc]. f_equal. f_equal; gfields.
  - vm_compute; reflexivity.
  - cbv [table]; simpl; tauto.
  - cbv [row_Cbz rmask rval enc]. lfields.
Qed.

Lemma dec_Cbnz : forall rt d, wf (Cbnz rt d) = true ->
  decode (enc (Cbnz rt d)) = Some (Cbnz rt d).
Proof.
  intros rt d H. wfsplit H.
  unfold decode. rewrite (decode1_of_match table _ row_Cbnz).
  - cbv [row_Cbnz rmk enc]. f_equal. f_equal; gfields.
  - vm_compute; reflexivity.
  - cbv [table]; simpl; tauto.
  - cbv [row_Cbnz rmask rval enc]. lfields.
Qed.

Lemma dec_Adr : forall rd bd, wf (Adr rd bd) = true ->
  decode (enc (Adr rd bd)) = Some (Adr rd bd).
Proof.
  intros rd bd H. wfsplit H.
  unfold decode. rewrite (decode1_of_match table _ row_Adr).
  - cbv [row_Adr rmk enc]. f_equal. f_equal; gfields.
  - vm_compute; reflexivity.
  - cbv [table]; simpl; tauto.
  - cbv [row_Adr rmask rval enc]. lfields.
Qed.

Lemma dec_Svc : forall imm, wf (Svc imm) = true ->
  decode (enc (Svc imm)) = Some (Svc imm).
Proof.
  intros imm H. wfsplit H.
  unfold decode. rewrite (decode1_of_match table _ row_Svc).
  - cbv [row_Svc rmk enc]. f_equal. f_equal; gfields.
  - vm_compute; reflexivity.
  - cbv [table]; simpl; tauto.
  - cbv [row_Svc rmask rval enc]. lfields.
Qed.

Lemma dec_Ret : wf (Ret) = true ->
  decode (enc (Ret)) = Some (Ret).
Proof.
  intros H. wfsplit H.
  unfold decode. rewrite (decode1_of_match table _ row_Ret).
  - cbv [row_Ret rmk enc]. reflexivity.
  - vm_compute; reflexivity.
  - cbv [table]; simpl; tauto.
  - cbv [row_Ret rmask rval enc]. lfields.
Qed.

Lemma dec_Brk : wf (Brk) = true ->
  decode (enc (Brk)) = Some (Brk).
Proof.
  intros H. wfsplit H.
  unfold decode. rewrite (decode1_of_match table _ row_Brk).
  - cbv [row_Brk rmk enc]. reflexivity.
  - vm_compute; reflexivity.
  - cbv [table]; simpl; tauto.
  - cbv [row_Brk rmask rval enc]. lfields.
Qed.

Lemma dec_Nop : wf (Nop) = true ->
  decode (enc (Nop)) = Some (Nop).
Proof.
  intros H. wfsplit H.
  unfold decode. rewrite (decode1_of_match table _ row_Nop).
  - cbv [row_Nop rmk enc]. reflexivity.
  - vm_compute; reflexivity.
  - cbv [table]; simpl; tauto.
  - cbv [row_Nop rmask rval enc]. lfields.
Qed.

Lemma dec_DsbIsh : wf (DsbIsh) = true ->
  decode (enc (DsbIsh)) = Some (DsbIsh).
Proof.
  intros H. wfsplit H.
  unfold decode. rewrite (decode1_of_match table _ row_DsbIsh).
  - cbv [row_DsbIsh rmk enc]. reflexivity.
  - vm_compute; reflexivity.
  - cbv [table]; simpl; tauto.
  - cbv [row_DsbIsh rmask rval enc]. lfields.
Qed.

Lemma dec_Isb : wf (Isb) = true ->
  decode (enc (Isb)) = Some (Isb).
Proof.
  intros H. wfsplit H.
  unfold decode. rewrite (decode1_of_match table _ row_Isb).
  - cbv [row_Isb rmk enc]. reflexivity.
  - vm_compute; reflexivity.
  - cbv [table]; simpl; tauto.
  - cbv [row_Isb rmask rval enc]. lfields.
Qed.

Lemma dec_Blr : forall rn, wf (Blr rn) = true ->
  decode (enc (Blr rn)) = Some (Blr rn).
Proof.
  intros rn H. wfsplit H.
  unfold decode. rewrite (decode1_of_match table _ row_Blr).
  - cbv [row_Blr rmk enc]. f_equal. f_equal; gfields.
  - vm_compute; reflexivity.
  - cbv [table]; simpl; tauto.
  - cbv [row_Blr rmask rval enc]. lfields.
Qed.

Lemma dec_Br : forall rn, wf (Br rn) = true ->
  decode (enc (Br rn)) = Some (Br rn).
Proof.
  intros rn H. wfsplit H.
  unfold decode. rewrite (decode1_of_match table _ row_Br).
  - cbv [row_Br rmk enc]. f_equal. f_equal; gfields.
  - vm_compute; reflexivity.
  - cbv [table]; simpl; tauto.
  - cbv [row_Br rmask rval enc]. lfields.
Qed.

Lemma dec_IcIvau : forall rt, wf (IcIvau rt) = true ->
  decode (enc (IcIvau rt)) = Some (IcIvau rt).
Proof.
  intros rt H. wfsplit H.
  unfold decode. rewrite (decode1_of_match table _ row_IcIvau).
  - cbv [row_IcIvau rmk enc]. f_equal. f_equal; gfields.
  - vm_compute; reflexivity.
  - cbv [table]; simpl; tauto.
  - cbv [row_IcIvau rmask rval enc]. lfields.
Qed.

Lemma dec_DcCvau : forall rt, wf (DcCvau rt) = true ->
  decode (enc (DcCvau rt)) = Some (DcCvau rt).
Proof.
  intros rt H. wfsplit H.
  unfold decode. rewrite (decode1_of_match table _ row_DcCvau).
  - cbv [row_DcCvau rmk enc]. f_equal. f_equal; gfields.
  - vm_compute; reflexivity.
  - cbv [table]; simpl; tauto.
  - cbv [row_DcCvau rmask rval enc]. lfields.
Qed.

Lemma dec_Lsri : forall rd rn sh, wf (Lsri rd rn sh) = true ->
  decode (enc (Lsri rd rn sh)) = Some (Lsri rd rn sh).
Proof.
  intros rd rn sh H. wfsplit H.
  assert (E : get 10 6 (enc (Lsri rd rn sh)) = 63) by (cbv [enc]; gfields).
  unfold decode. rewrite (decode1_of_match table _ row_Lsli).
  - cbv [row_Lsli rmk]. cbv beta. rewrite E. cbv [Z.eqb].
    cbv [enc]. f_equal. f_equal; gfields.
  - vm_compute; reflexivity.
  - cbv [table]; simpl; tauto.
  - cbv [row_Lsli rmask rval enc]. lfields.
Qed.

Lemma dec_Lsli : forall rd rn sh, wf (Lsli rd rn sh) = true ->
  decode (enc (Lsli rd rn sh)) = Some (Lsli rd rn sh).
Proof.
  intros rd rn sh H. wfsplit H.
  assert (E : get 10 6 (enc (Lsli rd rn sh)) = 63 - sh) by (cbv [enc]; gfields).
  unfold decode. rewrite (decode1_of_match table _ row_Lsli).
  - cbv [row_Lsli rmk]. cbv beta. rewrite E.
    replace (63 - sh =? 63) with false by (symmetry; apply Z.eqb_neq; lia).
    cbv [enc]. f_equal. f_equal; try gfields. lia.
  - vm_compute; reflexivity.
  - cbv [table]; simpl; tauto.
  - cbv [row_Lsli rmask rval enc]. lfields.
Qed.

Theorem decode_encode : forall i, wf i = true -> decode (enc i) = Some i.
Proof.
  destruct i.
  - apply dec_Movz.
  - apply dec_Movn.
  - apply dec_Movk.
  - apply dec_Add.
  - apply dec_Sub.
  - apply dec_And.
  - apply dec_Orr.
  - apply dec_Eor.
  - apply dec_Mul.
  - apply dec_Sdiv.
  - apply dec_Udiv.
  - apply dec_Lslv.
  - apply dec_Lsrv.
  - apply dec_Addi.
  - apply dec_Subi.
  - apply dec_Andi.
  - apply dec_Orri.
  - apply dec_Eori.
  - apply dec_Lsli.
  - apply dec_Lsri.
  - apply dec_Asri.
  - apply dec_Ldr.
  - apply dec_Str.
  - apply dec_Ldrb.
  - apply dec_Strb.
  - apply dec_Ldrw.
  - apply dec_Strw.
  - apply dec_Ldar.
  - apply dec_Stlr.
  - apply dec_Cmp.
  - apply dec_Cmpi.
  - apply dec_Cset.
  - apply dec_B.
  - apply dec_Bl.
  - apply dec_Bcond.
  - apply dec_Cbz.
  - apply dec_Cbnz.
  - apply dec_Adr.
  - apply dec_Svc.
  - apply dec_Ret.
  - apply dec_Brk.
  - apply dec_Nop.
  - apply dec_DsbIsh.
  - apply dec_Isb.
  - apply dec_Blr.
  - apply dec_Br.
  - apply dec_IcIvau.
  - apply dec_DcCvau.
Qed.

Corollary enc_injective : forall i j, wf i = true -> wf j = true ->
  enc i = enc j -> i = j.
Proof.
  intros i j Hi Hj He.
  assert (Some i = Some j) as Hs.
  { rewrite <- (decode_encode i Hi), <- (decode_encode j Hj), He. reflexivity. }
  injection Hs. auto.
Qed.

Theorem decoder_rows_exclusive : all_excl table = true.
Proof. vm_compute. reflexivity. Qed.

(* The shipped encoders end every packing with MSK, a 32-bit and.  On a well
   formed instruction that step changes nothing, so it can never be the reason
   a bit went missing. *)
Theorem mask_never_truncates : forall i, wf i = true -> emit i = enc i.
Proof.
  intros i H. unfold emit. destruct i; wfsplit H; cbv [enc]; maskid.
Qed.

(* ---- every bit of the word is accounted for ------------------------------

   The opcode mask of a form and the masks of its operand fields tile the whole
   32-bit word and never overlap.  That is what makes the decoder honest: there
   is no bit it neither checks nor reads, so no field can be moved without one
   of the two masks changing. *)
Definition opmask (i : insn) : Z :=
  match i with
  | Movz _ _ _ => 0xFF800000
  | Movn _ _ _ => 0xFF800000
  | Movk _ _ _ => 0xFF800000
  | Add _ _ _ => 0xFFE0FC00
  | Sub _ _ _ => 0xFFE0FC00
  | And _ _ _ => 0xFFE0FC00
  | Orr _ _ _ => 0xFFE0FC00
  | Eor _ _ _ => 0xFFE0FC00
  | Mul _ _ _ => 0xFFE0FC00
  | Sdiv _ _ _ => 0xFFE0FC00
  | Udiv _ _ _ => 0xFFE0FC00
  | Lslv _ _ _ => 0xFFE0FC00
  | Lsrv _ _ _ => 0xFFE0FC00
  | Addi _ _ _ => 0xFFC00000
  | Subi _ _ _ => 0xFFC00000
  | Andi _ _ _ => 0xFF800000
  | Orri _ _ _ => 0xFF800000
  | Eori _ _ _ => 0xFF800000
  | Lsli _ _ _ => 0xFFC00000
  | Lsri _ _ _ => 0xFFC0FC00
  | Asri _ _ _ => 0xFFC0FC00
  | Ldr _ _ _ => 0xFFC00000
  | Str _ _ _ => 0xFFC00000
  | Ldrb _ _ _ => 0xFFC00000
  | Strb _ _ _ => 0xFFC00000
  | Ldrw _ _ _ => 0xFFC00000
  | Strw _ _ _ => 0xFFC00000
  | Ldar _ _ => 0xFFFFFC00
  | Stlr _ _ => 0xFFFFFC00
  | Cmp _ _ => 0xFFE0FC1F
  | Cmpi _ _ => 0xFFC0001F
  | Cset _ _ => 0xFFFF0FE0
  | B _ => 0xFC000000
  | Bl _ => 0xFC000000
  | Bcond _ _ => 0xFF000010
  | Cbz _ _ => 0xFF000000
  | Cbnz _ _ => 0xFF000000
  | Adr _ _ => 0x9F000000
  | Svc _ => 0xFFE0001F
  | Ret => 0xFFFFFFFF
  | Brk => 0xFFFFFFFF
  | Nop => 0xFFFFFFFF
  | DsbIsh => 0xFFFFFFFF
  | Isb => 0xFFFFFFFF
  | Blr _ => 0xFFFFFC1F
  | Br _ => 0xFFFFFC1F
  | IcIvau _ => 0xFFFFFFE0
  | DcCvau _ => 0xFFFFFFE0
  end.

Definition fldmask (i : insn) : Z :=
  match i with
  | Movz _ _ _ => 0x007FFFFF
  | Movn _ _ _ => 0x007FFFFF
  | Movk _ _ _ => 0x007FFFFF
  | Add _ _ _ => 0x001F03FF
  | Sub _ _ _ => 0x001F03FF
  | And _ _ _ => 0x001F03FF
  | Orr _ _ _ => 0x001F03FF
  | Eor _ _ _ => 0x001F03FF
  | Mul _ _ _ => 0x001F03FF
  | Sdiv _ _ _ => 0x001F03FF
  | Udiv _ _ _ => 0x001F03FF
  | Lslv _ _ _ => 0x001F03FF
  | Lsrv _ _ _ => 0x001F03FF
  | Addi _ _ _ => 0x003FFFFF
  | Subi _ _ _ => 0x003FFFFF
  | Andi _ _ _ => 0x007FFFFF
  | Orri _ _ _ => 0x007FFFFF
  | Eori _ _ _ => 0x007FFFFF
  | Lsli _ _ _ => 0x003FFFFF
  | Lsri _ _ _ => 0x003F03FF
  | Asri _ _ _ => 0x003F03FF
  | Ldr _ _ _ => 0x003FFFFF
  | Str _ _ _ => 0x003FFFFF
  | Ldrb _ _ _ => 0x003FFFFF
  | Strb _ _ _ => 0x003FFFFF
  | Ldrw _ _ _ => 0x003FFFFF
  | Strw _ _ _ => 0x003FFFFF
  | Ldar _ _ => 0x000003FF
  | Stlr _ _ => 0x000003FF
  | Cmp _ _ => 0x001F03E0
  | Cmpi _ _ => 0x003FFFE0
  | Cset _ _ => 0x0000F01F
  | B _ => 0x03FFFFFF
  | Bl _ => 0x03FFFFFF
  | Bcond _ _ => 0x00FFFFEF
  | Cbz _ _ => 0x00FFFFFF
  | Cbnz _ _ => 0x00FFFFFF
  | Adr _ _ => 0x60FFFFFF
  | Svc _ => 0x001FFFE0
  | Ret => 0x00000000
  | Brk => 0x00000000
  | Nop => 0x00000000
  | DsbIsh => 0x00000000
  | Isb => 0x00000000
  | Blr _ => 0x000003E0
  | Br _ => 0x000003E0
  | IcIvau _ => 0x0000001F
  | DcCvau _ => 0x0000001F
  end.

Theorem opcode_and_operands_tile_the_word : forall i,
  Z.lor (opmask i) (fldmask i) = 0xFFFFFFFF /\ Z.land (opmask i) (fldmask i) = 0.
Proof. destruct i; split; vm_compute; reflexivity. Qed.

(* ---- what the shipped guards do and do not cover ------------------------ *)

(* FINDING.  src/arch/arm64/asm.f opens by saying the reserved-register
   refusal runs at encode time "for every X-register operand field".  Four
   modelled forms take an X register the shipped code never shows that check:
   ENC-LDAR and ENC-STLR simply do not call it, and CBZ,/CBNZ, in
   src/arch/arm64/icode.f build their word directly instead of going through
   the ENC-CBZ/ENC-CBNZ encoders that would have called it.  The statement is
   over the whole inductive, so adding a form without recording its checked
   operands breaks this proof. *)
Theorem unguarded_x_register_forms : forall i,
  checked_regs i <> xregs i ->
  (exists a b, i = Ldar a b) \/ (exists a b, i = Stlr a b) \/
  (exists a b, i = Cbz a b) \/ (exists a b, i = Cbnz a b).
Proof.
  destruct i; intros H;
    try (exfalso; apply H; reflexivity);
    eauto 6.
Qed.

(* ---- silent field overflow --------------------------------------------- *)

(* FINDING.  Nothing in the shipped encoders bounds an operand, so an operand
   one past its field runs into the next one.  Both cases below emit the
   encoding of a DIFFERENT well-formed instruction, with no diagnostic: a
   16-bit move immediate of 65536 becomes the same move shifted left 16, and
   a condition code of 16 becomes the condition code the encoder inverts to
   the same four bits. *)
Theorem overflow_aliases_another_instruction :
  wf (Movz 0 65536 0) = false /\ emit (Movz 0 65536 0) = enc (Movz 0 0 1) /\
  wf (Cset 1 16) = false /\ emit (Cset 1 16) = enc (Cset 1 0).
Proof. repeat split; vm_compute; reflexivity. Qed.

(* FINDING.  The mnemonics that carry a byte operand divide it by the access
   scale with a plain Forth `/`, which rounds down.  A misaligned operand is
   therefore not refused; it silently encodes the aligned one below it. *)
Theorem truncating_scale_aliases_another_offset :
  wf (Movk 5 4660 8) = false /\ emit (Movk 5 4660 8) = enc (Movk 5 4660 0) /\
  wf (Ldr 1 2 12) = false /\ emit (Ldr 1 2 12) = enc (Ldr 1 2 8).
Proof. repeat split; vm_compute; reflexivity. Qed.

(* FINDING.  The other way an unbounded operand fails: the word it produces
   is not any instruction this model names, so a decoder - or a reader of a
   dump - cannot even report what was emitted. *)
Theorem overflow_escapes_the_vocabulary :
  wf (Addi 1 2 4096) = false /\ decode (emit (Addi 1 2 4096)) = None /\
  wf (Add 1 2 32) = false /\ decode (emit (Add 1 2 32)) = None.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* A left shift by zero and a right shift by zero really are one instruction,
   which is why a left shift of zero is outside `wf`: it is the one place the
   emitted vocabulary is genuinely ambiguous, and the decoder has to pick a
   name for that word. *)
Theorem lsli_lsri_alias_at_zero :
  wf (Lsli 3 4 0) = false /\ enc (Lsli 3 4 0) = enc (Lsri 3 4 0).
Proof. split; vm_compute; reflexivity. Qed.

(* ---- what these results rest on ---------------------------------------- *)

Print Assumptions decode_encode.
Print Assumptions enc_injective.
Print Assumptions decoder_rows_exclusive.
Print Assumptions mask_never_truncates.
Print Assumptions opcode_and_operands_tile_the_word.
Print Assumptions unguarded_x_register_forms.
Print Assumptions overflow_aliases_another_instruction.
Print Assumptions truncating_scale_aliases_another_offset.
Print Assumptions overflow_escapes_the_vocabulary.
Print Assumptions lsli_lsri_alias_at_zero.
