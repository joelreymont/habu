From Stdlib Require Import Bool Lia ZArith.
From Habu.Common Require Import Ids.

Open Scope Z_scope.
Open Scope bool_scope.

Lemma serial_valid_bounds :
  forall serial,
    Ids.serial_validb serial = true ->
    1 <= serial <= 2147483647.
Proof.
  intros serial Hvalid.
  unfold Ids.serial_validb, Ids.betweenb in Hvalid.
  apply Bool.andb_true_iff in Hvalid as [Hmin Hmax].
  apply Z.leb_le in Hmin.
  apply Z.leb_le in Hmax.
  change (1 <= serial) in Hmin.
  change (serial <= 2147483647) in Hmax.
  lia.
Qed.

Lemma local_valid_bounds :
  forall idx,
    Ids.local_validb idx = true ->
    0 <= idx < 4294967296.
Proof.
  intros idx Hvalid.
  unfold Ids.local_validb, Ids.betweenb in Hvalid.
  apply Bool.andb_true_iff in Hvalid as [Hmin Hmax].
  apply Z.leb_le in Hmin.
  apply Z.leb_le in Hmax.
  change (0 <= idx) in Hmin.
  change (idx <= 4294967295) in Hmax.
  lia.
Qed.

Lemma scalar_in_range :
  forall raw,
    0 <= raw <= Ids.cell_max ->
    Ids.scalar_validb raw = true.
Proof.
  intros raw [Hmin Hmax].
  unfold Ids.scalar_validb, Ids.betweenb.
  apply Bool.andb_true_iff.
  split.
  - apply Z.leb_le.
    exact Hmin.
  - apply Z.leb_le.
  exact Hmax.
Qed.

Theorem count_scalar_id :
  forall raw,
    0 <= raw <= Ids.cell_max ->
    Ids.make_count raw = Some (Ids.MkCount raw)
    /\ Ids.count_raw (Ids.MkCount raw) = raw.
Proof.
  intros raw Hbounds.
  unfold Ids.make_count.
  rewrite scalar_in_range by exact Hbounds.
  split; reflexivity.
Qed.

Theorem pool_off_scalar_id :
  forall raw,
    0 <= raw <= Ids.cell_max ->
    Ids.make_pool_offset raw = Some (Ids.MkPoolOffset raw)
    /\ Ids.pool_offset_raw (Ids.MkPoolOffset raw) = raw.
Proof.
  intros raw Hbounds.
  unfold Ids.make_pool_offset.
  rewrite scalar_in_range by exact Hbounds.
  split; reflexivity.
Qed.

Theorem count_roundtrip :
  forall count,
    Ids.count_validb count = true ->
    Ids.make_count (Ids.count_raw count) = Some count.
Proof.
  intros [raw] Hvalid.
  unfold Ids.count_validb in Hvalid.
  unfold Ids.make_count.
  rewrite Hvalid.
  reflexivity.
Qed.

Theorem pool_off_roundtrip :
  forall offset,
    Ids.pool_offset_validb offset = true ->
    Ids.make_pool_offset (Ids.pool_offset_raw offset) = Some offset.
Proof.
  intros [raw] Hvalid.
  unfold Ids.pool_offset_validb in Hvalid.
  unfold Ids.make_pool_offset.
  rewrite Hvalid.
  reflexivity.
Qed.

Lemma local_mask_identity :
  forall idx,
    Ids.local_validb idx = true ->
    Z.land idx Ids.local_mask = idx.
Proof.
  intros idx Hvalid.
  pose proof (local_valid_bounds idx Hvalid) as Hidx.
  change (Z.land idx 4294967295 = idx).
  replace 4294967295 with (Z.ones 32) by
    (unfold Z.ones; reflexivity).
  rewrite Z.land_ones by lia.
  change (idx mod 4294967296 = idx).
  apply Z.mod_small.
  exact Hidx.
Qed.

Lemma shifted_local_mask_zero :
  forall serial,
    Z.land (Z.shiftl serial Ids.local_shift) Ids.local_mask = 0.
Proof.
  intros serial.
  change (Z.land (Z.shiftl serial 32) 4294967295 = 0).
  replace 4294967295 with (Z.ones 32) by
    (unfold Z.ones; reflexivity).
  rewrite Z.land_ones by lia.
  rewrite Z.shiftl_mul_pow2 by lia.
  change ((serial * 4294967296) mod 4294967296 = 0).
  apply Z.mod_mul.
  discriminate.
Qed.

Lemma shifted_local_disjoint :
  forall serial idx,
    Ids.local_validb idx = true ->
    Z.land (Z.shiftl serial Ids.local_shift) idx = 0.
Proof.
  intros serial idx Hvalid.
  rewrite <- (local_mask_identity idx Hvalid).
  rewrite (Z.land_comm idx Ids.local_mask).
  rewrite Z.land_assoc.
  rewrite shifted_local_mask_zero.
  apply Z.land_0_l.
Qed.

Theorem pack_as_sum :
  forall serial idx,
    Ids.local_validb idx = true ->
    Ids.pack serial idx = serial * 4294967296 + idx.
Proof.
  intros serial idx Hvalid.
  unfold Ids.pack.
  pose proof
    (Z.add_lor_land
       (Z.shiftl serial Ids.local_shift) idx) as Hsum.
  rewrite shifted_local_disjoint in Hsum by exact Hvalid.
  rewrite Z.add_0_r in Hsum.
  rewrite Hsum.
  rewrite Z.shiftl_mul_pow2 by
    (unfold Ids.local_shift; lia).
  unfold Ids.local_shift.
  reflexivity.
Qed.

Theorem pack_within_signed_cell :
  forall serial idx,
    Ids.serial_validb serial = true ->
    Ids.local_validb idx = true ->
    0 <= Ids.pack serial idx <= Ids.cell_max.
Proof.
  intros serial idx Hserial Hidx.
  pose proof (serial_valid_bounds serial Hserial) as Hserial_bounds.
  pose proof (local_valid_bounds idx Hidx) as Hidx_bounds.
  rewrite pack_as_sum by exact Hidx.
  change
    (0 <= serial * 4294967296 + idx <= 9223372036854775807).
  lia.
Qed.

Theorem owner_pack :
  forall serial idx,
    Ids.local_validb idx = true ->
    Ids.owner (Ids.pack serial idx) = serial.
Proof.
  intros serial idx Hvalid.
  unfold Ids.owner.
  rewrite pack_as_sum by exact Hvalid.
  change
    (Z.shiftr (serial * 4294967296 + idx) 32 = serial).
  rewrite Z.shiftr_div_pow2 by lia.
  change
    ((serial * 4294967296 + idx) / 4294967296 = serial).
  rewrite Z.div_add_l by discriminate.
  rewrite Z.div_small.
  - lia.
  - apply local_valid_bounds.
    exact Hvalid.
Qed.

Theorem local_pack :
  forall serial idx,
    Ids.local_validb idx = true ->
    Ids.local (Ids.pack serial idx) = idx.
Proof.
  intros serial idx Hvalid.
  unfold Ids.local.
  change
    (Z.land (Ids.pack serial idx) 4294967295 = idx).
  replace 4294967295 with (Z.ones 32) by
    (unfold Z.ones; reflexivity).
  rewrite Z.land_ones by lia.
  rewrite pack_as_sum by exact Hvalid.
  change
    ((serial * 4294967296 + idx) mod 4294967296 = idx).
  rewrite Z.add_comm.
  rewrite Z.mod_add by discriminate.
  apply Z.mod_small.
  apply local_valid_bounds.
  exact Hvalid.
Qed.

Theorem pack_injective :
  forall serial_a serial_b idx_a idx_b,
    Ids.local_validb idx_a = true ->
    Ids.local_validb idx_b = true ->
    Ids.pack serial_a idx_a = Ids.pack serial_b idx_b ->
    serial_a = serial_b /\ idx_a = idx_b.
Proof.
  intros serial_a serial_b idx_a idx_b
    Hidx_a Hidx_b Hequal.
  split.
  - pose proof (f_equal Ids.owner Hequal) as Howner.
    rewrite owner_pack in Howner by exact Hidx_a.
    rewrite owner_pack in Howner by exact Hidx_b.
    exact Howner.
  - pose proof (f_equal Ids.local Hequal) as Hlocal.
    rewrite local_pack in Hlocal by exact Hidx_a.
    rewrite local_pack in Hlocal by exact Hidx_b.
    exact Hlocal.
Qed.

Theorem pack_projection_roundtrip :
  forall serial idx,
    Ids.local_validb idx = true ->
    Ids.pack
      (Ids.owner (Ids.pack serial idx))
      (Ids.local (Ids.pack serial idx)) =
    Ids.pack serial idx.
Proof.
  intros serial idx Hvalid.
  rewrite owner_pack by exact Hvalid.
  rewrite local_pack by exact Hvalid.
  reflexivity.
Qed.

Lemma owner_as_div :
  forall raw,
    Ids.owner raw = raw / 4294967296.
Proof.
  intros raw.
  unfold Ids.owner.
  change (Z.shiftr raw 32 = raw / 4294967296).
  apply Z.shiftr_div_pow2.
  lia.
Qed.

Lemma local_as_mod :
  forall raw,
    Ids.local raw = raw mod 4294967296.
Proof.
  intros raw.
  unfold Ids.local.
  change (Z.land raw 4294967295 = raw mod 4294967296).
  replace 4294967295 with (Z.ones 32) by
    (unfold Z.ones; reflexivity).
  rewrite Z.land_ones by lia.
  reflexivity.
Qed.

Theorem projection_pack_roundtrip :
  forall raw,
    Ids.packed_validb raw = true ->
    Ids.pack (Ids.owner raw) (Ids.local raw) = raw.
Proof.
  intros raw Hvalid.
  unfold Ids.packed_validb in Hvalid.
  apply Bool.andb_true_iff in Hvalid as [Hprefix Hlocal].
  rewrite pack_as_sum by exact Hlocal.
  rewrite owner_as_div, local_as_mod.
  pose proof (Z.div_mod raw 4294967296 ltac:(discriminate)) as Hsplit.
  lia.
Qed.

Theorem distinct_valid_owners_separate :
  forall serial_a serial_b idx_a idx_b,
    Ids.serial_validb serial_a = true ->
    Ids.serial_validb serial_b = true ->
    Ids.local_validb idx_a = true ->
    Ids.local_validb idx_b = true ->
    serial_a <> serial_b ->
    Ids.pack serial_a idx_a <> Ids.pack serial_b idx_b.
Proof.
  intros serial_a serial_b idx_a idx_b
    Hserial_a Hserial_b Hidx_a Hidx_b Hdistinct Hequal.
  pose proof
    (pack_injective
       serial_a serial_b idx_a idx_b Hidx_a Hidx_b Hequal)
    as [Hserial _].
  contradiction.
Qed.

Theorem try_pack_rejects_negative :
  forall serial idx,
    idx < 0 ->
    Ids.try_pack serial idx = None.
Proof.
  intros serial idx Hnegative.
  unfold Ids.try_pack, Ids.pack_validb, Ids.local_validb,
    Ids.betweenb, Ids.local_min.
  replace (0 <=? idx) with false.
  - destruct (Ids.serial_validb serial); reflexivity.
  - symmetry.
    apply Z.leb_gt.
    exact Hnegative.
Qed.

Theorem try_pack_rejects_overflow :
  forall serial idx,
    Ids.local_max < idx ->
    Ids.try_pack serial idx = None.
Proof.
  intros serial idx Hoverflow.
  unfold Ids.try_pack, Ids.pack_validb, Ids.local_validb,
    Ids.betweenb.
  replace (idx <=? Ids.local_max) with false.
  - rewrite Bool.andb_false_r, Bool.andb_false_r.
    reflexivity.
  - symmetry.
    apply Z.leb_gt.
    exact Hoverflow.
Qed.

Theorem check_rejects_equal_bound :
  forall serial bound,
    Ids.serial_validb serial = true ->
    Ids.local_validb bound = true ->
    Ids.checkb serial bound (Ids.pack serial bound) = false.
Proof.
  intros serial bound Hserial Hbound.
  unfold Ids.checkb.
  rewrite Hserial.
  replace (Ids.scalar_validb bound) with true.
  2: {
    unfold Ids.scalar_validb, Ids.betweenb.
    pose proof (local_valid_bounds bound Hbound) as Hbounds.
    symmetry.
    apply Bool.andb_true_iff.
    split.
    - apply Z.leb_le.
      lia.
    - apply Z.leb_le.
      change (bound <= 9223372036854775807).
      lia.
  }
  replace (Ids.packed_validb (Ids.pack serial bound)) with true.
  2: {
    unfold Ids.packed_validb.
    symmetry.
    apply Bool.andb_true_iff.
    split.
    - apply Bool.andb_true_iff.
      split.
      + unfold Ids.cell_validb, Ids.betweenb.
        apply Bool.andb_true_iff.
        pose proof
          (pack_within_signed_cell serial bound Hserial Hbound)
          as Hcell.
        change
          (0 <= Ids.pack serial bound <= 9223372036854775807)
          in Hcell.
        split.
        * apply Z.leb_le.
          change
            (-9223372036854775808 <= Ids.pack serial bound).
          lia.
        * apply Z.leb_le.
          change
            (Ids.pack serial bound <= 9223372036854775807).
          lia.
      + rewrite owner_pack by exact Hbound.
        exact Hserial.
    - rewrite local_pack by exact Hbound.
      exact Hbound.
  }
  rewrite owner_pack by exact Hbound.
  rewrite local_pack by exact Hbound.
  rewrite Z.eqb_refl, Z.ltb_irrefl.
  reflexivity.
Qed.

Theorem check_rejects_foreign_owner :
  forall expected actual bound idx,
    Ids.serial_validb expected = true ->
    Ids.serial_validb actual = true ->
    Ids.scalar_validb bound = true ->
    Ids.local_validb idx = true ->
    idx < bound ->
    expected <> actual ->
    Ids.checkb expected bound (Ids.pack actual idx) = false.
Proof.
  intros expected actual bound idx
    Hexpected Hactual Hbound Hidx Hinbound Hforeign.
  unfold Ids.checkb.
  rewrite Hexpected, Hbound.
  replace (Ids.packed_validb (Ids.pack actual idx)) with true.
  2: {
    unfold Ids.packed_validb.
    symmetry.
    apply Bool.andb_true_iff.
    split.
    - apply Bool.andb_true_iff.
      split.
      + unfold Ids.cell_validb, Ids.betweenb.
        apply Bool.andb_true_iff.
        pose proof
          (pack_within_signed_cell actual idx Hactual Hidx)
          as Hcell.
        change
          (0 <= Ids.pack actual idx <= 9223372036854775807)
          in Hcell.
        split.
        * apply Z.leb_le.
          change
            (-9223372036854775808 <= Ids.pack actual idx).
          lia.
        * apply Z.leb_le.
          change
            (Ids.pack actual idx <= 9223372036854775807).
          lia.
      + rewrite owner_pack by exact Hidx.
        exact Hactual.
    - rewrite local_pack by exact Hidx.
      exact Hidx.
  }
  rewrite owner_pack by exact Hidx.
  replace (actual =? expected) with false.
  - reflexivity.
  - symmetry.
    apply Z.eqb_neq.
    congruence.
Qed.

Example negative_local_rejects_exact :
  Ids.try_pack 1 (-1) = None.
Proof. reflexivity. Qed.

Example equal_bound_rejects_exact :
  Ids.checkb 7 42 (Ids.pack 7 42) = false.
Proof. vm_compute; reflexivity. Qed.

Example overflow_local_rejects_exact :
  Ids.try_pack 1 4294967296 = None.
Proof. reflexivity. Qed.

Example foreign_owner_rejects_exact :
  Ids.checkb 8 43 (Ids.pack 7 42) = false.
Proof. vm_compute; reflexivity. Qed.

Definition cell_fits_at_widthb (bits raw : Z) : bool :=
  Ids.betweenb (-(2 ^ (bits - 1))) (2 ^ (bits - 1) - 1) raw.

Definition serial_fits_at_widthb (bits serial : Z) : bool :=
  Ids.betweenb 1 (2 ^ bits - 1) serial.

Definition local_fits_at_widthb (bits idx : Z) : bool :=
  Ids.betweenb 0 (2 ^ bits - 1) idx.

Definition pack_at_shift (shift serial idx : Z) : Z :=
  Z.lor (Z.shiftl serial shift) idx.

Definition owner_at_shift (shift raw : Z) : Z :=
  Z.shiftr raw shift.

Definition local_at_mask (mask raw : Z) : Z :=
  Z.land raw mask.

Definition make_trunc {A : Type} (make : Z -> A) (raw : Z) : option A :=
  if Ids.scalar_validb raw
  then Some (make (Z.land raw Ids.local_mask))
  else None.

Definition check_without_owner_eq
    (serial bound raw : Z) : bool :=
  Ids.serial_validb serial
  && Ids.scalar_validb bound
  && Ids.packed_validb raw
  && Z.ltb (Ids.local raw) bound.

Definition check_with_nonstrict_bound
    (serial bound raw : Z) : bool :=
  Ids.serial_validb serial
  && Ids.scalar_validb bound
  && Ids.packed_validb raw
  && Z.eqb (Ids.owner raw) serial
  && Z.leb (Ids.local raw) bound.

Definition owner_projection_expectedb (shift : Z) : bool :=
  Z.eqb
    (owner_at_shift shift
      (pack_at_shift shift 2 2147483648))
    2.

Definition local_projection_expectedb (mask : Z) : bool :=
  Z.eqb
    (local_at_mask mask (Ids.pack 7 2147483648))
    2147483648.

Definition foreign_owner_rejectedb
    (checker : Z -> Z -> Z -> bool) : bool :=
  negb (checker 8 43 (Ids.pack 7 42)).

Definition equal_bound_rejectedb
    (checker : Z -> Z -> Z -> bool) : bool :=
  negb (checker 7 42 (Ids.pack 7 42)).

Example mutation_cell_width_falsifies_max_pack_fit :
  cell_fits_at_widthb Ids.cell_bits
    (Ids.pack Ids.serial_max Ids.local_max) = true
  /\ cell_fits_at_widthb 63
    (Ids.pack Ids.serial_max Ids.local_max) = false.
Proof. repeat split; vm_compute; reflexivity. Qed.

Example mutation_serial_width_falsifies_max_serial_fit :
  serial_fits_at_widthb Ids.serial_bits Ids.serial_max = true
  /\ serial_fits_at_widthb 30 Ids.serial_max = false.
Proof. repeat split; vm_compute; reflexivity. Qed.

Example mutation_local_width_falsifies_max_local_fit :
  local_fits_at_widthb Ids.local_bits Ids.local_max = true
  /\ local_fits_at_widthb 31 Ids.local_max = false.
Proof. repeat split; vm_compute; reflexivity. Qed.

Example mutation_shift_falsifies_owner_projection :
  owner_projection_expectedb Ids.local_shift = true
  /\ owner_projection_expectedb 31 = false.
Proof. repeat split; vm_compute; reflexivity. Qed.

Example mutation_mask_falsifies_local_projection :
  local_projection_expectedb Ids.local_mask = true
  /\ local_projection_expectedb 2147483647 = false.
Proof. repeat split; vm_compute; reflexivity. Qed.

Example mutation_owner_eq_falsifies_foreign_rejection :
  foreign_owner_rejectedb Ids.checkb = true
  /\ foreign_owner_rejectedb check_without_owner_eq = false.
Proof. repeat split; vm_compute; reflexivity. Qed.

Example mutation_strict_bound_falsifies_equal_rejection :
  equal_bound_rejectedb Ids.checkb = true
  /\ equal_bound_rejectedb check_with_nonstrict_bound = false.
Proof. repeat split; vm_compute; reflexivity. Qed.

Example count_trunc_fails :
  Ids.make_count 4294967296 =
    Some (Ids.MkCount 4294967296)
  /\ make_trunc Ids.MkCount 4294967296 =
    Some (Ids.MkCount 0).
Proof. repeat split; vm_compute; reflexivity. Qed.

Example pool_off_trunc_fails :
  Ids.make_pool_offset 4294967296 =
    Some (Ids.MkPoolOffset 4294967296)
  /\ make_trunc Ids.MkPoolOffset 4294967296 =
    Some (Ids.MkPoolOffset 0).
Proof. repeat split; vm_compute; reflexivity. Qed.

Print Assumptions pack_as_sum.
Print Assumptions pack_within_signed_cell.
Print Assumptions owner_pack.
Print Assumptions local_pack.
Print Assumptions pack_injective.
Print Assumptions pack_projection_roundtrip.
Print Assumptions projection_pack_roundtrip.
Print Assumptions distinct_valid_owners_separate.
Print Assumptions try_pack_rejects_negative.
Print Assumptions try_pack_rejects_overflow.
Print Assumptions check_rejects_equal_bound.
Print Assumptions check_rejects_foreign_owner.
Print Assumptions count_scalar_id.
Print Assumptions pool_off_scalar_id.
Print Assumptions count_roundtrip.
Print Assumptions pool_off_roundtrip.
