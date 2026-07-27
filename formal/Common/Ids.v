From Stdlib Require Import Bool ZArith.

Open Scope Z_scope.

Definition cell_bits : Z := 64.
Definition cell_value_bits : Z := cell_bits - 1.
Definition serial_bits : Z := 31.
Definition local_bits : Z := 32.

Definition cell_min : Z := -(2 ^ cell_value_bits).
Definition cell_max : Z := 2 ^ cell_value_bits - 1.

Definition serial_min : Z := 1.
Definition serial_max : Z := 2 ^ serial_bits - 1.
Definition local_min : Z := 0.
Definition local_max : Z := 2 ^ local_bits - 1.
Definition local_shift : Z := 32.
Definition local_mask : Z := local_max.

Definition betweenb (lo hi value : Z) : bool :=
  Z.leb lo value && Z.leb value hi.

Definition cell_validb (value : Z) : bool :=
  betweenb cell_min cell_max value.

Definition serial_validb (serial : Z) : bool :=
  betweenb serial_min serial_max serial.

Definition local_validb (local : Z) : bool :=
  betweenb local_min local_max local.

Definition scalar_validb (value : Z) : bool :=
  betweenb 0 cell_max value.

Definition pack (serial local : Z) : Z :=
  Z.lor (Z.shiftl serial local_shift) local.

Definition pack_validb (serial local : Z) : bool :=
  serial_validb serial && local_validb local.

Definition try_pack (serial local : Z) : option Z :=
  if pack_validb serial local
  then Some (pack serial local)
  else None.

Definition owner (raw : Z) : Z :=
  Z.shiftr raw local_shift.

Definition local (raw : Z) : Z :=
  Z.land raw local_mask.

Definition packed_validb (raw : Z) : bool :=
  cell_validb raw
  && serial_validb (owner raw)
  && local_validb (local raw).

Definition checkb (serial bound raw : Z) : bool :=
  serial_validb serial
  && scalar_validb bound
  && packed_validb raw
  && Z.eqb (owner raw) serial
  && Z.ltb (local raw) bound.

(* Separate one-cell structures model nominal NEWTYPE declarations. *)

Structure ir_module_key : Type :=
  MkModuleKey { module_key_raw : Z }.

Structure ir_module_id : Type :=
  MkModuleId { module_id_raw : Z }.

Structure ir_source_id : Type :=
  MkSourceId { source_id_raw : Z }.

Structure ir_fun_id : Type :=
  MkFunId { fun_id_raw : Z }.

Structure ir_block_id : Type :=
  MkBlockId { block_id_raw : Z }.

Structure ir_op_id : Type :=
  MkOpId { op_id_raw : Z }.

Structure ir_value_id : Type :=
  MkValueId { value_id_raw : Z }.

Structure ir_type_id : Type :=
  MkTypeId { type_id_raw : Z }.

Structure ir_attr_id : Type :=
  MkAttrId { attr_id_raw : Z }.

Structure ir_symbol_id : Type :=
  MkSymbolId { symbol_id_raw : Z }.

Structure ir_span_id : Type :=
  MkSpanId { span_id_raw : Z }.

Structure ir_pool_offset : Type :=
  MkPoolOffset { pool_offset_raw : Z }.

Structure ir_count : Type :=
  MkCount { count_raw : Z }.

Definition module_key_validb (key : ir_module_key) : bool :=
  serial_validb (module_key_raw key).

Definition module_id_validb (id : ir_module_id) : bool :=
  serial_validb (module_id_raw id).

Definition source_id_validb (id : ir_source_id) : bool :=
  packed_validb (source_id_raw id).

Definition fun_id_validb (id : ir_fun_id) : bool :=
  packed_validb (fun_id_raw id).

Definition block_id_validb (id : ir_block_id) : bool :=
  packed_validb (block_id_raw id).

Definition op_id_validb (id : ir_op_id) : bool :=
  packed_validb (op_id_raw id).

Definition value_id_validb (id : ir_value_id) : bool :=
  packed_validb (value_id_raw id).

Definition type_id_validb (id : ir_type_id) : bool :=
  packed_validb (type_id_raw id).

Definition attr_id_validb (id : ir_attr_id) : bool :=
  packed_validb (attr_id_raw id).

Definition symbol_id_validb (id : ir_symbol_id) : bool :=
  packed_validb (symbol_id_raw id).

Definition span_id_validb (id : ir_span_id) : bool :=
  packed_validb (span_id_raw id).

Definition pool_offset_validb (offset : ir_pool_offset) : bool :=
  scalar_validb (pool_offset_raw offset).

Definition count_validb (count : ir_count) : bool :=
  scalar_validb (count_raw count).

Definition pack_id {A : Type}
    (make : Z -> A) (key : ir_module_key) (idx : Z) : option A :=
  match try_pack (module_key_raw key) idx with
  | Some raw => Some (make raw)
  | None => None
  end.

Definition id_owner {A : Type}
    (get_raw : A -> Z) (id : A) : ir_module_id :=
  MkModuleId (owner (get_raw id)).

Definition id_local {A : Type}
    (get_raw : A -> Z) (id : A) : Z :=
  local (get_raw id).

Definition id_checkb {A : Type}
    (get_raw : A -> Z)
    (key : ir_module_key) (bound : ir_count) (id : A) : bool :=
  checkb (module_key_raw key) (count_raw bound) (get_raw id).

Definition make_pool_offset (raw : Z) : option ir_pool_offset :=
  if scalar_validb raw then Some (MkPoolOffset raw) else None.

Definition make_count (raw : Z) : option ir_count :=
  if scalar_validb raw then Some (MkCount raw) else None.

Definition pack_source
    (key : ir_module_key) (idx : Z) : option ir_source_id :=
  pack_id MkSourceId key idx.

Definition source_owner (id : ir_source_id) : ir_module_id :=
  id_owner source_id_raw id.

Definition source_local (id : ir_source_id) : Z :=
  id_local source_id_raw id.

Definition source_checkb
    (key : ir_module_key) (bound : ir_count) (id : ir_source_id) : bool :=
  id_checkb source_id_raw key bound id.

Definition pack_fun
    (key : ir_module_key) (idx : Z) : option ir_fun_id :=
  pack_id MkFunId key idx.

Definition fun_owner (id : ir_fun_id) : ir_module_id :=
  id_owner fun_id_raw id.

Definition fun_local (id : ir_fun_id) : Z :=
  id_local fun_id_raw id.

Definition fun_checkb
    (key : ir_module_key) (bound : ir_count) (id : ir_fun_id) : bool :=
  id_checkb fun_id_raw key bound id.

Definition pack_block
    (key : ir_module_key) (idx : Z) : option ir_block_id :=
  pack_id MkBlockId key idx.

Definition block_owner (id : ir_block_id) : ir_module_id :=
  id_owner block_id_raw id.

Definition block_local (id : ir_block_id) : Z :=
  id_local block_id_raw id.

Definition block_checkb
    (key : ir_module_key) (bound : ir_count) (id : ir_block_id) : bool :=
  id_checkb block_id_raw key bound id.

Definition pack_op
    (key : ir_module_key) (idx : Z) : option ir_op_id :=
  pack_id MkOpId key idx.

Definition op_owner (id : ir_op_id) : ir_module_id :=
  id_owner op_id_raw id.

Definition op_local (id : ir_op_id) : Z :=
  id_local op_id_raw id.

Definition op_checkb
    (key : ir_module_key) (bound : ir_count) (id : ir_op_id) : bool :=
  id_checkb op_id_raw key bound id.

Definition pack_value
    (key : ir_module_key) (idx : Z) : option ir_value_id :=
  pack_id MkValueId key idx.

Definition value_owner (id : ir_value_id) : ir_module_id :=
  id_owner value_id_raw id.

Definition value_local (id : ir_value_id) : Z :=
  id_local value_id_raw id.

Definition value_checkb
    (key : ir_module_key) (bound : ir_count) (id : ir_value_id) : bool :=
  id_checkb value_id_raw key bound id.

Definition pack_type
    (key : ir_module_key) (idx : Z) : option ir_type_id :=
  pack_id MkTypeId key idx.

Definition type_owner (id : ir_type_id) : ir_module_id :=
  id_owner type_id_raw id.

Definition type_local (id : ir_type_id) : Z :=
  id_local type_id_raw id.

Definition type_checkb
    (key : ir_module_key) (bound : ir_count) (id : ir_type_id) : bool :=
  id_checkb type_id_raw key bound id.

Definition pack_attr
    (key : ir_module_key) (idx : Z) : option ir_attr_id :=
  pack_id MkAttrId key idx.

Definition attr_owner (id : ir_attr_id) : ir_module_id :=
  id_owner attr_id_raw id.

Definition attr_local (id : ir_attr_id) : Z :=
  id_local attr_id_raw id.

Definition attr_checkb
    (key : ir_module_key) (bound : ir_count) (id : ir_attr_id) : bool :=
  id_checkb attr_id_raw key bound id.

Definition pack_symbol
    (key : ir_module_key) (idx : Z) : option ir_symbol_id :=
  pack_id MkSymbolId key idx.

Definition symbol_owner (id : ir_symbol_id) : ir_module_id :=
  id_owner symbol_id_raw id.

Definition symbol_local (id : ir_symbol_id) : Z :=
  id_local symbol_id_raw id.

Definition symbol_checkb
    (key : ir_module_key) (bound : ir_count) (id : ir_symbol_id) : bool :=
  id_checkb symbol_id_raw key bound id.

Definition pack_span
    (key : ir_module_key) (idx : Z) : option ir_span_id :=
  pack_id MkSpanId key idx.

Definition span_owner (id : ir_span_id) : ir_module_id :=
  id_owner span_id_raw id.

Definition span_local (id : ir_span_id) : Z :=
  id_local span_id_raw id.

Definition span_checkb
    (key : ir_module_key) (bound : ir_count) (id : ir_span_id) : bool :=
  id_checkb span_id_raw key bound id.

Example cell_min_exact :
  cell_min = -9223372036854775808.
Proof. reflexivity. Qed.

Example cell_max_exact :
  cell_max = 9223372036854775807.
Proof. reflexivity. Qed.

Example serial_max_exact :
  serial_max = 2147483647.
Proof. reflexivity. Qed.

Example local_max_exact :
  local_max = 4294967295.
Proof. reflexivity. Qed.

Example width_constants_exact :
  cell_bits = 64
  /\ serial_bits = 31
  /\ local_bits = 32
  /\ local_shift = 32
  /\ local_mask = 4294967295.
Proof. repeat split; vm_compute. Qed.

Example cell_bounds_decide :
  cell_validb cell_min = true
  /\ cell_validb cell_max = true
  /\ cell_validb (cell_min - 1) = false
  /\ cell_validb (cell_max + 1) = false.
Proof. repeat split; vm_compute. Qed.

Example serial_bounds_decide :
  serial_validb serial_min = true
  /\ serial_validb serial_max = true
  /\ serial_validb 0 = false
  /\ serial_validb (serial_max + 1) = false.
Proof. repeat split; vm_compute. Qed.

Example local_bounds_decide :
  local_validb local_min = true
  /\ local_validb local_max = true
  /\ local_validb (-1) = false
  /\ local_validb (local_max + 1) = false.
Proof. repeat split; vm_compute. Qed.

Example scalar_bounds_decide :
  scalar_validb 0 = true
  /\ scalar_validb cell_max = true
  /\ scalar_validb (-1) = false
  /\ scalar_validb (cell_max + 1) = false.
Proof. repeat split; vm_compute. Qed.

Example scalar_constructors_decide :
  make_count 0 = Some (MkCount 0)
  /\ make_count cell_max = Some (MkCount cell_max)
  /\ make_count (-1) = None
  /\ make_pool_offset 0 = Some (MkPoolOffset 0)
  /\ make_pool_offset cell_max = Some (MkPoolOffset cell_max)
  /\ make_pool_offset (cell_max + 1) = None.
Proof. repeat split; vm_compute. Qed.

Example pack_word_bounds :
  pack serial_min local_min = 4294967296
  /\ pack serial_max local_max = cell_max.
Proof. repeat split; vm_compute. Qed.

Example pack_inputs_decide :
  try_pack serial_min local_min = Some 4294967296
  /\ try_pack serial_max local_max = Some cell_max
  /\ try_pack 0 0 = None
  /\ try_pack (serial_max + 1) 0 = None
  /\ try_pack serial_min (-1) = None
  /\ try_pack serial_min (local_max + 1) = None.
Proof. repeat split; vm_compute. Qed.

Example projections_decide :
  owner (pack 7 42) = 7
  /\ local (pack 7 42) = 42
  /\ owner cell_max = serial_max
  /\ local cell_max = local_max.
Proof. repeat split; vm_compute. Qed.

Example packed_values_decide :
  packed_validb (pack serial_min local_min) = true
  /\ packed_validb (pack serial_max local_max) = true
  /\ packed_validb 0 = false
  /\ packed_validb (-1) = false
  /\ packed_validb (cell_max + 1) = false.
Proof. repeat split; vm_compute. Qed.

Example checks_decide :
  checkb 7 43 (pack 7 42) = true
  /\ checkb 7 42 (pack 7 42) = false
  /\ checkb 8 43 (pack 7 42) = false
  /\ checkb 7 43 (pack 0 42) = false.
Proof. repeat split; vm_compute. Qed.

Example typed_source_decide :
  pack_source (MkModuleKey 7) 42 = Some (MkSourceId (pack 7 42))
  /\ source_owner (MkSourceId (pack 7 42)) = MkModuleId 7
  /\ source_local (MkSourceId (pack 7 42)) = 42
  /\ source_checkb
       (MkModuleKey 7) (MkCount 43) (MkSourceId (pack 7 42)) = true.
Proof. repeat split; vm_compute. Qed.

Fail Definition wrong_family_is_static : bool :=
  source_checkb
    (MkModuleKey 7) (MkCount 43) (MkFunId (pack 7 42)).
