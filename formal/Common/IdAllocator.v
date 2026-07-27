From Stdlib Require Import ZArith List.
From Habu.Common Require Import Ids.

Import ListNotations.
Open Scope Z_scope.
Open Scope bool_scope.

Definition cell_bytes : Z := Ids.cell_bits / 8.

(* [slot] is a cell index, so every represented address is aligned. *)
Record cell : Type := Cell {
  slot : nat;
  cur : Z
}.

Definition addr (c : cell) : Z :=
  Z.of_nat (slot c) * cell_bytes.

Definition aligned (c : cell) : bool :=
  Z.eqb (Z.modulo (addr c) cell_bytes) 0.

Definition serial_ok (n : Z) : bool :=
  Ids.serial_validb n.

(* Zero is the valid initial state but is never an issued serial. *)
Definition cell_ok (c : cell) : bool :=
  (0 <=? cur c) && (cur c <=? Ids.serial_max).

Definition set_cur (c : cell) (n : Z) : cell :=
  Cell (slot c) n.

Inductive cas_out : Type :=
| CasOk (old fresh : Z)
| CasMiss (want seen : Z).

(* This is the atomic transition at its linearization point. *)
Definition cas (c : cell) (want fresh : Z) : cell * cas_out :=
  if Z.eqb (cur c) want then
    (set_cur c fresh, CasOk want fresh)
  else
    (c, CasMiss want (cur c)).

Definition cas_impl : Type :=
  cell -> Z -> Z -> cell * cas_out.

Definition cas_linear (impl : cas_impl) : Prop :=
  forall c want fresh, impl c want fresh = cas c want fresh.

(* The bound check precedes addition, so exhaustion cannot increment or wrap. *)
Definition next_serial (seen : Z) : option Z :=
  if (0 <=? seen) && (seen <? Ids.serial_max) then
    Some (seen + 1)
  else
    None.

Inductive try_out : Type :=
| Issued (serial : Z)
| Retry (want seen : Z)
| Exhausted (seen : Z).

Definition attempt
    (impl : cas_impl) (c : cell) (seen : Z) : cell * try_out :=
  match next_serial seen with
  | None => (c, Exhausted seen)
  | Some fresh =>
      let '(c', out) := impl c seen fresh in
      match out with
      | CasOk _ _ => (c', Issued fresh)
      | CasMiss want actual => (c', Retry want actual)
      end
  end.

Definition tid : Type := nat.
Definition views : Type := list (tid * Z).

Fixpoint view_get (t : tid) (vs : views) : option Z :=
  match vs with
  | [] => None
  | (t', n) :: rest =>
      if Nat.eqb t t' then Some n else view_get t rest
  end.

Fixpoint view_del (t : tid) (vs : views) : views :=
  match vs with
  | [] => []
  | (t', n) :: rest =>
      if Nat.eqb t t' then rest else (t', n) :: view_del t rest
  end.

Definition view_set (t : tid) (n : Z) (vs : views) : views :=
  (t, n) :: view_del t vs.

(* All threads interleave over this one process-wide cell. *)
Record world : Type := World {
  shared : cell;
  observed : views
}.

Inductive action : Type :=
| Observe (t : tid)
| TryCas (t : tid).

Inductive event : Type :=
| Saw (t : tid) (n : Z)
| Tried (t : tid) (out : try_out)
| NoView (t : tid).

Definition step
    (impl : cas_impl) (w : world) (a : action) : world * event :=
  match a with
  | Observe t =>
      let n := cur (shared w) in
      (World (shared w) (view_set t n (observed w)), Saw t n)
  | TryCas t =>
      match view_get t (observed w) with
      | None => (w, NoView t)
      | Some seen =>
          let '(c', out) := attempt impl (shared w) seen in
          (World c' (view_del t (observed w)), Tried t out)
      end
  end.

Fixpoint run
    (impl : cas_impl) (w : world) (xs : list action)
    : world * list event :=
  match xs with
  | [] => (w, [])
  | a :: rest =>
      let '(w', e) := step impl w a in
      let '(w'', es) := run impl w' rest in
      (w'', e :: es)
  end.

Definition pure_attempt : cell -> Z -> cell * try_out :=
  attempt cas.

Definition pure_step : world -> action -> world * event :=
  step cas.

Definition pure_run : world -> list action -> world * list event :=
  run cas.

(* The host operation and its one law form the external atomic boundary. *)
Parameter host_atomic_cas : cas_impl.

Axiom atomic_cas_linearizable :
  cas_linear host_atomic_cas.

Definition host_attempt : cell -> Z -> cell * try_out :=
  attempt host_atomic_cas.

Definition host_step : world -> action -> world * event :=
  step host_atomic_cas.

Definition host_run : world -> list action -> world * list event :=
  run host_atomic_cas.

Example cell_is_aligned :
  aligned (Cell 9 0) = true.
Proof. reflexivity. Qed.

Example zero_is_initial_not_serial :
  cell_ok (Cell 0 0) = true /\ serial_ok 0 = false.
Proof. repeat split; reflexivity. Qed.

Example serial_edges :
  serial_ok 1 = true /\
  serial_ok Ids.serial_max = true /\
  serial_ok (Ids.serial_max + 1) = false.
Proof. repeat split; reflexivity. Qed.

Example bad_cells_reject :
  cell_ok (Cell 0 (-1)) = false /\
  cell_ok (Cell 0 (Ids.serial_max + 1)) = false.
Proof. repeat split; reflexivity. Qed.

Example cas_hit :
  cas (Cell 3 0) 0 1 = (Cell 3 1, CasOk 0 1).
Proof. reflexivity. Qed.

Example cas_miss :
  cas (Cell 3 1) 0 1 = (Cell 3 1, CasMiss 0 1).
Proof. reflexivity. Qed.

Example initial_issue :
  pure_attempt (Cell 3 0) 0 = (Cell 3 1, Issued 1).
Proof. reflexivity. Qed.

Example stale_retry :
  pure_attempt (Cell 3 1) 0 = (Cell 3 1, Retry 0 1).
Proof. reflexivity. Qed.

Example exhausted_before_add :
  next_serial Ids.serial_max = None /\
  pure_attempt (Cell 3 Ids.serial_max) Ids.serial_max =
    (Cell 3 Ids.serial_max, Exhausted Ids.serial_max).
Proof. repeat split; reflexivity. Qed.

Example out_of_domain_has_no_next :
  next_serial (-1) = None /\
  next_serial (Ids.serial_max + 1) = None.
Proof. repeat split; reflexivity. Qed.

Definition two_threads : list action :=
  [Observe 0%nat; Observe 1%nat;
   TryCas 0%nat; TryCas 1%nat;
   Observe 1%nat; TryCas 1%nat].

Example stale_interleaving :
  pure_run (World (Cell 3 0) []) two_threads =
    (World (Cell 3 2) [],
     [Saw 0%nat 0;
      Saw 1%nat 0;
      Tried 0%nat (Issued 1);
      Tried 1%nat (Retry 0 1);
      Saw 1%nat 1;
      Tried 1%nat (Issued 2)]).
Proof. reflexivity. Qed.
