(* Habu.Common.Control — a model of the checked language's CONTROL FLOW.

   `Habu.Common.Effects` models types, stacks, unification, and the sequential
   composition of straight-line bodies.  It stops there on purpose.  This file
   picks up exactly where it stops: branches, loops, early return, dead paths,
   recursion, and the APPLICATION of a quotation.

   Ground truth is `src/core/checker.f`, not `docs/effects.md`.  The checker's
   control-flow words are dispatched from `CF-TOK?` (checker.f:8356-8378) and
   live at checker.f:7794-7935; the frame stack `CFS` and its accessors are at
   checker.f:7573-7677; the dead-path gate is `LIVE-TOKEN?` (checker.f:8534);
   the definition boundary is `CHECK` (checker.f:9670-9695).  Every definition
   below names the checker word it models with a line reference, and every
   definitional example at the end names the `.f` fixture that was loaded
   through `bin/hb --load` to confirm it, with the exit status observed.

   WHY A TOKEN MACHINE AND NOT AN ABSTRACT SYNTAX TREE.  The checker is a
   single left-to-right token scan over the body text.  It has no parse tree:
   `if` PUSHES a frame onto `CFS` and `then` POPS one, and the rules that
   decide a program are stated over that frame stack, not over nesting.  Three
   things are only expressible this way, and all three decide real programs:

     - the dead-path rule is a property of the NEXT TOKEN's spelling
       (`DEAD-CLOSE?`, checker.f:8521-8532), not of a subtree;
     - `EXIT` accumulates into ONE per-definition row (`XROW`) no matter how
       deeply it is nested, so every early return in a word meets every other
       one directly, not at the enclosing join;
     - an unbalanced body is rejected by counting open frames at the end
       (`#CFC @ 0 <>`, checker.f:9682), which a tree cannot even express.

   So the state below is the checker's registers — `DCUR`, `RCUR`, `BROW`,
   `RBROW`, `XROW`, `XRROW`, `XSET`, `DEADP`, `CFS`, `OK`, `UNCK` — and `step`
   is `DO-TOK1` restricted to the control tokens.

   TWO REPRESENTATION FACTS CARRIED FORWARD FROM Effects.v, because control
   flow is where they start to bite.

   1. There is no closed stack.  Every row bottoms out in a row variable, so a
      branch join whose arms differ in ARITY fails as a row occurs check, not
      as a nil/cons clash.  This is visible in the checker's own diagnostic:
      `: C4 ( i64 -- i64 ) MK-BOOL IF DUP1 THEN ;` reports
      `at 'THEN' expected: i64 actual: i64 i64`, which is the occurs check on
      the shared implicit row, and `: C32 ( bool -- cell ) IF MK-CELL THEN
      MK-CELL ;` reports `at 'THEN' expected: actual: cell`.

   2. The implicit-row seal is POST-HOC.  Nothing in this file consults a
      rigidity flag.  A branch arm, a loop body, or a quotation may bind the
      declared base row freely while it is being scanned; `CHECK-NO-BORROW`
      only asks, at the very end, whether that row still resolves to a row.
      `finish` below therefore reuses `Effects.seal_okb` unchanged, and the
      control constructs neither strengthen nor weaken it.  The one place this
      shows up is a loop: `CF-UNTIL` unifies the row at `until` against the row
      at `begin`, and if the body underflowed, BOTH rows carry the borrowed
      cells, so the loop rule is silent and the seal is what rejects.

   MODELLED FRAGMENT.  `if` / `else` / `then`, `begin` / `until`,
   `begin` / `again`, `begin` / `while` / `repeat`, `exit`, `recurse`,
   `[:` / `;]`, `execute`, `catch`, and an ordinary call.

   DELIBERATE OMISSIONS, each of which the checker also decides and each of
   which a soundness proof built on this file would therefore not cover:
     - `do` / `?do` / `loop` / `+loop` / `i` / `j` / `leave` / `unloop`
       (CFS kind 5, checker.f:7922-7990) — a second loop shape with its own
       zero-trip rule;
     - `case` / `of` / `endof` / `endcase` (kinds 7 and 8, checker.f:7801-7848);
     - the `MATCH` eliminator (kinds 9 and 10, checker.f:8258-8330), which is
       the ONLY construct that raises the `MD-JOIN` diagnostic;
     - the exceptional edge: `throw`, `die`, `THROW-EDGE`, and the
       `Q>XHAS` / `Q>XDEAD` metadata a quotation carries beside its four rows;
     - `uniform<bool>` block-uniform branches (`COND-UNIFORM?`,
       checker.f:7784-7792), which need arity-1 type families;
     - the linear-once conservation pass (`LIN-CHECK`) and branch-scoped
       locals (`CF-LOC-REST`), which are bookkeeping this fragment drops.

   FAIL-CLOSED, AS IN Effects.v — with two named exceptions.  Every function
   here is total and returns a value, and unification that runs out of fuel
   reports failure, so on those counts the model can only reject more programs
   than the checker.  But two divergences run the OTHER way and are recorded
   here rather than buried:

     - `CF-PUSH` turns a 33rd open control frame into `UNCK`
       (checker.f:7652), so deeply nested control is UNCHECKABLE.  The frame
       list below is unbounded, so the model certifies bodies the checker
       refuses to certify.
     - a quotation whose every path leaves early carries `Q>XDEAD`, and
       `RSEXEC` then KILLS the caller's path instead of installing the
       quotation's output rows (checker.f:2019).  `Effects.ty`'s `TQuot`
       carries four rows and nothing else, so that flag cannot be represented
       without changing `Effects.v`; `do_exec` below always installs the output
       rows.  For such a quotation the model continues live where the checker
       goes dead, and so may accept a body the checker rejects.  Closing this
       needs a quotation type that carries its control flags. *)

From Stdlib Require Import Bool List PeanoNat.
Import ListNotations.
From Habu.Common Require Import Effects.

(* ------------------------------------------------------------------ *)
(* Tokens.                                                             *)
(*                                                                     *)
(* `CF-TOK?` (checker.f:8356-8378) matches the control spellings; every *)
(* other token that carries an effect goes through `EFF-APPLY`, which  *)
(* `Effects.compose` already models.  `TCall` is that case.            *)
(* ------------------------------------------------------------------ *)

Inductive tok : Type :=
  | TCall : word_eff -> tok   (* an ordinary word: EFF-APPLY, checker.f:4629 *)
  | TIf                       (* CF-IF,      checker.f:7794 *)
  | TElse                     (* CF-ELSE,    checker.f:7849 *)
  | TThen                     (* CF-THEN,    checker.f:7877 *)
  | TBegin                    (* CF-BEGIN,   checker.f:7895 *)
  | TUntil                    (* CF-UNTIL,   checker.f:7898 *)
  | TAgain                    (* CF-AGAIN,   checker.f:7904 *)
  | TWhile                    (* CF-WHILE,   checker.f:7909 *)
  | TRepeat                   (* CF-REPEAT,  checker.f:7917 *)
  | TExit                     (* CF-EXIT,    checker.f:7887 *)
  | TRecurse                  (* CF-RECURSE, checker.f:7768 *)
  | TOpenQ                    (* CF-QUOT  `[:`, checker.f:7971 *)
  | TCloseQ                   (* CF-SEMIQ `;]`, checker.f:7987 *)
  | TExec                     (* RSEXEC,   checker.f:2003 *)
  | TCatch.                   (* RSCATCH,  checker.f:2039 *)

(* ------------------------------------------------------------------ *)
(* A control frame (`CFS`, checker.f:7573-7597).                       *)
(*                                                                     *)
(* Frame KINDS in the modelled fragment: 1 = `if` with no `else` seen  *)
(* yet, 2 = `if`/`else`, 3 = `begin`, 4 = `begin`/`while`, 6 = `[:`.   *)
(* Kinds 5 (do), 7/8 (case/of) and 9/10 (match) are omitted above.     *)
(*                                                                     *)
(* `CF-PUSH` (checker.f:7651) writes only KND, SA, SB, RA, RB and the  *)
(* locals mark; it leaves DED and the four quotation save slots at      *)
(* whatever the reused `CFS` cell happened to hold.  Every read of      *)
(* those slots is preceded by a write from the construct that owns them *)
(* (`CF-ELSE` writes DED, `CF-QUOT` writes the save slots), so any      *)
(* initial value is faithful; the pushers below use `false` and the     *)
(* pushed row.                                                          *)
(* ------------------------------------------------------------------ *)

Record frame : Type := MkFrame {
  fr_knd : nat;
  fr_sa  : stack;     (* CF.SA: the data row the frame was opened at *)
  fr_sb  : stack;     (* CF.SB: written by CF-ELSE / CF-WHILE *)
  fr_ra  : stack;     (* CF.RA *)
  fr_rb  : stack;     (* CF.RB *)
  fr_ded : bool;      (* CF.DED: the if-arm's deadness, saved by CF-ELSE *)
  fr_xro : stack;     (* CF.XRO \                                        *)
  fr_xrr : stack;     (* CF.XRR  |  outer early-return state, saved by   *)
  fr_xst : bool;      (* CF.XST  |  CF-QUOT and restored by CF-SEMIQ     *)
  fr_xdp : bool       (* CF.XDP /                                        *)
}.

(* ------------------------------------------------------------------ *)
(* The checker's registers while it scans one definition.              *)
(*                                                                     *)
(* `DCUR`/`RCUR` are the current data and return rows.  `BROW`/`RBROW` *)
(* are the base rows of the CURRENT scope: `NEW` (checker.f:1706-1714) *)
(* allocates them fresh, `CHECK-SCAN` then points DCUR at the parsed    *)
(* signature input but leaves BROW alone, and `CF-QUOT` re-points both  *)
(* at fresh rows for the quotation body.  `XROW`/`XRROW`/`XSET` are the *)
(* early-return accumulator; `DEADP` says the current linear path is    *)
(* terminated.  `st_fv` is `FV` (checker.f:1687-1690), the single       *)
(* counter both type and row variables are drawn from.                  *)
(* ------------------------------------------------------------------ *)

Record st : Type := MkSt {
  st_sub   : subst;
  st_fv    : nat;
  st_dcur  : stack;
  st_rcur  : stack;
  st_brow  : stack;
  st_rbrow : stack;
  st_xset  : bool;
  st_xrow  : stack;
  st_xrrow : stack;
  st_dead  : bool;
  st_cfs   : list frame;   (* head = top of CFS *)
  st_ok    : bool;         (* OK *)
  st_unck  : bool          (* UNCK *)
}.

(* Field updates, written out one per field so a reader can check them by
   eye against the record above. *)

Definition put_sub (s : st) (x : subst) : st :=
  MkSt x (st_fv s) (st_dcur s) (st_rcur s) (st_brow s) (st_rbrow s)
       (st_xset s) (st_xrow s) (st_xrrow s) (st_dead s) (st_cfs s)
       (st_ok s) (st_unck s).

Definition put_fv (s : st) (n : nat) : st :=
  MkSt (st_sub s) n (st_dcur s) (st_rcur s) (st_brow s) (st_rbrow s)
       (st_xset s) (st_xrow s) (st_xrrow s) (st_dead s) (st_cfs s)
       (st_ok s) (st_unck s).

Definition put_d (s : st) (d : stack) : st :=
  MkSt (st_sub s) (st_fv s) d (st_rcur s) (st_brow s) (st_rbrow s)
       (st_xset s) (st_xrow s) (st_xrrow s) (st_dead s) (st_cfs s)
       (st_ok s) (st_unck s).

Definition put_r (s : st) (r : stack) : st :=
  MkSt (st_sub s) (st_fv s) (st_dcur s) r (st_brow s) (st_rbrow s)
       (st_xset s) (st_xrow s) (st_xrrow s) (st_dead s) (st_cfs s)
       (st_ok s) (st_unck s).

Definition put_dr (s : st) (d r : stack) : st :=
  MkSt (st_sub s) (st_fv s) d r (st_brow s) (st_rbrow s)
       (st_xset s) (st_xrow s) (st_xrrow s) (st_dead s) (st_cfs s)
       (st_ok s) (st_unck s).

Definition put_base (s : st) (b rb : stack) : st :=
  MkSt (st_sub s) (st_fv s) (st_dcur s) (st_rcur s) b rb
       (st_xset s) (st_xrow s) (st_xrrow s) (st_dead s) (st_cfs s)
       (st_ok s) (st_unck s).

Definition put_x (s : st) (xset : bool) (xrow xrrow : stack) : st :=
  MkSt (st_sub s) (st_fv s) (st_dcur s) (st_rcur s) (st_brow s) (st_rbrow s)
       xset xrow xrrow (st_dead s) (st_cfs s)
       (st_ok s) (st_unck s).

Definition put_dead (s : st) (b : bool) : st :=
  MkSt (st_sub s) (st_fv s) (st_dcur s) (st_rcur s) (st_brow s) (st_rbrow s)
       (st_xset s) (st_xrow s) (st_xrrow s) b (st_cfs s)
       (st_ok s) (st_unck s).

Definition put_cfs (s : st) (l : list frame) : st :=
  MkSt (st_sub s) (st_fv s) (st_dcur s) (st_rcur s) (st_brow s) (st_rbrow s)
       (st_xset s) (st_xrow s) (st_xrrow s) (st_dead s) l
       (st_ok s) (st_unck s).

(* `CF-FAIL` (checker.f:7689) and every `0 OK !` site: OK is a LATCH.  The
   checker keeps scanning after a failure, so this must not stop the machine. *)
Definition fail (s : st) : st :=
  MkSt (st_sub s) (st_fv s) (st_dcur s) (st_rcur s) (st_brow s) (st_rbrow s)
       (st_xset s) (st_xrow s) (st_xrrow s) (st_dead s) (st_cfs s)
       false (st_unck s).

(* `-1 UNCK !`: uncheckable, which `CHECK-VERDICT` ranks ABOVE a plain OK=0
   (checker.f:9666-9668). *)
Definition set_unck (s : st) : st :=
  MkSt (st_sub s) (st_fv s) (st_dcur s) (st_rcur s) (st_brow s) (st_rbrow s)
       (st_xset s) (st_xrow s) (st_xrrow s) (st_dead s) (st_cfs s)
       (st_ok s) true.

(* --- fresh variables (`FRESH`, checker.f:1687) ---------------------- *)

Definition fresh_id (s : st) : nat * st := (st_fv s, put_fv s (S (st_fv s))).

(* --- unification against a register --------------------------------- *)

(* One deliberate divergence, recorded here because it is the only place the
   model's STATE can differ from the checker's after a failure: the checker's
   `UNIFY` is destructive and leaves its partial bindings behind on failure,
   while `Effects.unify_stack` returns `None` and this keeps the old
   substitution.  OK is already latched false at that point, so the VERDICT is
   unaffected; only the bindings a later token would see differ. *)
Definition uni (k : ukind) (s : st) (got want : stack) : st :=
  match unify_stack k (st_sub s) got want with
  | Some s' => put_sub s s'
  | None => fail s
  end.

(* `SUNI` / `RSUNI` (checker.f:7699, 7713).  Both call bare `UNIFY`, and
   `UNIFY-IN` / `UNIFY-COERCE` restore `UK-EXACT` when they return
   (checker.f:1676-1683), so EVERY branch join, loop back-edge and early-return
   join in the checker runs at `UK-EXACT`: no integer widening, no field
   coercion.  Only the definition boundary coerces.  Measured: a word whose two
   arms produce `u8` and `cell` is rejected, while a straight-line word that
   produces `u8` for a declared `cell` output certifies. *)
Definition suni (s : st) (want : stack) : st := uni UkExact s (st_dcur s) want.
Definition rsuni (s : st) (want : stack) : st := uni UkExact s (st_rcur s) want.

(* `SUNI-IN` / `RSUNI-IN` (checker.f:7704, 7715). *)
Definition suni_in (s : st) (want : stack) : st := uni UkInput s (st_dcur s) want.
Definition rsuni_in (s : st) (want : stack) : st := uni UkInput s (st_rcur s) want.

(* --- primitive steps ------------------------------------------------ *)

(* `STEP-TYPE-IN` (checker.f:3231) through `CHECKER-STEP` (checker.f:1928):
   allocate a fresh tail, unify the current row against `t` over that tail at
   `UK-INPUT`, then REPLACE the current row with the tail.  The replacement is
   unconditional — the checker does it even when the unification failed. *)
Definition step_ty_in (s : st) (t : ty) : st :=
  let (r, s) := fresh_id s in
  let rest := SRow r in
  let s := uni UkInput s (st_dcur s) (SPush t rest) in
  put_d s rest.

(* `STEP-BOOL-IN` (checker.f:3251).  `bool` is not in the integer class, so
   `INT-WIDENS?` never admits `n` or `i64` here: a flag must already be a
   `bool`.  Measured: `: C33 ( i64 -- i64 ) IF STEP1 THEN ;` is rejected with
   `at 'IF' expected: i64 bool actual: i64`. *)
Definition step_bool_in (s : st) : st := step_ty_in s boolt.

(* An ordinary call: `EFF-APPLY` (checker.f:4629-4637), which is exactly
   `Effects.compose` in register form — unify the current row against the
   freshly instantiated declared input at `UK-INPUT`, replace it with the
   declared output, and touch the return row only if the callee's signature
   actually wrote a `|` clause. *)
Definition apply_eff (s : st) (w : word_eff) : st :=
  let above := st_fv s in
  let w' := instantiate above w in
  let s := put_fv s (above + next_eff (we_eff w)) in
  let s := suni_in s (we_din w') in
  let s := put_d s (we_dout w') in
  if we_hasr w'
  then let s := rsuni_in s (we_rin w') in put_r s (we_rout w')
  else s.

(* ------------------------------------------------------------------ *)
(* The dead-path gate.                                                 *)
(*                                                                     *)
(* `LIVE-TOKEN?` (checker.f:8534-8536) and `DEAD-CLOSE?`               *)
(* (checker.f:8521-8532).  While `DEADP` is set, only a structural      *)
(* closer may appear; anything else sets `DEADERR` and `OK = 0`, and    *)
(* the token is NOT processed at all (`DO-TOK1`, checker.f:9068).       *)
(*                                                                     *)
(* Note what is and is not a closer.  `else then loop +loop endof       *)
(* endcase repeat again ;]` are; `until` and `while` are NOT.  So       *)
(* `: C18 ( i64 -- i64 ) BEGIN EXIT UNTIL ;` is rejected, at `UNTIL`,   *)
(* for being dead code — not for anything about the loop.               *)
(*                                                                     *)
(* `DEAD-OWNER!` (checker.f:9079) is NOT this rule.  It only records    *)
(* which token killed the path so the message can say `after 'EXIT'`;   *)
(* the decision is `LIVE-TOKEN?`.  Measured: `: C16 ( i64 -- i64 ) EXIT *)
(* STEP1 ;` reports `at 'STEP1' after 'EXIT'`.                          *)
(* ------------------------------------------------------------------ *)

Definition dead_closer (t : tok) : bool :=
  match t with
  | TElse | TThen | TRepeat | TAgain | TCloseQ => true
  | _ => false
  end.

(* ------------------------------------------------------------------ *)
(* The declaration under check.                                        *)
(*                                                                     *)
(* `cfg_sig` is `CHECK-SIG?` (checker.f:9451): a definition whose text  *)
(* carried a `( ... )` signature right after its name.  It gates three  *)
(* things at once — the seal, the output join, and whether `RECURSE`    *)
(* has a cached effect to instantiate (`RECURSE-CACHE?`,                *)
(* checker.f:7752-7756).  Without it, `CF-RECURSE` sets `UNCK`.         *)
(*                                                                     *)
(* `cfg_brow` is the row `NEW` allocates for `BROW` before the          *)
(* signature is parsed.  It is distinct from the signature's implicit   *)
(* base row (`decl_dbase`), and `CHECK-SCAN` never re-points it.        *)
(* ------------------------------------------------------------------ *)

Record cfg : Type := MkCfg {
  cfg_decl : decl;
  cfg_brow : rowvar;
  cfg_sig  : bool
}.

(* ------------------------------------------------------------------ *)
(* The step relation.                                                  *)
(* ------------------------------------------------------------------ *)

(* `CF-IF`, checker.f:7794.  Consume the flag, THEN push the frame, so the
   frame records the row the two arms both start from.  (The `uniform<bool>`
   arm and the `CF.UNI` mark are the omitted block-uniform path.) *)
Definition do_if (s : st) : st :=
  let s := step_bool_in s in
  put_cfs s (MkFrame 1 (st_dcur s) (st_dcur s) (st_rcur s) (st_rcur s)
                     false (SRow 0) (SRow 0) false false :: st_cfs s).

(* `CF-ELSE`, checker.f:7849.  Save the if-arm's ROW into SB/RB and its
   DEADNESS into DED, then rewind the registers to the frame's entry row so the
   else arm starts where the if arm did.  The frame becomes kind 2. *)
Definition do_else (s : st) : st :=
  match st_cfs s with
  | f :: rest =>
      if Nat.eqb (fr_knd f) 1
      then
        let f' := MkFrame 2 (fr_sa f) (st_dcur s) (fr_ra f) (st_rcur s)
                          (st_dead s) (fr_xro f) (fr_xrr f) (fr_xst f) (fr_xdp f) in
        let s := put_cfs s (f' :: rest) in
        let s := put_dr s (fr_sa f) (fr_ra f) in
        put_dead s false
      else fail s
  | [] => fail s
  end.

(* `CF-THEN-ELSE-MERGE`, checker.f:7860.  THIS is the branch join, and its
   shape is the point of the whole file.

   Both arms live  -> the rows must UNIFY, at `UK-EXACT`.
   One arm dead    -> the join is SKIPPED and the live arm's row wins outright.
   Both arms dead  -> there is no normal continuation; the path stays dead.

   The middle case is not a detail.  `: C15 ( bool -- cell ) IF MK-CELL EXIT
   THEN MK-CELL ;` certifies precisely because the arm that ended in `EXIT`
   never meets the fall-through row at `THEN` — and the two rows differ by a
   whole cell, so a join would have failed the occurs check.  Removing the
   `EXIT` (`: C32 ... IF MK-CELL THEN MK-CELL ;`) rejects.

   One correction to how this join is usually described.  `MD-JOIN`
   (checker.f:8165) is NOT its diagnostic.  `MD-JOIN` is latched in exactly one
   place, `MATCH-ACCUM` (checker.f:8278), and `render.f:422` maps it to
   `E-MATCH-BRANCH-JOIN` with the prose "bad match: branch output mismatch".
   An `if`/`else`/`then` disagreement raises no reason code at all: it is an
   ordinary unification mismatch pinned on the `THEN` token. *)
Definition merge_else (s : st) (f : frame) : st :=
  if st_dead s
  then (if fr_ded f
        then put_dead s true
        else put_dead (put_dr s (fr_sb f) (fr_rb f)) false)
  else (if fr_ded f
        then put_dead s false
        else let s := suni s (fr_sb f) in
             let s := rsuni s (fr_rb f) in
             put_dead s false).

(* `CF-THEN`, checker.f:7877.  Kind 1 is `if` with no `else`: the missing arm
   is the frame's entry row, so an `if` without an `else` must be STACK
   NEUTRAL.  Kind 2 merges as above.  Any other frame kind fails WITHOUT
   popping, exactly as `CF-FAIL` does. *)
Definition do_then (s : st) : st :=
  match st_cfs s with
  | f :: rest =>
      if Nat.eqb (fr_knd f) 1
      then
        let s := if st_dead s
                 then put_dead (put_dr s (fr_sa f) (fr_ra f)) false
                 else let s := suni s (fr_sa f) in rsuni s (fr_ra f) in
        put_cfs s rest
      else if Nat.eqb (fr_knd f) 2
      then put_cfs (merge_else s f) rest
      else fail s
  | [] => fail s
  end.

(* `CF-BEGIN`, checker.f:7895. *)
Definition do_begin (s : st) : st :=
  put_cfs s (MkFrame 3 (st_dcur s) (st_dcur s) (st_rcur s) (st_rcur s)
                     false (SRow 0) (SRow 0) false false :: st_cfs s).

(* `CF-UNTIL`, checker.f:7898.  The flag is consumed BEFORE the frame is
   inspected, so a stray `until` still eats a `bool`.  Then the back edge:
   unify the row at `until` against the row at `begin` — the body, INCLUDING
   the code that produced the flag, must be stack neutral — and continue from
   the `begin` row.

   Note how this interacts with the post-hoc seal.  If the body reached below
   the declared inputs, both the `begin` row and the `until` row carry the
   borrowed cells, so they unify happily; the definition is rejected later by
   `CHECK-NO-BORROW`, not here. *)
Definition do_until (s : st) : st :=
  let s := step_bool_in s in
  match st_cfs s with
  | f :: rest =>
      if Nat.eqb (fr_knd f) 3
      then
        let s := suni s (fr_sa f) in
        let s := put_d s (fr_sa f) in
        let s := rsuni s (fr_ra f) in
        let s := put_r s (fr_ra f) in
        put_cfs s rest
      else fail s
  | [] => fail s
  end.

(* `CF-AGAIN`, checker.f:7904.  Same back edge, no flag, and the path after it
   is dead because the loop never falls out. *)
Definition do_again (s : st) : st :=
  match st_cfs s with
  | f :: rest =>
      if Nat.eqb (fr_knd f) 3
      then
        let s := suni s (fr_sa f) in
        let s := put_d s (fr_sa f) in
        let s := rsuni s (fr_ra f) in
        let s := put_r s (fr_ra f) in
        put_dead (put_cfs s rest) true
      else fail s
  | [] => fail s
  end.

(* `CF-WHILE`, checker.f:7909.  Consume the flag and RECORD the row at that
   point in SB/RB; the frame becomes kind 4 and stays open. *)
Definition do_while (s : st) : st :=
  let s := step_bool_in s in
  match st_cfs s with
  | f :: rest =>
      if Nat.eqb (fr_knd f) 3
      then put_cfs s (MkFrame 4 (fr_sa f) (st_dcur s) (fr_ra f) (st_rcur s)
                              (fr_ded f) (fr_xro f) (fr_xrr f) (fr_xst f) (fr_xdp f)
                      :: rest)
      else fail s
  | [] => fail s
  end.

(* `CF-REPEAT`, checker.f:7917.  Two DIFFERENT rows are in play and conflating
   them is the easy mistake: the back edge is checked against the `begin` row
   (SA), and execution continues from the `while` row (SB).

   So "loop bodies must be stack-neutral" is true only of the whole cycle
   `begin`..`repeat`.  The segment BEFORE `while` may change the stack, as long
   as the segment after it changes it back.  Measured: `: C11 ( i64 -- ) BEGIN
   DROP1 MK-BOOL WHILE MK-I64 REPEAT ;` certifies — it consumes an `i64` before
   the test and produces one after it, and the loop EXITS with the `i64` gone. *)
Definition do_repeat (s : st) : st :=
  match st_cfs s with
  | f :: rest =>
      if Nat.eqb (fr_knd f) 4
      then
        let s := suni s (fr_sa f) in
        let s := put_d s (fr_sb f) in
        let s := rsuni s (fr_ra f) in
        let s := put_r s (fr_rb f) in
        put_cfs s rest
      else fail s
  | [] => fail s
  end.

(* `CF-EXIT`, checker.f:7887.  An early return does NOT meet the declared
   output here.  It meets the OTHER early returns, in ONE per-definition
   accumulator, at `UK-EXACT`; the fall-through row joins the same accumulator
   at `;` (`CHECK-FOLD-EXITS`); and only the joined result is coerced against
   the declaration.  The accumulator is per-DEFINITION, not per-frame, so two
   `exit`s in unrelated branches still meet each other directly. *)
Definition do_exit (s : st) : st :=
  let s := if st_xset s
           then let s := uni UkExact s (st_dcur s) (st_xrow s) in
                uni UkExact s (st_rcur s) (st_xrrow s)
           else put_x s true (st_dcur s) (st_rcur s) in
  put_dead s true.

(* `CF-RECURSE` / `CF-RECURSE-EFF`, checker.f:7758-7770.  With a cached
   signature this is an ordinary call to the word's own declared effect,
   freshly instantiated.  Without one it is UNCHECKABLE, not a reject: the
   `RECURSE-CACHE?` guard falls through to `-1 UNCK !`. *)
Definition do_recurse (c : cfg) (s : st) : st :=
  if cfg_sig c then apply_eff s (decl_eff (cfg_decl c)) else set_unck s.

(* `CF-QUOT`, checker.f:7971.  `[:` opens a nested inference: save the outer
   rows, the outer BASE rows, and the outer EARLY-RETURN state, then start the
   quotation body on two fresh rows with a cleared accumulator.

   Clearing `XSET` and `DEADP` is what makes `exit` QUOTATION-SCOPED: an `exit`
   inside `[: ... ;]` returns from the quotation, contributes to the
   quotation's own effect, and leaves the enclosing definition live.
   Measured: `: C23 ( -- i64 ) [: MK-I64 EXIT ;] execute ;` certifies. *)
Definition do_quot (s : st) : st :=
  let f := MkFrame 6 (st_dcur s) (st_brow s) (st_rcur s) (st_rbrow s)
                   false (st_xrow s) (st_xrrow s) (st_xset s) (st_dead s) in
  let s := put_cfs s (f :: st_cfs s) in
  let (b, s) := fresh_id s in
  let (r, s) := fresh_id s in
  let s := put_base s (SRow b) (SRow r) in
  let s := put_dr s (SRow b) (SRow r) in
  let s := put_x s false (st_xrow s) (st_xrrow s) in
  put_dead s false.

(* `CHECK-FOLD-EXITS`, checker.f:9441, and the identical fold inside
   `CF-SEMIQ`: if any `exit` fired, either the fall-through joins the
   accumulator, or — when every path exited — the accumulator IS the output. *)
Definition fold_exits (s : st) : st :=
  if st_xset s
  then (if st_dead s
        then put_dr s (st_xrow s) (st_xrrow s)
        else let s := uni UkExact s (st_dcur s) (st_xrow s) in
             uni UkExact s (st_rcur s) (st_xrrow s))
  else s.

(* `CF-SEMIQ`, checker.f:7987.  Fold the quotation's own exits, build the
   four-row quotation type from its base and current rows, restore everything
   the frame saved, and push the quotation onto the OUTER data row.

   Omitted here and worth naming: `QX!` also records whether the quotation has
   a throw edge (`Q>XHAS`) and whether it has no normal return (`Q>XDEAD`).
   Those live beside the four rows, not inside `Effects.ty`, so this model
   cannot carry them, and `TExec` below therefore only handles a quotation with
   a normal return. *)
Definition do_semiq (s : st) : st :=
  match st_cfs s with
  | f :: rest =>
      if Nat.eqb (fr_knd f) 6
      then
        let s := fold_exits s in
        let q := TQuot (Eff (st_brow s) (st_dcur s) (st_rbrow s) (st_rcur s)) in
        let s := put_x s (fr_xst f) (fr_xro f) (fr_xrr f) in
        let s := put_dead s (fr_xdp f) in
        let s := put_base s (fr_sb f) (fr_rb f) in
        let s := put_dr s (SPush q (fr_sa f)) (fr_ra f) in
        put_cfs s rest
      else fail s
  | [] => fail s
  end.

(* Pop an execution token off the data row, at `UK-EXACT`, and resolve it.
   Shared by `RSEXEC` (checker.f:2003) and `RSCATCH` (checker.f:2039). *)
Definition pop_xt (s : st) : ty * st :=
  let (v, s) := fresh_id s in
  let (r, s) := fresh_id s in
  let rest := SRow r in
  let s := uni UkExact s (st_dcur s) (SPush (TVar v) rest) in
  let s := put_d s rest in
  (resolve_ty (st_sub s) (TVar v), s).

(* `RSEXEC`, checker.f:2003 — quotation APPLICATION, the piece `Effects.v`
   modelled the type of but never used.  The quotation's declared rows meet the
   live rows at `UK-INPUT` (so an argument may widen into the quotation, just
   as at any call), and the live rows are then REPLACED by the quotation's
   outputs.

   The failure arm is worth its own name.  When the popped value resolves to a
   bare type variable, it is an execution token of unknown provenance — one
   fetched from untyped memory — and executing it would launder whatever it
   really does past the checker.  The checker rejects it with a dedicated
   diagnostic (`MD-EXEC-OPAQUE`, checker.f:8171).  Both that arm and the
   "not a quotation at all" arm set `OK = 0`, so they are one case here. *)
Definition do_exec (s : st) : st :=
  let (t, s) := pop_xt s in
  match t with
  | TQuot (Eff qd qo qr qro) =>
      let s := suni_in s qd in
      let s := rsuni_in s qr in
      put_dr s qo qro
  | _ => fail s
  end.

(* `RSCATCH`, checker.f:2039.  The same application, but the quotation must be
   STACK PRESERVING: its outputs are unified against the live rows instead of
   replacing them.  A throw code (`n`) is then pushed. *)
Definition do_catch (s : st) : st :=
  let (t, s) := pop_xt s in
  match t with
  | TQuot (Eff qd qo qr qro) =>
      let s := suni_in s qd in
      let s := rsuni_in s qr in
      let s := suni_in s qo in
      let s := rsuni_in s qro in
      put_d s (SPush nt (st_dcur s))
  | _ => fail s
  end.

Definition step_live (c : cfg) (s : st) (t : tok) : st :=
  match t with
  | TCall w => apply_eff s w
  | TIf => do_if s
  | TElse => do_else s
  | TThen => do_then s
  | TBegin => do_begin s
  | TUntil => do_until s
  | TAgain => do_again s
  | TWhile => do_while s
  | TRepeat => do_repeat s
  | TExit => do_exit s
  | TRecurse => do_recurse c s
  | TOpenQ => do_quot s
  | TCloseQ => do_semiq s
  | TExec => do_exec s
  | TCatch => do_catch s
  end.

(* `DO-TOK1`, checker.f:9068: a token that is not allowed on a dead path is
   rejected and NOT processed. *)
Definition step (c : cfg) (s : st) (t : tok) : st :=
  if st_dead s && negb (dead_closer t) then fail s else step_live c s t.

Fixpoint run (c : cfg) (s : st) (ts : list tok) : st :=
  match ts with
  | [] => s
  | t :: rest => run c (step c s t) rest
  end.

(* ------------------------------------------------------------------ *)
(* Entry and exit.                                                     *)
(* ------------------------------------------------------------------ *)

(* `NEW` (checker.f:1706) then `CHECK-SCAN`'s signature arm (checker.f:9415):
   BROW and RBROW are allocated fresh, then DCUR is pointed at the parsed input
   row — and BROW is NOT.  With no signature, DCUR stays at BROW.  RCUR keeps
   RBROW unless the signature wrote a `|` clause. *)
Definition init (c : cfg) : st :=
  let d := cfg_decl c in
  let e := decl_eff d in
  let base :=
    S (Nat.max (max_eff (we_eff e))
               (Nat.max (decl_dbase d) (Nat.max (decl_rbrow d) (cfg_brow c)))) in
  MkSt empty_subst base
       (if cfg_sig c then we_din e else SRow (cfg_brow c))
       (if cfg_sig c && we_hasr e then we_rin e else SRow (decl_rbrow d))
       (SRow (cfg_brow c)) (SRow (decl_rbrow d))
       false (SRow 0) (SRow 0)   (* XROW/XRROW are never read while XSET is 0 *)
       false [] true false.

(* `CHECK-VERDICT`, checker.f:9666-9668.  Three outcomes, and UNCHECKABLE
   outranks a plain `OK = 0` — only the hard structural latches this fragment
   omits (`SGBAD`, `MREJ`, `UNSAFE`, ...) can force a reject past it. *)
Inductive verdict : Type := VCert | VUncheckable | VReject.

Definition verdict_of (s : st) : verdict :=
  if st_unck s then VUncheckable else if st_ok s then VCert else VReject.

Definition certifiedb (v : verdict) : bool :=
  match v with VCert => true | _ => false end.

(* `CHECK`, checker.f:9670-9695, in its own order:

     1. fold the early returns into the output row;
     2. `CHECK-NO-BORROW` — the implicit-row seal, unchanged from Effects.v;
     3. join the reached output against the declared one, at `UK-COERCE`;
     4. reject if any control frame is still open;
     5. return-row balance, or the declared return output at `UK-COERCE`.

   Effects.v's `check_body` cites `CHECK-DOES!` (checker.f:10126-10133) and
   runs the return balance BEFORE the output join.  `CHECK` runs it after, with
   the open-frame test in between.  Every one of these is a conjunctive latch on
   `OK`, so the ORDER cannot change a verdict — only which token a diagnostic
   is pinned to.  The order below is `CHECK`'s. *)
Definition finish (c : cfg) (s : st) : verdict :=
  let d := cfg_decl c in
  let e := decl_eff d in
  let s := fold_exits s in
  let s := if cfg_sig c
           then (if seal_okb (st_sub s) d then s else fail s)
           else s in
  let s := if cfg_sig c then uni UkCoerce s (st_dcur s) (we_dout e) else s in
  let s := match st_cfs s with [] => s | _ :: _ => fail s end in
  let s := if we_hasr e
           then (if cfg_sig c then uni UkCoerce s (st_rcur s) (we_rout e) else s)
           else (if stack_eqb (resolve_row (st_sub s) (st_rcur s))
                              (resolve_row (st_sub s) (SRow (decl_rbrow d)))
                 then s else fail s) in
  verdict_of s.

Definition check_ctl (c : cfg) (ts : list tok) : verdict :=
  finish c (run c (init c) ts).

(* ------------------------------------------------------------------ *)
(* Definitional examples.                                              *)
(*                                                                     *)
(* Every example below was cross-checked against the shipped checker.   *)
(* The `.f` text in each comment was loaded with                        *)
(*                                                                     *)
(*   HB_TMP=<scratch> bin/hb --load <fixture>.f ; echo $?              *)
(*                                                                     *)
(* on top of this common prelude, which certifies (exit 0):             *)
(*                                                                     *)
(*   : STEP1   ( i64 -- i64 )      ;                                    *)
(*   : MK-U8   ( -- u8 )         0 ;                                    *)
(*   : MK-CELL ( -- cell )       0 ;                                    *)
(*   : MK-I64  ( -- i64 )        0 ;                                    *)
(*   : MK-BOOL ( -- bool )  0 0< ;                                      *)
(*   : DUP1    ( i64 -- i64 i64 ) dup ;                                 *)
(*   : DROP1   ( i64 -- )        drop ;                                 *)
(*                                                                     *)
(* Exit 0 means the whole file certified; exit 70 means the checker     *)
(* rejected a definition and the load hook refused it.  The observed    *)
(* status and the checker's own message are quoted at each example.     *)
(* ------------------------------------------------------------------ *)

(* The callee effects, one per prelude word, over their own row letter. *)
Definition wStep1 : word_eff := prim 1 [i64] [i64].
Definition wMkU8 : word_eff := prim 1 [] [u8].
Definition wMkCell : word_eff := prim 1 [] [cellt].
Definition wMkI64 : word_eff := prim 1 [] [i64].
Definition wMkBool : word_eff := prim 1 [] [boolt].
Definition wDup1 : word_eff := prim 1 [i64] [i64; i64].
Definition wDrop1 : word_eff := prim 1 [i64] [].

(* Signature letters: 0 is the declaration's implicit data row, 8 is `BROW`,
   9 is `RBROW`. *)
Definition sig (din dout : list ty) : cfg := MkCfg (decl_plain 0 9 din dout) 8 true.

(* --- 1. A branch whose arms join ------------------------------------ *)

(* : C1 ( i64 -- i64 ) MK-BOOL IF STEP1 ELSE STEP1 THEN ;   -> exit 0 *)
Example branch_arms_join :
  check_ctl (sig [i64] [i64])
            [TCall wMkBool; TIf; TCall wStep1; TElse; TCall wStep1; TThen]
  = VCert.
Proof. vm_compute; reflexivity. Qed.

(* --- 2. A branch whose arms disagree, and is rejected ---------------- *)

(* : C2 ( i64 -- i64 ) MK-BOOL IF STEP1 ELSE DROP1 MK-CELL THEN ;
   -> exit 70, `at 'THEN' expected: i64 actual: cell` *)
Example branch_arms_disagree_reject :
  check_ctl (sig [i64] [i64])
            [TCall wMkBool; TIf; TCall wStep1; TElse; TCall wDrop1; TCall wMkCell; TThen]
  = VReject.
Proof. vm_compute; reflexivity. Qed.

(* An `if` with no `else` must be stack neutral: the missing arm is the entry
   row.  The rejection is the row occurs check, which is why the checker's
   message names a whole row rather than a type.

   : C3 ( i64 -- i64 ) MK-BOOL IF STEP1 THEN ;  -> exit 0
   : C4 ( i64 -- i64 ) MK-BOOL IF DUP1  THEN ;  -> exit 70,
       `at 'THEN' expected: i64 actual: i64 i64` *)
Example no_else_arm_must_be_neutral :
  check_ctl (sig [i64] [i64]) [TCall wMkBool; TIf; TCall wStep1; TThen] = VCert
  /\ check_ctl (sig [i64] [i64]) [TCall wMkBool; TIf; TCall wDup1; TThen] = VReject.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* The join is EXACT while the boundary COERCES.  Two arms producing `u8` and
   `cell` are rejected, but a straight-line `u8` reaching a declared `cell`
   output certifies.  Nothing in docs/effects.md says this; `SUNI` calling bare
   `UNIFY` at `UK-EXACT` does.

   : C5 ( bool -- cell ) IF MK-U8 ELSE MK-CELL THEN ;  -> exit 70,
       `at 'THEN' expected: u8 actual: cell`
   : C6 ( -- cell ) MK-U8 ;                            -> exit 0 *)
Example join_is_exact_but_the_boundary_coerces :
  check_ctl (sig [boolt] [cellt])
            [TIf; TCall wMkU8; TElse; TCall wMkCell; TThen] = VReject
  /\ check_ctl (sig [] [cellt]) [TCall wMkU8] = VCert.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* A flag is a `bool`.  `bool` is not in the integer class, so `INT-WIDENS?`
   admits neither `n` nor `i64` into it, at any unification kind.

   : C33 ( i64 -- i64 ) IF STEP1 THEN ;  -> exit 70,
       `at 'IF' expected: i64 bool actual: i64` *)
Example a_flag_must_already_be_bool :
  check_ctl (sig [i64] [i64]) [TIf; TCall wStep1; TThen] = VReject.
Proof. vm_compute; reflexivity. Qed.

(* --- 3. Stack-neutral loops ----------------------------------------- *)

(* : C7 ( i64 -- i64 ) BEGIN MK-BOOL UNTIL ;        -> exit 0
   : C8 ( i64 -- i64 ) BEGIN DUP1 MK-BOOL UNTIL ;   -> exit 70,
       `at 'UNTIL' expected: i64 actual: i64 i64` *)
Example until_loop_must_be_neutral :
  check_ctl (sig [i64] [i64]) [TBegin; TCall wMkBool; TUntil] = VCert
  /\ check_ctl (sig [i64] [i64])
               [TBegin; TCall wDup1; TCall wMkBool; TUntil] = VReject.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* : C9  ( i64 -- i64 ) BEGIN MK-BOOL WHILE STEP1 REPEAT ;  -> exit 0
   : C10 ( i64 -- i64 ) BEGIN MK-BOOL WHILE DUP1  REPEAT ;  -> exit 70,
       `at 'REPEAT' expected: i64 actual: i64 i64` *)
Example while_repeat_loop_must_be_neutral :
  check_ctl (sig [i64] [i64])
            [TBegin; TCall wMkBool; TWhile; TCall wStep1; TRepeat] = VCert
  /\ check_ctl (sig [i64] [i64])
               [TBegin; TCall wMkBool; TWhile; TCall wDup1; TRepeat] = VReject.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* The back edge and the loop EXIT are different rows.  Only the whole cycle
   `begin`..`repeat` is neutral; the segment before `while` may change the
   stack, and the loop leaves with the row it had at `while`.

   : C11 ( i64 -- ) BEGIN DROP1 MK-BOOL WHILE MK-I64 REPEAT ;  -> exit 0 *)
Example loop_exit_row_is_the_while_row :
  check_ctl (sig [i64] [])
            [TBegin; TCall wDrop1; TCall wMkBool; TWhile; TCall wMkI64; TRepeat]
  = VCert.
Proof. vm_compute; reflexivity. Qed.

(* `again` closes the same frame with no flag, and kills the path after it.

   : C30 ( i64 -- i64 ) BEGIN STEP1 AGAIN ;        -> exit 0
   : C31 ( i64 -- i64 ) BEGIN STEP1 AGAIN STEP1 ;  -> exit 70,
       `at 'STEP1' after 'AGAIN'` *)
Example again_closes_and_kills_the_path :
  check_ctl (sig [i64] [i64]) [TBegin; TCall wStep1; TAgain] = VCert
  /\ check_ctl (sig [i64] [i64])
               [TBegin; TCall wStep1; TAgain; TCall wStep1] = VReject.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* --- 4. EXIT -------------------------------------------------------- *)

(* An early return that agrees with the fall-through, and one that does not.

   : C12 ( i64 -- i64 ) MK-BOOL IF DROP1 MK-I64 EXIT THEN STEP1 ;  -> exit 0
   : C13 ( i64 -- i64 ) DROP1 MK-BOOL EXIT ;                       -> exit 70,
       `at 'EXIT' expected: i64 actual: bool` *)
Example exit_matches_and_fails_the_declared_output :
  check_ctl (sig [i64] [i64])
            [TCall wMkBool; TIf; TCall wDrop1; TCall wMkI64; TExit; TThen;
             TCall wStep1] = VCert
  /\ check_ctl (sig [i64] [i64])
               [TCall wDrop1; TCall wMkBool; TExit] = VReject.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* docs/effects.md:201 says `EXIT` "asserts current = declared output".  It does
   not.  It asserts current = the OTHER returns, at `UK-EXACT`; only the joined
   result meets the declaration, at `UK-COERCE`.  The difference is observable:
   an `exit` leaving `u8` and a fall-through leaving `cell` are rejected even
   though each one alone would coerce into the declared `cell`.

   : C14 ( bool -- cell ) IF MK-U8   EXIT THEN MK-CELL ;  -> exit 70,
       `at 'MK-CELL'` (no expected/actual: the fold-exit unify captures none)
   : C15 ( bool -- cell ) IF MK-CELL EXIT THEN MK-CELL ;  -> exit 0
   : C6  ( -- cell ) MK-U8 ;                              -> exit 0 *)
Example early_returns_join_each_other_exactly :
  check_ctl (sig [boolt] [cellt])
            [TIf; TCall wMkU8; TExit; TThen; TCall wMkCell] = VReject
  /\ check_ctl (sig [boolt] [cellt])
               [TIf; TCall wMkCell; TExit; TThen; TCall wMkCell] = VCert
  /\ check_ctl (sig [] [cellt]) [TCall wMkU8] = VCert.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* A dead arm is EXCLUDED from the join rather than reconciled with it.  These
   two differ by one `EXIT`: with it, the arm's row never meets the
   fall-through's, and the definition stands; without it, they meet and the row
   occurs check rejects.

   : C15 ( bool -- cell ) IF MK-CELL EXIT THEN MK-CELL ;  -> exit 0
   : C32 ( bool -- cell ) IF MK-CELL      THEN MK-CELL ;  -> exit 70,
       `at 'THEN' expected: actual: cell` *)
Example a_dead_arm_is_excluded_from_the_join :
  check_ctl (sig [boolt] [cellt])
            [TIf; TCall wMkCell; TExit; TThen; TCall wMkCell] = VCert
  /\ check_ctl (sig [boolt] [cellt])
               [TIf; TCall wMkCell; TThen; TCall wMkCell] = VReject.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* --- 5. Dead code after EXIT ---------------------------------------- *)

(* An ordinary word after `exit` is rejected; a structural closer is not.

   : C16 ( i64 -- i64 ) EXIT STEP1 ;              -> exit 70,
       `at 'STEP1' after 'EXIT'`
   : C17 ( i64 -- i64 ) MK-BOOL IF EXIT THEN ;    -> exit 0 *)
Example dead_code_after_exit :
  check_ctl (sig [i64] [i64]) [TExit; TCall wStep1] = VReject
  /\ check_ctl (sig [i64] [i64]) [TCall wMkBool; TIf; TExit; TThen] = VCert.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* `until` is NOT in `DEAD-CLOSE?`, so it cannot close a dead path even though
   it closes a control frame.

   : C18 ( i64 -- i64 ) BEGIN EXIT UNTIL ;  -> exit 70,
       `at 'UNTIL' after 'EXIT'` *)
Example until_is_not_a_dead_closer :
  check_ctl (sig [i64] [i64]) [TBegin; TExit; TUntil] = VReject.
Proof. vm_compute; reflexivity. Qed.

(* --- 6. Quotation application --------------------------------------- *)

(* `Effects.v` models the quotation TYPE.  This applies one.

   : C19 ( i64 -- i64 ) [: STEP1 ;] execute ;  -> exit 0
   : C20 ( i64 -- i64 ) [: DUP1  ;] execute ;  -> exit 70,
       `at 'execute' expected: i64 actual: i64 i64` *)
Example quotation_application :
  check_ctl (sig [i64] [i64]) [TOpenQ; TCall wStep1; TCloseQ; TExec] = VCert
  /\ check_ctl (sig [i64] [i64]) [TOpenQ; TCall wDup1; TCloseQ; TExec] = VReject.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* The quotation really does carry its body's effect: the same quotation
   applied where its input is absent is rejected by the row occurs check, not
   by anything about `execute`. *)
Example an_applied_quotation_carries_its_effect :
  check_ctl (sig [] [i64]) [TOpenQ; TCall wStep1; TCloseQ; TExec] = VReject.
Proof. vm_compute; reflexivity. Qed.

(* `catch` applies the same quotation but demands it be stack preserving, then
   pushes a throw code of type `n`.

   : C21 ( i64 -- i64 n ) [: STEP1 ;] catch ;  -> exit 0 *)
Example catch_requires_a_stack_preserving_quotation :
  check_ctl (sig [i64] [i64; nt]) [TOpenQ; TCall wStep1; TCloseQ; TCatch] = VCert
  /\ check_ctl (sig [i64] [i64; i64; nt])
               [TOpenQ; TCall wDup1; TCloseQ; TCatch] = VReject.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* `exit` inside a quotation returns from the QUOTATION.  It folds into the
   quotation's own effect and leaves the enclosing definition live, because
   `CF-QUOT` cleared `XSET` and `DEADP` on the way in.

   : C23 ( -- i64 ) [: MK-I64 EXIT ;] execute ;  -> exit 0 *)
Example exit_is_quotation_scoped :
  check_ctl (sig [] [i64]) [TOpenQ; TCall wMkI64; TExit; TCloseQ; TExec] = VCert.
Proof. vm_compute; reflexivity. Qed.

(* An execution token of unknown provenance — modelled as anything that does
   not resolve to a quotation — is rejected rather than assumed.

   variable V
   : C22 ( -- ) V @ execute ;  -> exit 70,
       `at 'execute' execute: opaque xt of unknown provenance` *)
Example an_opaque_execution_token_rejects :
  check_ctl (sig [i64] []) [TExec] = VReject.
Proof. vm_compute; reflexivity. Qed.

(* --- 7. RECURSE ----------------------------------------------------- *)

(* `recurse` is a call to the word's own declaration.

   : C24 ( i64 -- i64 ) MK-BOOL IF STEP1 ELSE RECURSE THEN ;  -> exit 0
   : C25 ( i64 -- i64 ) DROP1 MK-BOOL RECURSE ;               -> exit 70,
       `at 'RECURSE' expected: i64 actual: bool` *)
Example recurse_checks_against_the_declaration :
  check_ctl (sig [i64] [i64])
            [TCall wMkBool; TIf; TCall wStep1; TElse; TRecurse; TThen] = VCert
  /\ check_ctl (sig [i64] [i64])
               [TCall wDrop1; TCall wMkBool; TRecurse] = VReject.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* Without a signature there is nothing to instantiate, and `CF-RECURSE` falls
   through to `UNCK` — uncheckable, which outranks a reject.  Both statuses
   fail the load hook, but the diagnostic differs: an uncheckable definition is
   reported with a bare token pin and no expected/actual pair.

   : C26 RECURSE ;                    -> exit 70, `at 'RECURSE'` (bare pin)
   : C27 ( i64 -- i64 ) RECURSE ;     -> exit 0 *)
Example recurse_without_a_signature_is_uncheckable :
  check_ctl (MkCfg (decl_plain 0 9 [] []) 8 false) [TRecurse] = VUncheckable
  /\ check_ctl (sig [i64] [i64]) [TRecurse] = VCert.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* --- 8. Unbalanced control ------------------------------------------ *)

(* A definition that ends with a frame still open is rejected at the boundary.

   : C29 ( i64 -- i64 ) MK-BOOL IF ;  -> exit 70, `at 'IF'` (bare pin) *)
Example an_unclosed_frame_rejects :
  check_ctl (sig [i64] [i64]) [TCall wMkBool; TIf] = VReject.
Proof. vm_compute; reflexivity. Qed.

(* A closer with no opener runs `CF-FAIL`.  Note that in a top-level definition
   the ENGINE rejects this before the checker ever sees it — `: C28 ( -- )
   THEN ;` reports `hb: control-flow closer without opener: THEN`, not a
   checker diagnostic.  So this arm of `CF-THEN` is defensive; the model keeps
   it, and rejecting here is at worst stricter than the checker. *)
Example a_closer_without_an_opener_rejects :
  check_ctl (sig [] []) [TThen] = VReject
  /\ check_ctl (sig [] []) [TRepeat] = VReject
  /\ check_ctl (sig [] []) [TElse] = VReject
  /\ check_ctl (sig [] []) [TCloseQ] = VReject.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* Mismatched openers and closers are rejected by frame KIND, not by nesting
   depth: `begin ... then` has a frame to pop but the wrong one. *)
Example a_closer_must_match_its_opener :
  check_ctl (sig [] []) [TBegin; TThen] = VReject
  /\ check_ctl (sig [] []) [TCall wMkBool; TIf; TUntil] = VReject
  /\ check_ctl (sig [] []) [TBegin; TRepeat] = VReject.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* --- 9. Agreement with the straight-line model ---------------------- *)

(* On a body with no control tokens this machine must decide exactly what
   `Effects.check_body` decides, including the implicit-row seal — the whole
   point of the seal being post-hoc is that control flow does not touch it.
   `declared_empty`, `trusted_img` and `balanced_word` are Effects.v's. *)
Example agrees_with_the_straight_line_model :
  check_ctl (MkCfg declared_empty 8 true) [TCall trusted_img]
    = VReject
  /\ check_body declared_empty [trusted_img] = false
  /\ check_ctl (MkCfg declared_empty 8 true) [TCall balanced_word]
    = VCert
  /\ check_body declared_empty [balanced_word] = true.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* And the seal still rejects a body that underflows inside a branch: the arm
   is free to bind the declared base row while it is scanned, both arms end up
   agreeing, the output row joins the declaration — and the definition is
   rejected anyway, because the base row came back bound to a spine. *)
Example the_seal_still_catches_underflow_inside_a_branch :
  check_ctl (MkCfg declared_empty 8 true)
            [TCall wMkBool; TIf; TCall trusted_img; TElse; TCall trusted_img; TThen]
  = VReject.
Proof. vm_compute; reflexivity. Qed.

(* A loop body that underflows is likewise NOT caught by the loop rule — the
   `begin` row and the `until` row both carry the borrowed cell, so they unify.
   The seal is what rejects it.  This is the clearest statement of what a
   post-hoc seal buys: no control construct has to know about it. *)
Example the_loop_rule_is_silent_about_underflow :
  check_ctl (MkCfg declared_empty 8 true)
            [TBegin; TCall trusted_img; TCall wMkBool; TUntil]
  = VReject.
Proof. vm_compute; reflexivity. Qed.

(* --- 10. Failure is still a value ------------------------------------ *)

(* Nothing above can raise, diverge, or be partial.  `step` is total, `run` is
   structural on the token list, and every verdict is one of three values. *)
Example failure_is_a_value :
  certifiedb (check_ctl (sig [i64] [i64]) [TExit; TCall wStep1]) = false
  /\ certifiedb (check_ctl (sig [i64] [i64]) [TCall wStep1]) = true.
Proof. repeat split; vm_compute; reflexivity. Qed.
