(* Habu.Common.Control — a model of the checked language's CONTROL FLOW.

   `Habu.Common.Effects` models types, stacks, unification, and the sequential
   composition of straight-line bodies.  It stops there on purpose.  This file
   picks up exactly where it stops: branches, both loop shapes, `case`, the
   `MATCH` eliminator, early return, the exceptional edge, dead paths,
   recursion, quotation application, branch-scoped locals, and the linear-once
   conservation pass.

   Ground truth is `src/core/checker.f`, not `docs/effects.md`.  The checker's
   control-flow words are dispatched from `CF-TOK?` (checker.f:8356-8390) and
   live at checker.f:7794-8000; `MATCH` lives at checker.f:8258-8352; the frame
   stack `CFS` and its accessors are at checker.f:7573-7677; the dead-path gate
   is `LIVE-TOKEN?` (checker.f:8534); the definition boundary is `CHECK`
   (checker.f:9670-9695).  Every definition below names the checker word it
   models with a line reference, and every definitional example at the end
   names the `.f` fixture that was loaded through `bin/hb`, with the exit
   status observed.

   WHY A TOKEN MACHINE AND NOT AN ABSTRACT SYNTAX TREE.  The checker is a
   single left-to-right token scan over the body text.  It has no parse tree:
   `if` PUSHES a frame onto `CFS` and `then` POPS one, and the rules that
   decide a program are stated over that frame stack, not over nesting.  Four
   things are only expressible this way, and all four decide real programs:

     - the dead-path rule is a property of the NEXT TOKEN's spelling
       (`DEAD-CLOSE?`, checker.f:8522-8532), not of a subtree;
     - `EXIT` accumulates into ONE per-definition row (`XROW`) no matter how
       deeply it is nested, so every early return in a word meets every other
       one directly, not at the enclosing join;
     - an unbalanced body is rejected by counting open frames at the end
       (`#CFC @ 0 <>`, checker.f:9682), which a tree cannot even express;
     - `of` and `endof` belong to TWO constructs at once and are told apart by
       the KIND of the enclosing frame (`CF-ENDOF-DISPATCH`, checker.f:8348).

   So the state below is the checker's registers — `DCUR`, `RCUR`, `BROW`,
   `RBROW`, `XROW`, `XRROW`, `XSET`, `THSET`, `DEADP`, `CFS`, `#LOC`, `MM`,
   `MPEND`, the `MF` arena, `LTNT`, `OK`, `UNCK`, `FAILSET`, `MDIAG`, and the
   hard-reject latches — and `step` is `DO-TOK1` restricted to the body tokens
   that carry an effect or a control decision.

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

   MODELLED FRAGMENT.  `if` / `else` / `then`; `begin` with `until`, `again`,
   and `while` / `repeat`; `do` / `?do` with `loop`, `+loop`, `i`, `j`,
   `leave`, `unloop`; `case` / `of` / `endof` / `endcase`; `match` /
   `of` / `endof` / `;match` with variant exhaustiveness; `exit`; `throw` and
   `die` and the throw edge they raise; `recurse`; `[:` / `;]`; `execute` and
   `catch`, both reading the quotation's control flags; `>r` and `r>`, which
   are their own rules and not calls; locals and their branch scoping; the
   per-step linear-once conservation pass and its deferred taint; and an
   ordinary call, with or without inherited control flags.

   DELIBERATE OMISSIONS, each of which the checker also decides and each of
   which a soundness proof built on this file would therefore not cover:
     - `uniform<bool>` block-uniform branches (`COND-UNIFORM?`,
       checker.f:7778-7799) and the block-collective barrier rule
       (`ALL-CF-UNIFORM?`, checker.f:7681), which need arity-1 type families;
     - the layout / hidden-field machinery, which `Effects.v` does not model.
       `MATCH`'s scrutinee is therefore abstracted: the checker requires a
       width-expanded hidden-field BUNDLE on top of the row and walks it cell
       by cell (`MATCH-SCRUT?`, checker.f:8236-8246), while the model requires
       one arity-0 `TFam` cell.  Every rule ABOVE that pop — payload
       refinement, the branch join, exhaustiveness — is modelled exactly;
     - `construct` (`CONM`), the field projection window, and the transport
       ops, which are row surgery over the same omitted layout machinery;
     - `Q>XDOUT` / `Q>XROUT`, the rows captured at a quotation's first throw.
       They are written by `QX!` (checker.f:310-314) and read only by the image
       serialisers (checker.f:2433-2434, 4197-4198); no rule consults them, so
       `Effects.TQuot` deliberately does not carry them.

   FAIL-CLOSED, AS IN Effects.v.  Every function here is total and returns a
   value, and unification that runs out of fuel reports failure, so the model
   can only reject more programs than the checker on that account.  The two
   divergences that ran the OTHER way in the previous revision are now closed
   and are recorded as examples rather than as caveats: see
   `frame_depth_is_bounded` and `xdead_quotation_does_not_install_its_output`.
   One divergence remains, and it cannot change a verdict: after `CF-PUSH`
   refuses a 33rd frame it does NOT advance `#CFC`, so the checker's following
   `CF-TOP` writes (`CF-CASE`'s `CF.DED`, `CF-QUOT`'s save slots) land on the
   frame BELOW; the model writes nothing at all.  `UNCK` is latched at that
   point and outranks `OK = 0` (`CHECK-VERDICT`, checker.f:9666-9668), and the
   only hard-reject latch this fragment can still raise afterwards is
   `MATCH`'s, which refuses at `#CFC @ 30 >` (checker.f:8255) before it can
   ever observe a corrupted frame.  So both machines answer UNCHECKABLE. *)

From Stdlib Require Import Bool List PeanoNat.
Import ListNotations.
From Habu.Common Require Import Effects.

(* ------------------------------------------------------------------ *)
(* Sum families, for `MATCH`.                                          *)
(*                                                                     *)
(* A family is a variant count plus, per variant, the payload its       *)
(* branch is refined with (`MATCH-PAY-XT`, applied in `MATCH-OF-TOK`,   *)
(* checker.f:8335-8337).  Variants are identified by DECLARATION ORDER: *)
(* `MATCH-VTAG-XT` maps a variant to its declaration-order tag and      *)
(* `MSEEN` is a bitset over exactly those tags (checker.f:8340-8342),   *)
(* which is what makes exhaustiveness decidable without names.          *)
(* ------------------------------------------------------------------ *)

Record fam : Type := MkFam {
  fam_id  : nat;
  fam_pay : list (list ty)   (* declaration order; payload written bottom-first *)
}.

Definition fam_vcnt (f : fam) : nat := length (fam_pay f).

(* ------------------------------------------------------------------ *)
(* Tokens.                                                             *)
(*                                                                     *)
(* `CF-TOK?` (checker.f:8356-8390) matches the control spellings;       *)
(* `MATCH-TOK` (checker.f:8344) intercepts every token while `MM` is    *)
(* non-zero; `throw` and `die` are recognised by name in `THROW-CUR?`   *)
(* and `DEAD-CUR?` (checker.f:6359-6365); every other token that        *)
(* carries an effect goes through `EFF-APPLY`, which `Effects.compose`  *)
(* already models.  `TCall` is that case.                              *)
(* ------------------------------------------------------------------ *)

Inductive tok : Type :=
  | TCall : word_eff -> tok   (* an ordinary word: EFF-APPLY, checker.f:4629 *)
  (* A call to a word whose CERTIFIED body was dead or threw, so `CHECK`
     recorded `CTL-DEAD` / `CTL-THROW` for it (checker.f:9696-9701) and
     `DEAD-CUR?` / `THROW-CUR?` read them back at every later call site.
     Arguments: the effect, then dead, then throw. *)
  | TCallCtl : word_eff -> bool -> bool -> tok
  | TThrow                    (* `throw`: PRIM ( n -- ), checker.f:4934 *)
  | TDie                      (* `die`:   PRIM ( ptr u8 n n -- ), checker.f:4935 *)
  | TIf                       (* CF-IF,      checker.f:7794 *)
  | TElse                     (* CF-ELSE,    checker.f:7849 *)
  | TThen                     (* CF-THEN,    checker.f:7877 *)
  | TBegin                    (* CF-BEGIN,   checker.f:7895 *)
  | TUntil                    (* CF-UNTIL,   checker.f:7898 *)
  | TAgain                    (* CF-AGAIN,   checker.f:7904 *)
  | TWhile                    (* CF-WHILE,   checker.f:7909 *)
  | TRepeat                   (* CF-REPEAT,  checker.f:7917 *)
  | TDo                       (* CF-DO, `do` and `?do` alike, checker.f:7922 *)
  | TLoop                     (* CF-LOOP,    checker.f:7929 *)
  | TPlusLoop                 (* CF-+LOOP,   checker.f:7936 *)
  | TI                        (* CF-I,       checker.f:7942 *)
  | TJ                        (* CF-J,       checker.f:7947 *)
  | TLeave                    (* CF-LEAVE,   checker.f:7967 *)
  | TUnloop                   (* CF-UNLOOP,  checker.f:7893 *)
  | TCase                     (* CF-CASE,    checker.f:7801 *)
  | TOf                       (* CF-OF / MATCH-OF-TOK, checker.f:7817, 8330 *)
  | TEndof                    (* CF-ENDOF-DISPATCH, checker.f:8348 *)
  | TEndcase                  (* CF-ENDCASE, checker.f:7836 *)
  | TMatch                    (* MATCH-BEGIN, checker.f:8180 *)
  | TFamTok : fam -> tok      (* MATCH-FAM-TOK, checker.f:8252 *)
  | TVarTok : nat -> tok      (* MATCH-VARIANT-TOK, checker.f:8318 *)
  | TSemiMatch                (* MATCH-SEMI, checker.f:8296 *)
  | TExit                     (* CF-EXIT,    checker.f:7887 *)
  | TRecurse                  (* CF-RECURSE, checker.f:7758 *)
  | TOpenQ                    (* CF-QUOT  `[:`, checker.f:7971 *)
  | TCloseQ                   (* CF-SEMIQ `;]`, checker.f:7987 *)
  | TExec                     (* RSEXEC,   checker.f:2003 *)
  | TCatch                    (* RSCATCH,  checker.f:2039 *)
  | TLocals : nat -> tok      (* `{: x .. :}` binding n locals: LOC-BIND, checker.f:7460 *)
  | TLocRef : nat -> tok      (* a local reference, 0 = most recent: LOC-REF?, checker.f:7505 *)
  | TToR                      (* `>r`: RS->R, checker.f:1941 *)
  | TFromR.                   (* `r>`: RSR>,  checker.f:1947 *)

(* ------------------------------------------------------------------ *)
(* A control frame (`CFS`, checker.f:7573-7597).                       *)
(*                                                                     *)
(* Frame KINDS: 1 = `if` with no `else` yet, 2 = `if`/`else`,          *)
(* 3 = `begin`, 4 = `begin`/`while`, 5 = `do`, 6 = `[:`, 7 = `case`,   *)
(* 8 = a `case` branch, 9 = `match`, 10 = a `match` branch.            *)
(*                                                                     *)
(* `CF-PUSH` (checker.f:7651) writes only KND, SA, SB, RA, RB, the      *)
(* locals mark and CF.UNI; it leaves DED and the save slots at whatever *)
(* the reused `CFS` cell happened to hold.  Every read of those slots   *)
(* is preceded by a write from the construct that owns them, so any     *)
(* initial value is faithful; `cf_push` below takes a whole frame, so   *)
(* each construct writes its own slots at push time — which also makes  *)
(* the depth refusal write nothing at all.                              *)
(*                                                                     *)
(* CF.TXD and CF.TXR (the saved outer throw ROWS) are omitted for the   *)
(* reason given in the header: nothing reads them back out.  CF.TXS,    *)
(* the saved outer THSET, is kept because `CF-SEMIQ` turns it into the  *)
(* quotation's `Q>XHAS`, which `RSEXEC` and `RSCATCH` both read.        *)
(* ------------------------------------------------------------------ *)

Record frame : Type := MkFrame {
  fr_knd : nat;
  fr_sa  : stack;     (* CF.SA: the data row the frame was opened at *)
  fr_sb  : stack;     (* CF.SB: written by CF-ELSE / CF-WHILE; the CASE accumulator *)
  fr_ra  : stack;     (* CF.RA *)
  fr_rb  : stack;     (* CF.RB *)
  fr_ded : bool;      (* CF.DED: the if-arm's deadness; the CASE accumulator's has-flag *)
  fr_ln  : nat;       (* CF.LN: #LOC at the push, restored by CF-LOC-REST *)
  fr_xro : stack;     (* CF.XRO \                                        *)
  fr_xrr : stack;     (* CF.XRR  |  outer early-return state, saved by   *)
  fr_xst : bool;      (* CF.XST  |  CF-QUOT and restored by CF-SEMIQ     *)
  fr_xdp : bool;      (* CF.XDP /                                        *)
  fr_txs : bool       (* CF.TXS: outer THSET, saved and restored likewise *)
}.

(* ------------------------------------------------------------------ *)
(* A `MATCH` frame (`MF`, checker.f:8026-8035).                        *)
(*                                                                     *)
(* This is a SECOND stack, growing in step with the kind-9 CFS frames.  *)
(* It holds what the branch join and the exhaustiveness test need:      *)
(* the family, the rows the scrutinee's bundle was popped down to, the  *)
(* accumulated branch output, and the declaration-order tag set.        *)
(* `MF.TERM`, `MF.VCNT` and `MF.TIX` are derivable or diagnostic-only.  *)
(* ------------------------------------------------------------------ *)

Record mframe : Type := MkMF {
  mf_fam   : fam;
  mf_base  : stack;   (* MF.BASE:  row below the popped bundle *)
  mf_rbase : stack;   (* MF.RBASE *)
  mf_out   : stack;   (* MF.OUT:   the accumulated branch output *)
  mf_rout  : stack;   (* MF.ROUT *)
  mf_has   : bool;    (* MF.HAS:   has any live branch contributed yet? *)
  mf_seen  : list nat (* MSEEN:    declaration-order tags already branched on *)
}.

(* ------------------------------------------------------------------ *)
(* The checker's registers while it scans one definition.              *)
(*                                                                     *)
(* `DCUR`/`RCUR` are the current data and return rows.  `BROW`/`RBROW` *)
(* are the base rows of the CURRENT scope: `NEW` (checker.f:1706-1714) *)
(* allocates them fresh, `CHECK-SCAN` then points DCUR at the parsed    *)
(* signature input but leaves BROW alone, and `CF-QUOT` re-points both  *)
(* at fresh rows for the quotation body.  `XROW`/`XRROW`/`XSET` are the *)
(* early-return accumulator and `THSET` the throw edge; `DEADP` says    *)
(* the current linear path is terminated.  `st_fv` is `FV`              *)
(* (checker.f:1687-1690), the single counter both type and row          *)
(* variables are drawn from.                                            *)
(*                                                                     *)
(* `st_loc` is the locals table (`LOCTV`, checker.f:7381) with the most *)
(* recent binding at the HEAD, so `#LOC` is its length and              *)
(* `CF-LOC-REST` is a truncation.  `st_mm` / `st_mpend` / `st_mf` are   *)
(* `MATCH`'s sub-mode, pending variant and frame arena.  `st_taint` is  *)
(* `LTNT` (checker.f:239), the deferred linear-laundering watch list.   *)
(*                                                                     *)
(* `st_failset` is `FAILSET`, the "a failure has been pinned" latch.    *)
(* It decides nothing about acceptance, but `MDIAG!` (checker.f:8186)   *)
(* refuses to latch a reason once it is set, so it is what makes        *)
(* `MD-JOIN` name the `match` join and nothing else.                    *)
(*                                                                     *)
(* `QDEPTH` is NOT a field: `CF-QUOT` raises it and `CF-SEMIQ` lowers   *)
(* it in exact step with the kind-6 frames, so it is derived below.     *)
(* ------------------------------------------------------------------ *)

Record st : Type := MkSt {
  st_sub     : subst;
  st_fv      : nat;
  st_dcur    : stack;
  st_rcur    : stack;
  st_brow    : stack;
  st_rbrow   : stack;
  st_xset    : bool;
  st_xrow    : stack;
  st_xrrow   : stack;
  st_thset   : bool;
  st_dead    : bool;
  st_cfs     : list frame;    (* head = top of CFS *)
  st_loc     : list ty;       (* head = most recently bound local *)
  st_mm      : nat;           (* MM: 0 = not in a match form *)
  st_mpend   : option nat;    (* MPEND: the variant awaiting its `of` *)
  st_mf      : list mframe;   (* head = MF-CUR *)
  st_taint   : list tyvar;    (* LTNT *)
  st_ok      : bool;          (* OK *)
  st_unck    : bool;          (* UNCK *)
  st_failset : bool;          (* FAILSET *)
  st_mdiag   : nat;           (* MDIAG *)
  st_hard    : bool           (* MREJ / LOCALBAD / LINLOCBAD *)
}.

(* `MDIAG` reason codes, checker.f:8150-8175.  Only the ones this fragment
   can raise are named. *)
Definition MD_SCRUT : nat := 3.
Definition MD_VAR_UNKNOWN : nat := 5.
Definition MD_VAR_DUP : nat := 6.
Definition MD_MISSING_OF : nat := 7.
Definition MD_NONEXH : nat := 8.
Definition MD_STRAY : nat := 9.
Definition MD_TRUNC : nat := 10.
Definition MD_DEPTH : nat := 11.
Definition MD_QUOT : nat := 12.
Definition MD_JOIN : nat := 14.

(* `QDEPTH`, raised by `CF-QUOT` (checker.f:7981) and lowered by `CF-SEMIQ`
   (checker.f:7999).  It gates `MATCH`'s scrutinee (`MATCH-SCRUT-DIAG`,
   checker.f:8249) and every locals operation (`LOC-BEGIN`, checker.f:7484;
   `LOC-REF?`, checker.f:7511). *)
Definition qdepth (s : st) : nat :=
  length (filter (fun f => Nat.eqb (fr_knd f) 6) (st_cfs s)).

(* Field updates, written out one per field so a reader can check them by
   eye against the record above.  The `_` in each pattern is the slot that
   changes; every other slot is carried through by name. *)

Definition put_sub (s : st) (x : subst) : st :=
  let '(MkSt _ fv dcur rcur brow rbrow xset xrow xrrow thset dead cfs loc mm mpend mf taint ok unck failset mdiag hard) := s in
  MkSt x fv dcur rcur brow rbrow xset xrow xrrow thset dead cfs loc mm mpend mf taint ok unck failset mdiag hard.

Definition put_fv (s : st) (n : nat) : st :=
  let '(MkSt sub _ dcur rcur brow rbrow xset xrow xrrow thset dead cfs loc mm mpend mf taint ok unck failset mdiag hard) := s in
  MkSt sub n dcur rcur brow rbrow xset xrow xrrow thset dead cfs loc mm mpend mf taint ok unck failset mdiag hard.

Definition put_d (s : st) (d : stack) : st :=
  let '(MkSt sub fv _ rcur brow rbrow xset xrow xrrow thset dead cfs loc mm mpend mf taint ok unck failset mdiag hard) := s in
  MkSt sub fv d rcur brow rbrow xset xrow xrrow thset dead cfs loc mm mpend mf taint ok unck failset mdiag hard.

Definition put_r (s : st) (r : stack) : st :=
  let '(MkSt sub fv dcur _ brow rbrow xset xrow xrrow thset dead cfs loc mm mpend mf taint ok unck failset mdiag hard) := s in
  MkSt sub fv dcur r brow rbrow xset xrow xrrow thset dead cfs loc mm mpend mf taint ok unck failset mdiag hard.

Definition put_dr (s : st) (d : stack) (r : stack) : st :=
  let '(MkSt sub fv _ _ brow rbrow xset xrow xrrow thset dead cfs loc mm mpend mf taint ok unck failset mdiag hard) := s in
  MkSt sub fv d r brow rbrow xset xrow xrrow thset dead cfs loc mm mpend mf taint ok unck failset mdiag hard.

Definition put_base (s : st) (b : stack) (rb : stack) : st :=
  let '(MkSt sub fv dcur rcur _ _ xset xrow xrrow thset dead cfs loc mm mpend mf taint ok unck failset mdiag hard) := s in
  MkSt sub fv dcur rcur b rb xset xrow xrrow thset dead cfs loc mm mpend mf taint ok unck failset mdiag hard.

Definition put_x (s : st) (xs : bool) (xd : stack) (xr : stack) : st :=
  let '(MkSt sub fv dcur rcur brow rbrow _ _ _ thset dead cfs loc mm mpend mf taint ok unck failset mdiag hard) := s in
  MkSt sub fv dcur rcur brow rbrow xs xd xr thset dead cfs loc mm mpend mf taint ok unck failset mdiag hard.

Definition put_thset (s : st) (b : bool) : st :=
  let '(MkSt sub fv dcur rcur brow rbrow xset xrow xrrow _ dead cfs loc mm mpend mf taint ok unck failset mdiag hard) := s in
  MkSt sub fv dcur rcur brow rbrow xset xrow xrrow b dead cfs loc mm mpend mf taint ok unck failset mdiag hard.

Definition put_dead (s : st) (b : bool) : st :=
  let '(MkSt sub fv dcur rcur brow rbrow xset xrow xrrow thset _ cfs loc mm mpend mf taint ok unck failset mdiag hard) := s in
  MkSt sub fv dcur rcur brow rbrow xset xrow xrrow thset b cfs loc mm mpend mf taint ok unck failset mdiag hard.

Definition put_cfs (s : st) (l : list frame) : st :=
  let '(MkSt sub fv dcur rcur brow rbrow xset xrow xrrow thset dead _ loc mm mpend mf taint ok unck failset mdiag hard) := s in
  MkSt sub fv dcur rcur brow rbrow xset xrow xrrow thset dead l loc mm mpend mf taint ok unck failset mdiag hard.

Definition put_loc (s : st) (l : list ty) : st :=
  let '(MkSt sub fv dcur rcur brow rbrow xset xrow xrrow thset dead cfs _ mm mpend mf taint ok unck failset mdiag hard) := s in
  MkSt sub fv dcur rcur brow rbrow xset xrow xrrow thset dead cfs l mm mpend mf taint ok unck failset mdiag hard.

Definition put_mm (s : st) (n : nat) : st :=
  let '(MkSt sub fv dcur rcur brow rbrow xset xrow xrrow thset dead cfs loc _ mpend mf taint ok unck failset mdiag hard) := s in
  MkSt sub fv dcur rcur brow rbrow xset xrow xrrow thset dead cfs loc n mpend mf taint ok unck failset mdiag hard.

Definition put_mpend (s : st) (o : option nat) : st :=
  let '(MkSt sub fv dcur rcur brow rbrow xset xrow xrrow thset dead cfs loc mm _ mf taint ok unck failset mdiag hard) := s in
  MkSt sub fv dcur rcur brow rbrow xset xrow xrrow thset dead cfs loc mm o mf taint ok unck failset mdiag hard.

Definition put_mf (s : st) (l : list mframe) : st :=
  let '(MkSt sub fv dcur rcur brow rbrow xset xrow xrrow thset dead cfs loc mm mpend _ taint ok unck failset mdiag hard) := s in
  MkSt sub fv dcur rcur brow rbrow xset xrow xrrow thset dead cfs loc mm mpend l taint ok unck failset mdiag hard.

Definition put_taint (s : st) (l : list tyvar) : st :=
  let '(MkSt sub fv dcur rcur brow rbrow xset xrow xrrow thset dead cfs loc mm mpend mf _ ok unck failset mdiag hard) := s in
  MkSt sub fv dcur rcur brow rbrow xset xrow xrrow thset dead cfs loc mm mpend mf l ok unck failset mdiag hard.

(* `CF-FAIL` (checker.f:7689) and every `0 OK !` site: OK is a LATCH.  The
   checker keeps scanning after a failure, so this must not stop the machine. *)
Definition fail (s : st) : st :=
  let '(MkSt sub fv dcur rcur brow rbrow xset xrow xrrow thset dead cfs loc mm mpend mf taint _ unck failset mdiag hard) := s in
  MkSt sub fv dcur rcur brow rbrow xset xrow xrrow thset dead cfs loc mm mpend mf taint false unck failset mdiag hard.

(* `-1 UNCK !`: uncheckable, which `CHECK-VERDICT` ranks ABOVE a plain OK=0
   and BELOW every hard latch (checker.f:9666-9668). *)
Definition set_unck (s : st) : st :=
  let '(MkSt sub fv dcur rcur brow rbrow xset xrow xrrow thset dead cfs loc mm mpend mf taint ok _ failset mdiag hard) := s in
  MkSt sub fv dcur rcur brow rbrow xset xrow xrrow thset dead cfs loc mm mpend mf taint ok true failset mdiag hard.

Definition put_failset (s : st) (b : bool) : st :=
  let '(MkSt sub fv dcur rcur brow rbrow xset xrow xrrow thset dead cfs loc mm mpend mf taint ok unck _ mdiag hard) := s in
  MkSt sub fv dcur rcur brow rbrow xset xrow xrrow thset dead cfs loc mm mpend mf taint ok unck b mdiag hard.

Definition put_mdiag (s : st) (n : nat) : st :=
  let '(MkSt sub fv dcur rcur brow rbrow xset xrow xrrow thset dead cfs loc mm mpend mf taint ok unck failset _ hard) := s in
  MkSt sub fv dcur rcur brow rbrow xset xrow xrrow thset dead cfs loc mm mpend mf taint ok unck failset n hard.

(* The hard structural latches this fragment can raise: `MREJ`
   (`MATCH-REJECT`, checker.f:8177), `LOCALBAD` (`LOC-REJECT`,
   checker.f:7481) and `LINLOCBAD` (`LIN-LOCAL-REJECT`, checker.f:7399).
   All three also clear OK and take the failure pin, and `CHECK-VERDICT`
   ORs them into one reject that outranks UNCK — so one field is enough,
   and nothing downstream can tell them apart either. *)
Definition set_hard (s : st) : st :=
  let '(MkSt sub fv dcur rcur brow rbrow xset xrow xrrow thset dead cfs loc mm mpend mf taint _ unck _ mdiag _) := s in
  MkSt sub fv dcur rcur brow rbrow xset xrow xrrow thset dead cfs loc mm mpend mf taint false unck true mdiag true.

(* --- fresh variables (`FRESH`, checker.f:1687) ---------------------- *)

Definition fresh_id (s : st) : nat * st := (st_fv s, put_fv s (S (st_fv s))).

(* ------------------------------------------------------------------ *)
(* The linear-once conservation pass.                                  *)
(*                                                                     *)
(* Two independent mechanisms, and they catch different programs.      *)
(*                                                                     *)
(* (a) CONCRETE COUNT CONSERVATION.  `CHECKER-STEP` (checker.f:1928)   *)
(*     counts the linear cons on DCUR+RCUR before the step and after   *)
(*     it, and rejects a change — UNLESS the step's own DECLARED rows  *)
(*     name a linear (`LIN-EXPLICIT?`, checker.f:1879), in which case  *)
(*     the change is the point of the word and is allowed.  Measured:  *)
(*     `Q2 ( ltok -- ltok ltok ) dup` and `Q3 ( ltok -- ) drop` are    *)
(*     rejected, `Q4 ( ltok n -- n ltok ) swap` certifies.             *)
(*                                                                     *)
(* (b) DEFERRED TAINT.  A polymorphic word may copy or drop a variable *)
(*     that has not resolved yet.  `LIN-EFF-PASS` (checker.f:4610)     *)
(*     compares each effect variable's multiplicity across the input   *)
(*     and output rows: if it already resolved to a linear con and the *)
(*     counts differ, reject; if it is still a variable, TAINT it, and *)
(*     `LIN-TAINT-SCAN` (checker.f:1826) rejects later, after each     *)
(*     token, if that variable ever binds to a linear con.  Measured:  *)
(*     `: DUP-POLY ( a -- a a ) dup ;` certifies on its own, but       *)
(*     `R1 ( ltok -- ltok ltok ) DUP-POLY` rejects at `DUP-POLY`, and  *)
(*     `R5 ( ltok -- ltok ) [: DUP-POLY drop ;] execute` rejects at    *)
(*     `execute` — the taint outlived the quotation.                   *)
(*                                                                     *)
(* `LIN-ANY?` (checker.f:234) short-circuits both when no linear type  *)
(* has been declared at all.  It is a pure optimisation — with no      *)
(* linear con in the program every count is 0 and no variable can      *)
(* resolve linear — so the model always runs the pass.                 *)
(* ------------------------------------------------------------------ *)

(* `LIN-TYPE-COUNT*`, checker.f:1842-1861.  Over the fragment `Effects.v`
   models this is not recursive: a pointer, a quotation and an atom all count
   0 by their own arms, a variable falls to the `endcase` default, and only a
   resolved linear con counts.  The omitted arms are the layout ones. *)
Definition lin_ty (s : subst) (t : ty) : nat :=
  match resolve_ty s t with
  | TCon c => if linear_conb c then 1 else 0
  | _ => 0
  end.

(* `LIN-ROW-COUNT`, checker.f:1864-1872: walk the spine, resolving as it goes. *)
Fixpoint lin_row_fuel (fuel : nat) (s : subst) (x : stack) : nat :=
  match fuel with
  | 0 => 0
  | S f =>
      match resolve_row s x with
      | SRow _ => 0
      | SPush t rest => lin_ty s t + lin_row_fuel f s rest
      end
  end.

Definition lin_row (s : subst) (x : stack) : nat :=
  lin_row_fuel (walk_budget s (stack_size x)) s x.

(* `LIN-TOTAL`, checker.f:1873. *)
Definition lin_total (s : subst) (a b : stack) : nat := lin_row s a + lin_row s b.

(* `LIN-SNAPSHOT`, checker.f:1876: the live rows, under the substitution as it
   stands BEFORE the step runs. *)
Definition lin_snapshot (s : st) : nat := lin_total (st_sub s) (st_dcur s) (st_rcur s).

(* `LIN-CHECK`, checker.f:1882.  Gated on OK, as at every call site. *)
Definition lin_check (s : st) (before : nat) : st :=
  if negb (st_ok s) then s
  else if Nat.eqb (lin_snapshot s) before then s else fail s.

(* `LIN-EXPLICIT?`, checker.f:1879. *)
Definition lin_explicitb (s : subst) (a b : stack) : bool :=
  negb (Nat.eqb (lin_total s a b) 0).

(* `LIN-TAINT-SCAN`, checker.f:1826-1834: any watched variable that has since
   resolved to a linear con laundered one. *)
Fixpoint taint_scan (s : subst) (l : list tyvar) : bool :=
  match l with
  | [] => false
  | v :: rest =>
      match resolve_ty s (TVar v) with
      | TCon c => if linear_conb c then true else taint_scan s rest
      | _ => taint_scan s rest
      end
  end.

Definition lin_taint_scan (s : st) : st :=
  if negb (st_ok s) then s
  else if taint_scan (st_sub s) (st_taint s) then fail s else s.

Definition add_taint (s : st) (v : tyvar) : st :=
  if mem_nat v (st_taint s) then s else put_taint s (v :: st_taint s).

(* `EN-MULT`, checker.f:4557-4590.  Multiplicity of one variable in one row,
   split by POLARITY: a quotation's input rows flip the polarity, so a
   quotation that consumes a variable counts as producing it.  This walks the
   effect's STORED node tree, not the resolved terms — which is why there is
   no substitution and no fuel here: the tree is finite. *)
Definition mpair : Type := nat * nat.   (* (LMNEG, LMPOS) = (inputs, outputs) *)
Definition madd (a b : mpair) : mpair := (fst a + fst b, snd a + snd b).
Definition mnil : mpair := (0, 0).

Fixpoint mv_ty (v : tyvar) (pol : bool) (t : ty) : mpair :=
  match t with
  | TVar w => if Nat.eqb w v then (if pol then (0, 1) else (1, 0)) else mnil
  | TCon _ => mnil
  | TFam _ => mnil
  | TPtr u => mv_ty v pol u
  | TQuot _ _ (Eff a b c d) =>
      madd (madd (mv_row v (negb pol) a) (mv_row v pol b))
           (madd (mv_row v (negb pol) c) (mv_row v pol d))
  end
with mv_row (v : tyvar) (pol : bool) (x : stack) : mpair :=
  match x with
  | SRow _ => mnil
  | SPush t rest => madd (mv_ty v pol t) (mv_row v pol rest)
  end.

(* `LIN-VAR-MULT`, checker.f:4593-4602: inputs at negative polarity, outputs at
   positive, and the return rows only when the signature declared them. *)
Definition var_mult (w : word_eff) (v : tyvar) : mpair :=
  let d := madd (mv_row v false (we_din w)) (mv_row v true (we_dout w)) in
  if we_hasr w
  then madd d (madd (mv_row v false (we_rin w)) (mv_row v true (we_rout w)))
  else d.

Definition mult_balancedb (w : word_eff) (v : tyvar) : bool :=
  let m := var_mult w v in Nat.eqb (fst m) (snd m).

(* The canonical effect variables `LIN-EFF-PASS` walks (`EI-TV`,
   checker.f:4614).  Collected from the instantiated effect, which is a
   renaming of the stored one and so has the same variables up to the shift. *)
Fixpoint vars_ty (t : ty) (acc : list tyvar) : list tyvar :=
  match t with
  | TVar w => if mem_nat w acc then acc else w :: acc
  | TCon _ => acc
  | TFam _ => acc
  | TPtr u => vars_ty u acc
  | TQuot _ _ (Eff a b c d) =>
      vars_row a (vars_row b (vars_row c (vars_row d acc)))
  end
with vars_row (x : stack) (acc : list tyvar) : list tyvar :=
  match x with
  | SRow _ => acc
  | SPush t rest => vars_ty t (vars_row rest acc)
  end.

Definition vars_eff (w : word_eff) : list tyvar :=
  vars_row (we_din w) (vars_row (we_dout w)
    (vars_row (we_rin w) (vars_row (we_rout w) []))).

(* `LIN-EFF-PASS`, checker.f:4610-4628.  Gated on OK, as the checker gates it. *)
Fixpoint lin_eff_pass_vars (w : word_eff) (vs : list tyvar) (s : st) : st :=
  match vs with
  | [] => s
  | v :: rest =>
      let s :=
        if mult_balancedb w v then s
        else match resolve_ty (st_sub s) (TVar v) with
             | TCon c => if linear_conb c then fail s else s
             | TVar u => add_taint s u
             | _ => s
             end in
      lin_eff_pass_vars w rest s
  end.

Definition lin_eff_pass (s : st) (w : word_eff) : st :=
  if negb (st_ok s) then s else lin_eff_pass_vars w (vars_eff w) s.

(* ------------------------------------------------------------------ *)
(* Unification against a register.                                     *)
(* ------------------------------------------------------------------ *)

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

(* `SUNI` (checker.f:7695-7700) is `UNIFY` plus the failure PIN: on the first
   failure that happens while OK is still set, it records the expected/actual
   pair and latches `FAILSET`.  `MDIAG!` then refuses every later reason, which
   is exactly how a `match` join mismatch keeps `MD-JOIN` and a plain type
   clash inside a branch does not. *)
Definition uni_pin (k : ukind) (s : st) (got want : stack) : st :=
  match unify_stack k (st_sub s) got want with
  | Some s' => put_sub s s'
  | None =>
      if st_ok s && negb (st_failset s)
      then put_failset (fail s) true
      else fail s
  end.

(* `SUNI` / `RSUNI` (checker.f:7695, 7713).  Both call bare `UNIFY`, and
   `UNIFY-IN` / `UNIFY-COERCE` restore `UK-EXACT` when they return
   (checker.f:1676-1683), so EVERY branch join, loop back-edge, `case` join,
   `match` join and early-return join in the checker runs at `UK-EXACT`: no
   integer widening, no field coercion.  Only the definition boundary coerces.
   Measured: a word whose two arms produce `u8` and `cell` is rejected, while a
   straight-line word that produces `u8` for a declared `cell` output
   certifies. *)
Definition suni (s : st) (want : stack) : st := uni_pin UkExact s (st_dcur s) want.
Definition rsuni (s : st) (want : stack) : st := uni_pin UkExact s (st_rcur s) want.

(* `SUNI-IN` / `RSUNI-IN` (checker.f:7704, 7715). *)
Definition suni_in (s : st) (want : stack) : st := uni_pin UkInput s (st_dcur s) want.
Definition rsuni_in (s : st) (want : stack) : st := uni_pin UkInput s (st_rcur s) want.

(* ------------------------------------------------------------------ *)
(* Primitive steps.                                                    *)
(* ------------------------------------------------------------------ *)

(* `CHECKER-STEP`, checker.f:1928-1936, in full: take the linear snapshot
   unless the DECLARED rows are explicit about linears, unify the current row
   against the declared input at `UK-INPUT`, REPLACE the current row with the
   declared output — unconditionally, the checker does it even when the
   unification failed — and then re-check the linear count. *)
Definition checker_step (s : st) (din dout : stack) : st :=
  let explicit := lin_explicitb (st_sub s) din dout in
  let before := lin_snapshot s in
  let s := suni_in s din in
  let s := put_d s dout in
  if explicit then s else lin_check s before.

(* `STEP-TYPE-IN` (checker.f:3231): allocate a fresh tail and step through it. *)
Definition step_ty_in (s : st) (t : ty) : st :=
  let (r, s) := fresh_id s in
  let rest := SRow r in
  checker_step s (SPush t rest) rest.

(* `STEP-TYPE-OUT` (checker.f:3225). *)
Definition step_ty_out (s : st) (t : ty) : st :=
  let (r, s) := fresh_id s in
  let rest := SRow r in
  checker_step s rest (SPush t rest).

(* `STEP-TYPE2-IN` (checker.f:3236): two cells at once, `a` deeper than `b`. *)
Definition step_ty2_in (s : st) (a b : ty) : st :=
  let (r, s) := fresh_id s in
  let rest := SRow r in
  checker_step s (SPush b (SPush a rest)) rest.

(* `STEP-BOOL-IN` (checker.f:3251).  `bool` is not in the integer class, so
   `INT-WIDENS?` never admits `n` or `i64` here: a flag must already be a
   `bool`.  Measured: `: C33 ( i64 -- i64 ) IF STEP1 THEN ;` is rejected with
   `at 'IF' expected: i64 bool actual: i64`. *)
Definition step_bool_in (s : st) : st := step_ty_in s boolt.

(* `STEP-N-IN` (checker.f:3242), `STEP-N-OUT` (checker.f:3245),
   `STEP-NN-IN` (checker.f:3254). *)
Definition step_n_in (s : st) : st := step_ty_in s nt.
Definition step_n_out (s : st) : st := step_ty_out s nt.
Definition step_nn_in (s : st) : st := step_ty2_in s nt nt.

(* An ordinary call: `EFF-APPLY` (checker.f:4629-4638).  The data rows go
   through `CHECKER-STEP` — so the linear count is checked BEFORE the return
   rows move, exactly as the checker orders it — then the return rows, only if
   the callee's signature actually wrote a `|` clause, and finally the
   per-variable multiplicity pass. *)
Definition apply_eff (s : st) (w : word_eff) : st :=
  let above := st_fv s in
  let w' := instantiate above w in
  let s := put_fv s (above + next_eff (we_eff w)) in
  let s := checker_step s (we_din w') (we_dout w') in
  let s := if we_hasr w'
           then let s := rsuni_in s (we_rin w') in put_r s (we_rout w')
           else s in
  lin_eff_pass s w'.

(* ------------------------------------------------------------------ *)
(* The dead-path gate.                                                 *)
(*                                                                     *)
(* `LIVE-TOKEN?` (checker.f:8534-8536) and `DEAD-CLOSE?`               *)
(* (checker.f:8522-8532).  While `DEADP` is set, only a structural      *)
(* closer may appear; anything else sets `DEADERR` and `OK = 0`, and    *)
(* the token is NOT processed at all (`DO-TOK1`, checker.f:9068).       *)
(*                                                                     *)
(* Note what is and is not a closer.  `else then loop +loop endof       *)
(* endcase repeat again ;]` are; `until`, `while` and `;match` are NOT. *)
(* So `: C18 ( i64 -- i64 ) BEGIN EXIT UNTIL ;` is rejected, at         *)
(* `UNTIL`, for being dead code — not for anything about the loop.      *)
(*                                                                     *)
(* `DEAD-OWNER!` (checker.f:8519) is NOT this rule.  It only records    *)
(* which token killed the path so the message can say `after 'EXIT'`;   *)
(* the decision is `LIVE-TOKEN?`.  Measured: `: C16 ( i64 -- i64 ) EXIT *)
(* STEP1 ;` reports `at 'STEP1' after 'EXIT'`.                          *)
(* ------------------------------------------------------------------ *)

Definition dead_closer (t : tok) : bool :=
  match t with
  | TElse | TThen | TLoop | TPlusLoop | TEndof | TEndcase
  | TRepeat | TAgain | TCloseQ => true
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
(* The frame stack.                                                    *)
(* ------------------------------------------------------------------ *)

(* `CF-PUSH`, checker.f:7651-7661.  UNSOUNDNESS CLOSED HERE.  `CFS` is a
   FIXED 32-record array (`create CFS 32 CFS-REC * allot`, checker.f:7601), and
   the guard is `#CFC @ 31 >`: with 32 frames already open the push is refused
   and the definition becomes UNCHECKABLE.  A model with an unbounded frame
   list certifies bodies the checker will not.  Measured: a body with 32 nested
   `IF`/`THEN` pairs certifies (`CHECK-CANDIDATE!` answers -1), 33 answers 1 —
   uncheckable, not rejected — and 33 nested `BEGIN`/`UNTIL` likewise.

   Taking the whole frame is also what makes the refusal write NOTHING: the
   checker's own `CF-CASE` and `CF-QUOT` write their extra slots through
   `CF-TOP` AFTER the push, which after a refusal addresses the frame below.
   See the header for why that cannot change a verdict. *)
Definition cfs_cap : nat := 32.

Definition cf_push (s : st) (f : frame) : st :=
  if Nat.leb cfs_cap (length (st_cfs s))
  then set_unck s
  else put_cfs s (f :: st_cfs s).

(* A frame with the slots `CF-PUSH` itself writes and the rest inert.  The
   locals mark is `#LOC @ rec CF.LN !` (checker.f:7656). *)
Definition mkf (s : st) (k : nat) (sa sb ra rb : stack) : frame :=
  MkFrame k sa sb ra rb false (length (st_loc s))
          (SRow 0) (SRow 0) false false false.

(* `CF-LOC-REST`, checker.f:7672: a local bound inside a branch, a loop body, a
   `case` arm, a `match` arm or a quotation goes out of scope at that
   construct's closer.  Measured: `P2 ( i64 -- i64 ) MK-BOOL IF {: x:i64 :} x
   THEN` certifies, and `P4 ( ... ) MK-BOOL IF {: x:i64 :} x THEN x` answers 1
   — after `THEN` the name is not a local, so it is an unknown word, which is
   uncheckable rather than a reject.  `CF-ELSE` restores it too, so
   `P6 ( ... ) MK-BOOL IF {: x:i64 :} x ELSE x THEN` also answers 1. *)
Definition loc_rest (s : st) (f : frame) : st :=
  put_loc s (skipn (length (st_loc s) - fr_ln f) (st_loc s)).

(* ------------------------------------------------------------------ *)
(* Branches.                                                           *)
(* ------------------------------------------------------------------ *)

(* `CF-IF`, checker.f:7794.  Consume the flag, THEN push the frame, so the
   frame records the row the two arms both start from.  (The `uniform<bool>`
   arm and the `CF.UNI` mark are the omitted block-uniform path.) *)
Definition do_if (s : st) : st :=
  let s := step_bool_in s in
  cf_push s (mkf s 1 (st_dcur s) (st_dcur s) (st_rcur s) (st_rcur s)).

(* `CF-ELSE`, checker.f:7849.  Save the if-arm's ROW into SB/RB and its
   DEADNESS into DED, then rewind the registers to the frame's entry row so the
   else arm starts where the if arm did.  The frame becomes kind 2, and the
   locals bound in the if arm go out of scope. *)
Definition do_else (s : st) : st :=
  match st_cfs s with
  | f :: rest =>
      if Nat.eqb (fr_knd f) 1
      then
        let f' := MkFrame 2 (fr_sa f) (st_dcur s) (fr_ra f) (st_rcur s)
                          (st_dead s) (fr_ln f)
                          (fr_xro f) (fr_xrr f) (fr_xst f) (fr_xdp f) (fr_txs f) in
        let s := put_cfs s (f' :: rest) in
        let s := put_dr s (fr_sa f) (fr_ra f) in
        let s := put_dead s false in
        loc_rest s f
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
   ordinary unification mismatch pinned on the `THEN` token.  Measured, both
   halves: `T7 ( mbool -- n ) MATCH mbool yes OF ENDOF no OF ENDOF ;MATCH` is
   refused with `at 'ENDOF' bad match: branch output mismatch expected: bool
   actual: n`, while `: C2 ( i64 -- i64 ) MK-BOOL IF STEP1 ELSE DROP1 MK-CELL
   THEN ;` is refused with a bare `at 'THEN' expected: i64 actual: cell`. *)
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
        put_cfs (loc_rest s f) rest
      else if Nat.eqb (fr_knd f) 2
      then put_cfs (loc_rest (merge_else s f) f) rest
      else fail s
  | [] => fail s
  end.

(* ------------------------------------------------------------------ *)
(* `begin` loops.                                                      *)
(* ------------------------------------------------------------------ *)

(* `CF-BEGIN`, checker.f:7895. *)
Definition do_begin (s : st) : st :=
  cf_push s (mkf s 3 (st_dcur s) (st_dcur s) (st_rcur s) (st_rcur s)).

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
        put_cfs (loc_rest s f) rest
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
        put_dead (put_cfs (loc_rest s f) rest) true
      else fail s
  | [] => fail s
  end.

(* `CF-WHILE`, checker.f:7909.  Consume the flag and RECORD the row at that
   point in SB/RB; the frame becomes kind 4 and stays open — so, alone among
   the closers, `while` does NOT restore the locals mark. *)
Definition do_while (s : st) : st :=
  let s := step_bool_in s in
  match st_cfs s with
  | f :: rest =>
      if Nat.eqb (fr_knd f) 3
      then put_cfs s (MkFrame 4 (fr_sa f) (st_dcur s) (fr_ra f) (st_rcur s)
                              (fr_ded f) (fr_ln f)
                              (fr_xro f) (fr_xrr f) (fr_xst f) (fr_xdp f) (fr_txs f)
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
        put_cfs (loc_rest s f) rest
      else fail s
  | [] => fail s
  end.

(* ------------------------------------------------------------------ *)
(* `do` loops.                                                         *)
(*                                                                     *)
(* A SECOND loop shape, and it differs from `begin` in three ways that *)
(* each decide programs.                                               *)
(*                                                                     *)
(*   - it consumes TWO `n` cells (limit and index) at `do`;            *)
(*   - the loop EXIT is always live even when the body's fall-through  *)
(*     is dead, because a zero-trip `?do` and a `leave` both arrive    *)
(*     there — so `CF-LOOP` revives the path instead of joining;       *)
(*   - the loop indices `i` and `j` are not on the typed rows at all.  *)
(*     They are produced out of thin air, and the only thing checked   *)
(*     is that an enclosing `do` frame EXISTS.                         *)
(*                                                                     *)
(* `do` and `?do` are the SAME rule: `CF-TOK?` (checker.f:8375-8376)   *)
(* sends both to `CF-DO`.  The zero-trip difference is a runtime one.  *)
(* ------------------------------------------------------------------ *)

(* `CF-DO`, checker.f:7922.  Measured: `: D1 ( i64 -- i64 ) MK-N MK-N do STEP1
   loop ;` certifies, `: D2 ... do DUP1 loop ;` is refused with `at 'loop'
   expected: i64 actual: i64 i64`, and `: D3 ... ?do STEP1 loop ;` certifies. *)
Definition do_do (s : st) : st :=
  let s := step_nn_in s in
  cf_push s (mkf s 5 (st_dcur s) (st_dcur s) (st_rcur s) (st_rcur s)).

(* `CF-LOOP` / `CF-+LOOP`, checker.f:7929-7941.  The comment at
   checker.f:7925-7928 states the rule and the code implements it: a dead
   fall-through means the back edge is never taken, so the body-vs-`do`-point
   unification is SKIPPED — but the loop-exit row is the `do`-point row either
   way.  Measured: `: Z1 ( i64 -- i64 ) MK-N MK-N do DROP1 MK-I64 EXIT loop ;`
   certifies, and `: Z2 ... do DUP1 EXIT loop ;` is refused at `loop` (its
   `EXIT` row and the loop-exit row disagree at the fold).

   `+loop` first consumes the increment, and it does so even on a dead path,
   because `+loop` IS a dead closer.  Measured: `: Z3 ( i64 -- i64 ) MK-N MK-N
   do EXIT +loop ;` certifies — the `STEP-N-IN` runs against the dead row and
   its result is then thrown away by `CF@A DCUR !`. *)
Definition loop_close (s : st) : st :=
  match st_cfs s with
  | f :: rest =>
      if Nat.eqb (fr_knd f) 5
      then
        let s := if st_dead s
                 then put_dead s false
                 else let s := suni s (fr_sa f) in rsuni s (fr_ra f) in
        let s := put_dr s (fr_sa f) (fr_ra f) in
        put_cfs (loc_rest s f) rest
      else fail s
  | [] => fail s
  end.

Definition do_loop (s : st) : st := loop_close s.
Definition do_plus_loop (s : st) : st := loop_close (step_n_in s).

(* `CF-I` and `CF-J`, checker.f:7942-7951.  `i` needs one enclosing `do` frame
   anywhere on `CFS`, `j` needs two; both then push an `n` out of nothing,
   because loop control is not carried on the typed rows.  Measured:
   `: D8 ( -- ) MK-N MK-N do i DROP-N loop ;` certifies, `: D9 ( -- ) i
   DROP-N ;` is refused at `i`, `: D10 ( -- ) MK-N MK-N do j DROP-N loop ;` is
   refused at `j`, and the doubly-nested `: D11` certifies.

   A DIVERGENCE FROM `leave`, and it is in the code, not in any document: this
   scan walks EVERY open frame, while `CF-FINDDO` (checker.f:7955-7962) stops
   at a quotation frame.  So `i` inside `[: ;]` inside a `do` loop is accepted
   and `leave` there is not.  Measured: `: L10 ( -- n ) MK-N MK-N do [: i ;]
   drop loop MK-N ;` certifies (exit 0) while `: L9 ( i64 -- i64 ) MK-N MK-N do
   [: leave ;] drop loop ;` is refused at `leave`. *)
Definition count_do (l : list frame) : nat :=
  length (filter (fun f => Nat.eqb (fr_knd f) 5) l).

Definition do_i (s : st) : st :=
  if Nat.ltb 0 (count_do (st_cfs s)) then step_n_out s else fail s.

Definition do_j (s : st) : st :=
  if Nat.ltb 1 (count_do (st_cfs s)) then step_n_out s else fail s.

(* `CF-FINDDO`, checker.f:7955-7962: the nearest enclosing `do` frame, stopping
   at a quotation boundary so a `leave` inside `[: ;]` cannot escape. *)
Fixpoint find_do (l : list frame) : option frame :=
  match l with
  | [] => None
  | f :: rest =>
      if Nat.eqb (fr_knd f) 5 then Some f
      else if Nat.eqb (fr_knd f) 6 then None
      else find_do rest
  end.

(* `CF-LEAVE`, checker.f:7967-7972.  The stack at `leave` must already match
   the loop-exit row, which is the `do`-point row, and the path to `loop` is
   then dead.  Measured: `: L1 ( i64 -- i64 ) MK-N MK-N do MK-BOOL IF leave
   THEN STEP1 loop ;` certifies, `: L3 ... do DUP1 leave loop ;` is refused
   with `at 'leave' expected: i64 actual: i64 i64`, `: L4 ( i64 -- i64 )
   leave ;` is refused at `leave`, and `: L5 ... do leave STEP1 loop ;` is
   refused with `at 'STEP1' after 'leave'`. *)
Definition do_leave (s : st) : st :=
  match find_do (st_cfs s) with
  | None => fail s
  | Some f =>
      let s := suni s (fr_sa f) in
      let s := rsuni s (fr_ra f) in
      put_dead s true
  end.

(* `CF-UNLOOP`, checker.f:7893: `: CF-UNLOOP ( -- ) ;`.  Loop control is not on
   the typed rows, so discarding it is a typing no-op.  Measured:
   `: L6 ( i64 -- i64 ) MK-N MK-N do unloop leave loop ;` certifies. *)
Definition do_unloop (s : st) : st := s.

(* ------------------------------------------------------------------ *)
(* `case`.                                                             *)
(*                                                                     *)
(* Two frame kinds: 7 for the `case` itself, 8 for one `of` arm.  The   *)
(* kind-7 frame doubles as the JOIN ACCUMULATOR — `CF.SB`/`CF.RB` hold  *)
(* the rows the first live arm produced and `CF.DED` says whether       *)
(* anything has been accumulated yet (the helpers at                    *)
(* checker.f:7630-7648 name exactly those three slots).                 *)
(*                                                                     *)
(* The SELECTOR is what makes `case` unlike `if`.  It sits on the row   *)
(* throughout: `CF-OF` consumes the test value, checks that producing   *)
(* it was neutral, and consumes the selector; `CF-ENDOF` restores the   *)
(* row INCLUDING the selector so the next `of` can test it again; and   *)
(* `CF-ENDCASE` drops it on the default path.                           *)
(* ------------------------------------------------------------------ *)

(* `CF-CASE`, checker.f:7801-7803.  `0 CF-TOP CF.DED !` clears the accumulator
   flag; SB/RB start as the literal 0 and are only ever read once DED is set,
   so the placeholder row below is faithful. *)
Definition do_case (s : st) : st :=
  cf_push s (mkf s 7 (st_dcur s) (SRow 0) (st_rcur s) (SRow 0)).

(* `CF-CASE-ACCUM`, checker.f:7805-7815.  The join, and it is EXACTLY the
   `match` join's shape minus the reason code: a rejected or dead path
   contributes nothing, the first live arm installs, every later live arm must
   unify at `UK-EXACT`.  Returns the state and the updated accumulator frame. *)
Definition acc_set (f : frame) (d r : stack) : frame :=
  MkFrame (fr_knd f) (fr_sa f) d (fr_ra f) r true (fr_ln f)
          (fr_xro f) (fr_xrr f) (fr_xst f) (fr_xdp f) (fr_txs f).

Definition case_accum (s : st) (f : frame) : st * frame :=
  if negb (st_ok s) then (s, f)
  else if st_dead s then (s, f)
  else if fr_ded f
       then (rsuni (suni s (fr_sb f)) (fr_rb f), f)
       else (s, acc_set f (st_dcur s) (st_rcur s)).

(* `CF-BELOW-CASE?`, checker.f:7626-7628: is the frame UNDER the top a `case`? *)
Definition below_case (l : list frame) : bool :=
  match l with
  | _ :: g :: _ => Nat.eqb (fr_knd g) 7
  | _ => false
  end.

(* `CF-OF`, checker.f:7817-7824.  Measured: `: K1 ( n -- n ) case MK-N of MK-N
   endof MK-N of MK-N endof MK-N endcase ;` certifies, and `: K8 ( n -- n )
   case DUP1 MK-N of ... ;` is refused with `at 'of' expected: n actual: i64
   i64` — the test expression must be neutral apart from the one `n` it
   produces. *)
Definition do_of_case (s : st) : st :=
  match st_cfs s with
  | f :: rest =>
      if Nat.eqb (fr_knd f) 7
      then
        let s := step_n_in s in
        let s := suni s (fr_sa f) in
        let s := rsuni s (fr_ra f) in
        let s := step_n_in s in
        cf_push s (mkf s 8 (fr_sa f) (SRow 0) (fr_ra f) (SRow 0))
      else fail s
  | [] => fail s
  end.

(* `CF-ENDOF`, checker.f:7826-7834.  Accumulate into the `case` frame two
   below, revive the path, and continue from the row the arm was entered at —
   which still carries the selector.  Measured: `: K10 ( n -- cell ) case MK-N
   of MK-U8 endof MK-N of MK-CELL endof drop MK-CELL endcase ;` is refused with
   `at 'endof' expected: u8 actual: cell`: the arms join EXACTLY. *)
Definition do_endof_case (s : st) : st :=
  match st_cfs s with
  | f :: g :: rest =>
      if below_case (st_cfs s) && Nat.eqb (fr_knd f) 8
      then
        let '(s, g') := case_accum s g in
        let s := loc_rest s f in
        let s := put_dead s false in
        let s := put_cfs s (g' :: rest) in
        put_dr s (fr_sa f) (fr_ra f)
      else fail s
  | _ => fail s
  end.

(* `CF-ENDCASE`, checker.f:7836-7847.  The default path drops the selector,
   joins the accumulator, and the `case` yields the accumulated rows — or, if
   NOTHING was accumulated because every path left early, the path is dead.
   Measured: `: K13 ( n -- cell ) case MK-N of MK-CELL EXIT endof MK-N of
   MK-CELL EXIT endof drop MK-CELL EXIT endcase ;` certifies (all paths exited,
   so the early-return fold supplies the output), while `: K5 ( n -- n ) case
   MK-N of MK-N endof endcase ;` is refused with `at 'endcase' expected: n
   actual:`. *)
Definition do_endcase (s : st) : st :=
  match st_cfs s with
  | f :: rest =>
      if Nat.eqb (fr_knd f) 7
      then
        let s := if st_dead s then s else step_n_in s in
        let '(s, f') := case_accum s f in
        let s := if fr_ded f'
                 then put_dead (put_dr s (fr_sb f') (fr_rb f')) false
                 else put_dead s true in
        put_cfs (loc_rest s f') rest
      else fail s
  | [] => fail s
  end.

(* ------------------------------------------------------------------ *)
(* The `MATCH` eliminator.                                             *)
(*                                                                     *)
(* `MATCH family  variant OF ... ENDOF ...  ;MATCH`.  Two frame kinds   *)
(* again — 9 for the form, 10 for one branch — plus a parallel `MF`     *)
(* record holding the join accumulator and the seen-tag set.  `of` and  *)
(* `endof` are the SAME tokens `case` uses and are told apart by the    *)
(* frame kind (`CF-ENDOF-DISPATCH`, checker.f:8348-8351).               *)
(*                                                                     *)
(* Three things here exist nowhere else in the checker.                 *)
(*                                                                     *)
(*   - PAYLOAD REFINEMENT: a branch starts at the row the scrutinee's   *)
(*     bundle was popped down to, plus THAT variant's payload           *)
(*     (`MATCH-OF-TOK`, checker.f:8330-8342).  So the arms of one       *)
(*     `match` legitimately start from DIFFERENT rows, which no other   *)
(*     construct allows.                                                *)
(*   - EXHAUSTIVENESS: `MATCH-SEMI` (checker.f:8296) requires every     *)
(*     declaration-order tag to have been branched on.  v1 has no       *)
(*     default branch.                                                  *)
(*   - `MD-JOIN`: the ONE place a branch-join mismatch gets a named     *)
(*     reason code (`MATCH-ACCUM`, checker.f:8278).                      *)
(*                                                                     *)
(* Every structural failure latches `MREJ`, a HARD reject that outranks *)
(* `UNCK` — and the depth guard at checker.f:8255 refuses at `#CFC > 30` *)
(* precisely so a `match` can never reach `CF-PUSH`'s uncheckable       *)
(* overflow with only one of its two frames installed.                  *)
(* ------------------------------------------------------------------ *)

Definition MD_FAM_UNKNOWN : nat := 1.

(* `MDIAG!`, checker.f:8186-8189: the first reason wins, and only while the
   failure pin is still open. *)
Definition mdiag_set (s : st) (code : nat) : st :=
  if negb (Nat.eqb (st_mdiag s) 0) then s
  else if st_failset s then s
  else put_mdiag s code.

(* `MATCH-REJECT`, checker.f:8177. *)
Definition match_reject (s : st) : st := put_mm (set_hard s) 0.

(* `MATCH-BEGIN`, checker.f:8180. *)
Definition do_match (s : st) : st := put_mm s 1.

(* `MATCH-SCRUT?`, checker.f:8236-8246, ABSTRACTED.  The checker walks a
   width-expanded hidden-field bundle of the family cell by cell and answers
   the row below it; `Effects.v` has no layouts, so the model requires one
   arity-0 `TFam` cell of the family and answers the row below THAT.  Every
   rule above this pop is modelled exactly. *)
Definition match_scrut (s : st) (f : fam) : option stack :=
  match resolve_row (st_sub s) (st_dcur s) with
  | SPush t rest =>
      match resolve_ty (st_sub s) t with
      | TFam k => if Nat.eqb k (fam_id f) then Some rest else None
      | _ => None
      end
  | SRow _ => None
  end.

(* `MATCH-FAM-TOK`, checker.f:8252-8270. *)
Definition do_fam_tok (s : st) (f : fam) : st :=
  if Nat.ltb 30 (length (st_cfs s))
  then match_reject (mdiag_set s MD_DEPTH)
  else
    match match_scrut s f with
    | None =>
        (* MATCH-SCRUT-DIAG, checker.f:8241-8249 *)
        let code := if Nat.ltb 0 (qdepth s) then MD_QUOT else MD_SCRUT in
        match_reject (mdiag_set s code)
    | Some base =>
        let s := put_mf s (MkMF f base (st_rcur s) base (st_rcur s) false [] :: st_mf s) in
        let s := put_d s base in
        let s := cf_push s (mkf s 9 (st_dcur s) (SRow 0) (st_rcur s) (SRow 0)) in
        put_mm s 2
    end.

(* `MATCH-ACCUM`, checker.f:8271-8284.  The join.  `MD-JOIN` is latched only
   when THIS unification is the one that failed — `fs0` is the failure pin as
   it stood before, so an earlier failure inside the branch keeps its own
   diagnostic.  That guard is the whole reason `FAILSET` is modelled. *)
Definition mf_set (m : mframe) (d r : stack) : mframe :=
  MkMF (mf_fam m) (mf_base m) (mf_rbase m) d r true (mf_seen m).

Definition match_accum (s : st) (m : mframe) : st * mframe :=
  if negb (st_ok s) then (s, m)
  else if st_dead s then (s, m)
  else if mf_has m
       then
         let fs0 := st_failset s in
         let s := rsuni (suni s (mf_out m)) (mf_rout m) in
         let s := if negb (st_ok s) && negb fs0 && Nat.eqb (st_mdiag s) 0
                  then put_mdiag s MD_JOIN else s in
         (s, m)
       else (s, mf_set m (st_dcur s) (st_rcur s)).

(* `MATCH-VARIANT-TOK`, checker.f:8318-8328.  A duplicate variant is a hard
   reject; an unknown one leaves no pending variant and still expects an `OF`.
   Measured: `T4 ( mres -- n ) MATCH mres ok OF ENDOF ok OF ENDOF ;MATCH` is
   refused with `at 'ok' bad match: duplicate variant`. *)
Definition do_var_tok (s : st) (v : nat) : st :=
  match st_mf s with
  | [] => match_reject (mdiag_set s MD_STRAY)
  | m :: rest =>
      if Nat.leb (fam_vcnt (mf_fam m)) v
      then put_mm (put_mpend (match_reject (mdiag_set s MD_VAR_UNKNOWN)) None) 3
      else
        let s := if mem_nat v (mf_seen m)
                 then set_hard (mdiag_set s MD_VAR_DUP) else s in
        let m' := MkMF (mf_fam m) (mf_base m) (mf_rbase m) (mf_out m) (mf_rout m)
                       (mf_has m) (v :: mf_seen m) in
        put_mm (put_mpend (put_mf s (m' :: rest)) (Some v)) 3
  end.

(* `MATCH-OF-TOK`, checker.f:8330-8342.  A token other than `of` here latches
   `MD-MISSING-OF` and rejects, and the form then continues as if the `of` had
   been there.  Measured: `T9 ( mres -- n ) MATCH mres ok DROP-N OF ENDOF err
   OF ENDOF ;MATCH` is refused with `at 'DROP-N' bad match: variant token must
   be followed by OF`.  (Through `bin/hb --load` the ENGINE refuses the same
   text earlier with `hb: match: expected of: ENDOF`, so this arm is only
   reachable through `CHECK-CANDIDATE!`.) *)
Definition push_list (base : stack) (ts : list ty) : stack :=
  fold_left (fun x t => SPush t x) ts base.

Definition do_of_match (s : st) (is_of : bool) : st :=
  let s := if is_of then s else set_hard (mdiag_set s MD_MISSING_OF) in
  match st_mf s with
  | [] => match_reject (mdiag_set s MD_STRAY)
  | m :: _ =>
      let s := put_dr s (mf_base m) (mf_rbase m) in
      let s := match st_mpend s with
               | None => s
               | Some v => put_d s (push_list (mf_base m)
                                      (nth v (fam_pay (mf_fam m)) []))
               end in
      let s := cf_push s (mkf s 10 (mf_base m) (SRow 0) (mf_rbase m) (SRow 0)) in
      put_mm (put_dead s false) 0
  end.

(* `MATCH-ENDOF`, checker.f:8286-8294. *)
Definition do_endof_match (s : st) : st :=
  match st_cfs s, st_mf s with
  | f :: rest, m :: mrest =>
      let '(s, m') := match_accum s m in
      let s := loc_rest s f in
      let s := put_cfs s rest in
      let s := put_dead s false in
      let s := put_mf s (m' :: mrest) in
      let s := put_dr s (mf_base m') (mf_rbase m') in
      put_mm s 2
  | _, _ => match_reject (mdiag_set s MD_STRAY)
  end.

(* `MSEEN-FULL?`: every declaration-order tag branched on. *)
Definition seen_fullb (m : mframe) : bool :=
  forallb (fun v => mem_nat v (mf_seen m)) (seq 0 (fam_vcnt (mf_fam m))).

(* `MATCH-SEMI`, checker.f:8296-8316.  Exhaustiveness, then the join result —
   or, when no branch contributed because every one of them left early, a dead
   path with the base rows restored.  Measured: `T2 ( mres -- n ) MATCH mres ok
   OF ENDOF ;MATCH` is refused with `at ';MATCH' bad match: missing variants:
   err`, `T6 ( mthree -- n ) MATCH mthree one OF ENDOF two OF ENDOF ;MATCH`
   with `missing variants: three`, and `TB ( mres -- n ) MATCH mres ok OF EXIT
   ENDOF err OF EXIT ENDOF ;MATCH` certifies. *)
Definition do_semimatch (s : st) : st :=
  match st_cfs s, st_mf s with
  | f :: rest, m :: mrest =>
      if negb (Nat.eqb (fr_knd f) 9)
      then match_reject (mdiag_set s MD_STRAY)
      else
        let s := if seen_fullb m then s
                 else let s := if Nat.eqb (st_mdiag s) 0 && negb (st_failset s)
                               then put_mdiag s MD_NONEXH else s in
                      set_hard s in
        let s := if mf_has m
                 then put_dead (put_dr s (mf_out m) (mf_rout m)) false
                 else put_dead (put_dr s (mf_base m) (mf_rbase m)) true in
        let s := loc_rest s f in
        put_mm (put_mf (put_cfs s rest) mrest) 0
  | _, _ => match_reject (mdiag_set s MD_STRAY)
  end.

(* ------------------------------------------------------------------ *)
(* Early return, the throw edge, and no-return words.                  *)
(* ------------------------------------------------------------------ *)

(* `CF-EXIT`, checker.f:7887.  An early return does NOT meet the declared
   output here.  It meets the OTHER early returns, in ONE per-definition
   accumulator, at `UK-EXACT`; the fall-through row joins the same accumulator
   at `;` (`CHECK-FOLD-EXITS`); and only the joined result is coerced against
   the declaration.  The accumulator is per-DEFINITION, not per-frame, so two
   `exit`s in unrelated branches still meet each other directly.  `CF-EXIT`
   calls BARE `UNIFY`, not `SUNI`, so it takes no failure pin. *)
Definition do_exit (s : st) : st :=
  let s := if st_xset s
           then let s := uni UkExact s (st_dcur s) (st_xrow s) in
                uni UkExact s (st_rcur s) (st_xrrow s)
           else put_x s true (st_dcur s) (st_rcur s) in
  put_dead s true.

(* `THROW-EDGE`, checker.f:1986-1988.  The FIRST throw point wins outright:
   later throws do not unify against it, they only re-set the latch.  The rows
   it captures are stored and never consulted (see the header), so only the
   latch is modelled. *)
Definition throw_edge (s : st) : st := put_thset s true.

(* `throw` (`PRIM: throw PE-N PE-IN`, checker.f:4934) and `die`
   (`PRIM: die PE-PTR-U8 PE-IN PE-N PE-IN PE-N PE-IN`, checker.f:4935) are
   recognised BY NAME in `THROW-CUR?` and `DEAD-CUR?` (checker.f:6359-6365).
   `DO-TOK1` (checker.f:9094-9095) applies the effect first, then raises the
   throw edge, then kills the path — each gated on OK.

   `throw` is BOTH: it raises the edge and kills the path.  `die` only kills.
   And the difference between a dead path and an early return matters at the
   boundary: `exit` feeds the output accumulator, `throw` does not.  Measured:
   `: T2 ( i64 -- i64 ) MK-N throw ;` certifies while
   `: T1 ( i64 -- i64 ) DROP1 MK-N throw ;` is refused with `at 'throw'
   expected: i64 actual:` — the dead row still had to satisfy the declaration.
   `: T4 ( i64 -- i64 ) MK-N throw STEP1 ;` is refused with `at 'STEP1' after
   'throw'`, and `: T5 ( i64 -- cell ) MK-BOOL IF MK-N throw THEN DROP1
   MK-CELL ;` certifies because `THEN` excludes the dead arm. *)
Definition throw_prim : word_eff := prim 1 [nt] [].
Definition die_prim : word_eff := prim 1 [TPtr u8; nt; nt] [].

Definition after_ctl (s : st) (dead throw : bool) : st :=
  let s := if st_ok s && throw then throw_edge s else s in
  if st_ok s && dead then put_dead s true else s.

Definition do_throw (s : st) : st := after_ctl (apply_eff s throw_prim) true true.
Definition do_die (s : st) : st := after_ctl (apply_eff s die_prim) true false.

(* `CF-RECURSE` / `CF-RECURSE-EFF`, checker.f:7758-7770.  With a cached
   signature this is an ordinary call to the word's own declared effect,
   freshly instantiated.  Without one it is UNCHECKABLE, not a reject: the
   `RECURSE-CACHE?` guard falls through to `-1 UNCK !`. *)
Definition do_recurse (c : cfg) (s : st) : st :=
  if cfg_sig c then apply_eff s (decl_eff (cfg_decl c)) else set_unck s.

(* ------------------------------------------------------------------ *)
(* Quotations.                                                         *)
(* ------------------------------------------------------------------ *)

(* `CF-QUOT`, checker.f:7971-7985.  `[:` opens a nested inference: save the
   outer rows, the outer BASE rows, the outer EARLY-RETURN state and the outer
   THROW latch, then start the quotation body on two fresh rows with all three
   cleared.

   Clearing `XSET` and `DEADP` is what makes `exit` QUOTATION-SCOPED: an `exit`
   inside `[: ... ;]` returns from the quotation, contributes to the
   quotation's own effect, and leaves the enclosing definition live.
   Measured: `: C23 ( -- i64 ) [: MK-I64 EXIT ;] execute ;` certifies.
   Clearing `THSET` does the same for the throw edge: a quotation that throws
   raises the edge in whoever EXECUTES it, not in whoever built it. *)
Definition do_quot (s : st) : st :=
  let f := MkFrame 6 (st_dcur s) (st_brow s) (st_rcur s) (st_rbrow s)
                   false (length (st_loc s))
                   (st_xrow s) (st_xrrow s) (st_xset s) (st_dead s) (st_thset s) in
  let s := cf_push s f in
  let (b, s) := fresh_id s in
  let (r, s) := fresh_id s in
  let s := put_base s (SRow b) (SRow r) in
  let s := put_dr s (SRow b) (SRow r) in
  let s := put_x s false (st_xrow s) (st_xrrow s) in
  let s := put_thset s false in
  put_dead s false.

(* `CHECK-FOLD-EXITS`, checker.f:9441-9445, and the identical fold inside
   `CF-SEMIQ`: if any `exit` fired, either the fall-through joins the
   accumulator, or — when every path exited — the accumulator IS the output. *)
Definition fold_exits (s : st) : st :=
  if st_xset s
  then (if st_dead s
        then put_dr s (st_xrow s) (st_xrrow s)
        else let s := uni UkExact s (st_dcur s) (st_xrow s) in
             uni UkExact s (st_rcur s) (st_xrrow s))
  else s.

(* `CF-SEMIQ`, checker.f:7987-8002.  Fold the quotation's own exits, build the
   quotation term from its base and current rows AND its two control flags,
   restore everything the frame saved, and push the term onto the OUTER row.

   `QX!` (checker.f:7994) computes those flags exactly like this:
     `Q>XHAS`  = `THSET`, the quotation raised a throw edge;
     `Q>XDEAD` = `DEADP AND NOT XSET`, the quotation has NO normal return.
   The `NOT XSET` matters: a body that ends dead but recorded an `exit` DOES
   have a normal return — the fold above just installed it. *)
Definition do_semiq (s : st) : st :=
  match st_cfs s with
  | f :: rest =>
      if Nat.eqb (fr_knd f) 6
      then
        let s := fold_exits s in
        let q := TQuot (st_thset s) (st_dead s && negb (st_xset s))
                       (Eff (st_brow s) (st_dcur s) (st_rbrow s) (st_rcur s)) in
        let s := put_x s (fr_xst f) (fr_xro f) (fr_xrr f) in
        let s := put_dead s (fr_xdp f) in
        let s := put_thset s (fr_txs f) in
        let s := put_base s (fr_sb f) (fr_rb f) in
        let s := put_dr s (SPush q (fr_sa f)) (fr_ra f) in
        put_cfs (loc_rest s f) rest
      else fail s
  | [] => fail s
  end.

(* Pop an execution token off the data row, at `UK-EXACT`, and resolve it.
   Shared by `RSEXEC` (checker.f:2003) and `RSCATCH` (checker.f:2039).  Both
   use bare `UNIFY`, so no failure pin. *)
Definition pop_xt (s : st) : ty * st :=
  let (v, s) := fresh_id s in
  let (r, s) := fresh_id s in
  let rest := SRow r in
  let s := uni UkExact s (st_dcur s) (SPush (TVar v) rest) in
  let s := put_d s rest in
  (resolve_ty (st_sub s) (TVar v), s).

(* `RSEXEC`, checker.f:2003-2037 — quotation APPLICATION.  UNSOUNDNESS CLOSED
   HERE.  The quotation's declared rows meet the live rows at `UK-INPUT` (so an
   argument may widen into the quotation, just as at any call).  What happens
   NEXT depends on the quotation's control flags, and the previous revision of
   this file could not see them:

     `Q>XHAS`  -> raise the CALLER's throw edge (checker.f:2015-2017);
     `Q>XDEAD` -> KILL the caller's path and DO NOT install the output rows
                  (checker.f:2018-2019);
     otherwise -> install the output rows and conserve linears.

   Measured, and this is the fixture the old model got wrong:
   `: X2 ( i64 -- i64 i64 ) [: DUP1 MK-N throw ;] execute ;` is refused with
   `at 'execute' expected: i64 i64 actual: i64` — the quotation's `i64 i64`
   output row was NOT installed — while the old model installed it and would
   have certified.  In the other direction `: X1 ( i64 -- i64 ) [: MK-N throw
   ;] execute STEP1 ;` is refused with `at 'STEP1' after 'throw'`: the caller
   went dead, where the old model continued live.

   The failure arm is worth its own name.  When the popped value resolves to a
   bare type variable, it is an execution token of unknown provenance — one
   fetched from untyped memory — and executing it would launder whatever it
   really does past the checker.  The checker rejects it with a dedicated
   diagnostic (`MD-EXEC-OPAQUE`, checker.f:8171).  Both that arm and the
   "not a quotation at all" arm set `OK = 0`, so they are one case here. *)
Definition do_exec (s : st) : st :=
  let (t, s) := pop_xt s in
  match t with
  | TQuot xhas xdead (Eff qd qo qr qro) =>
      let explicit := lin_explicitb (st_sub s) qd qo
                      || lin_explicitb (st_sub s) qr qro in
      let before := lin_snapshot s in
      let s := suni_in s qd in
      let s := rsuni_in s qr in
      let s := if xhas then throw_edge s else s in
      if xdead then put_dead s true
      else
        let s := put_dr s qo qro in
        if explicit then s else lin_check s before
  | _ => fail s
  end.

(* `RSCATCH`, checker.f:2039-2073.  The same application, but the quotation
   must be STACK PRESERVING: its outputs are unified against the live rows
   instead of replacing them, and a throw code (`n`) is then pushed.  The
   control flags split the cases three ways:

     not `Q>XDEAD`            -> unify the outputs, push the code;
     `Q>XDEAD` and `Q>XHAS`   -> the quotation ALWAYS throws, so `catch` always
                                 recovers: no output unification, push the code;
     `Q>XDEAD` and not `Q>XHAS` -> the quotation never returns at all (it died,
                                 or looped forever): the caller goes dead and
                                 NO code is pushed (checker.f:2051-2052).

   `catch` raises no throw edge: recovering the throw is what it is for.
   Measured: `: X6 ( i64 -- i64 ) [: DUP1 MK-N throw ;] catch DROP-N ;`
   certifies, `: X4 ( i64 -- i64 ) [: BEGIN MK-N DROP-N AGAIN ;] catch ;`
   certifies, and `: X4b ( i64 -- i64 n ) [: BEGIN MK-N DROP-N AGAIN ;]
   catch ;` is refused with `at 'catch' expected: i64 n actual: i64` — no code
   was pushed. *)
Definition do_catch (s : st) : st :=
  let (t, s) := pop_xt s in
  match t with
  | TQuot xhas xdead (Eff qd qo qr qro) =>
      let s := suni_in s qd in
      let s := rsuni_in s qr in
      if xdead
      then (if xhas
            then put_d s (SPush nt (st_dcur s))
            else put_dead s true)
      else
        let s := suni_in s qo in
        let s := rsuni_in s qro in
        put_d s (SPush nt (st_dcur s))
  | _ => fail s
  end.

(* ------------------------------------------------------------------ *)
(* Typed locals, and their branch scope.                               *)
(*                                                                     *)
(* `{: x y :}` allocates one fresh type variable per name              *)
(* (`LOC-ADD`, checker.f:7381) and then binds them all with ONE step    *)
(* whose input row is the base plus those variables, first name         *)
(* deepest (`LOC-BIND`, checker.f:7460-7473).  A reference re-pushes    *)
(* the variable (`LOC-PUSH-REF`, checker.f:7490).                       *)
(*                                                                     *)
(* The CONTROL rule is `CF.LN`: every frame records `#LOC` at its push  *)
(* and every closer restores it, so a local bound inside a branch, a    *)
(* loop body, a `case` arm, a `match` arm or a quotation is gone at     *)
(* that construct's closer.  `while` is the exception, because its      *)
(* frame stays open.                                                    *)
(*                                                                     *)
(* Locals and quotations do not mix at all: `LOC-BEGIN` (checker.f:7484) *)
(* refuses to open a locals group while `QDEPTH > 0`, and `LOC-REF?`     *)
(* (checker.f:7511) refuses a reference there too, both as HARD rejects. *)
(* Measured: `P7 ( i64 -- i64 ) [: {: x:i64 :} x ;] execute` and        *)
(* `P8 ( i64 -- i64 ) {: x:i64 :} [: x ;] drop x` both answer 0.        *)
(* ------------------------------------------------------------------ *)

Fixpoint fresh_vars (n : nat) (s : st) : list ty * st :=
  match n with
  | 0 => ([], s)
  | S k =>
      let (v, s) := fresh_id s in
      let (rest, s) := fresh_vars k s in
      (TVar v :: rest, s)
  end.

(* `LIN-LOCAL-BIND-CHECK`, checker.f:7401-7406: a local that CONCRETELY
   resolves linear at bind time is a hard reject, because a reference re-pushes
   it without any step for `LIN-CHECK` to see.  Measured:
   `Q7 ( ltok -- ltok ) {: x:ltok :} x` answers 0 with
   `E-LINEAR-LOCAL ... linear value cannot be bound to a local`. *)
Fixpoint lin_local_bind_check (s : st) (l : list ty) : st :=
  match l with
  | [] => s
  | t :: rest =>
      let s := match resolve_ty (st_sub s) t with
               | TCon c => if linear_conb c then set_hard s else s
               | _ => s
               end in
      lin_local_bind_check s rest
  end.

Definition do_locals (s : st) (n : nat) : st :=
  if Nat.ltb 0 (qdepth s) then set_hard s
  else
    let (vs, s) := fresh_vars n s in
    (* `vs` is allocated first-name-first; the binding row wants the first name
       DEEPEST, and the locals table wants the last name at its HEAD. *)
    let (b, s) := fresh_id s in
    let base := SRow b in
    let s := checker_step s (push_list base vs) base in
    let s := put_loc s (rev vs ++ st_loc s) in
    lin_local_bind_check s vs.

(* `LOC-REF?`, checker.f:7505-7516, with `LIN-LOCAL-REF-TAINT`
   (checker.f:7411).  An index past the end of the table is not a local at all;
   the checker falls through to `DO-TOK`, finds no word, and the definition
   becomes UNCHECKABLE — which is what the measured `1` verdicts above are. *)
Definition do_locref (s : st) (i : nat) : st :=
  if Nat.ltb 0 (qdepth s) then set_hard s
  else
    match nth_error (st_loc s) i with
    | None => set_unck s
    | Some t =>
        let s := match resolve_ty (st_sub s) t with
                 | TVar v => add_taint s v
                 | _ => s
                 end in
        put_d s (SPush t (st_dcur s))
    end.

(* ------------------------------------------------------------------ *)
(* The return-row transfers.                                           *)
(*                                                                     *)
(* `>r` and `r>` are NOT ordinary calls, and the difference is not      *)
(* cosmetic.  `RS->R` (checker.f:1941-1946) and `RSR>` (checker.f:1947) *)
(* take ONE linear snapshot around the whole transfer, so a linear      *)
(* value crossing between the rows conserves.  An ordinary word with    *)
(* the same declared effect would not: `EFF-APPLY` runs `LIN-CHECK`     *)
(* inside `CHECKER-STEP`, BEFORE the return rows move                   *)
(* (checker.f:4629-4636), so the value is momentarily on neither row.   *)
(* Measured: `Q6 ( ltok -- ltok ) >r r>` certifies.  Both also use bare *)
(* `UNIFY`, so neither takes a failure pin.                             *)
(* ------------------------------------------------------------------ *)

Definition do_to_r (s : st) : st :=
  let before := lin_snapshot s in
  let (v, s) := fresh_id s in
  let (r, s) := fresh_id s in
  let rest := SRow r in
  let s := uni UkExact s (st_dcur s) (SPush (TVar v) rest) in
  let s := put_dr s rest (SPush (TVar v) (st_rcur s)) in
  lin_check s before.

Definition do_from_r (s : st) : st :=
  let before := lin_snapshot s in
  let (v, s) := fresh_id s in
  let (r, s) := fresh_id s in
  let rest := SRow r in
  let s := uni UkExact s (st_rcur s) (SPush (TVar v) rest) in
  let s := put_dr s (SPush (TVar v) (st_dcur s)) rest in
  lin_check s before.

(* ------------------------------------------------------------------ *)
(* The step relation.                                                  *)
(* ------------------------------------------------------------------ *)

(* The MM = 0 dispatch: `CF-TOK?` and then the ordinary word path. *)
Definition step_ctl (c : cfg) (s : st) (t : tok) : st :=
  match t with
  | TCall w => apply_eff s w
  | TCallCtl w dead throw => after_ctl (apply_eff s w) dead throw
  | TThrow => do_throw s
  | TDie => do_die s
  | TIf => do_if s
  | TElse => do_else s
  | TThen => do_then s
  | TBegin => do_begin s
  | TUntil => do_until s
  | TAgain => do_again s
  | TWhile => do_while s
  | TRepeat => do_repeat s
  | TDo => do_do s
  | TLoop => do_loop s
  | TPlusLoop => do_plus_loop s
  | TI => do_i s
  | TJ => do_j s
  | TLeave => do_leave s
  | TUnloop => do_unloop s
  | TCase => do_case s
  | TOf => do_of_case s
  (* `CF-ENDOF-DISPATCH`, checker.f:8348-8351: the frame kind decides. *)
  | TEndof =>
      match st_cfs s with
      | f :: _ => if Nat.eqb (fr_knd f) 10 then do_endof_match s else do_endof_case s
      | [] => do_endof_case s
      end
  | TEndcase => do_endcase s
  | TMatch => do_match s
  (* A family or variant token OUTSIDE a match form is just a word the checker
     does not know; `;match` there is a hard reject (checker.f:8366). *)
  | TFamTok _ => set_unck s
  | TVarTok _ => set_unck s
  | TSemiMatch => match_reject (mdiag_set s MD_STRAY)
  | TExit => do_exit s
  | TRecurse => do_recurse c s
  | TOpenQ => do_quot s
  | TCloseQ => do_semiq s
  | TExec => do_exec s
  | TCatch => do_catch s
  | TLocals n => do_locals s n
  | TLocRef i => do_locref s i
  | TToR => do_to_r s
  | TFromR => do_from_r s
  end.

(* `MATCH-TOK`, checker.f:8344-8347: while `MM` is non-zero EVERY token is
   intercepted, whatever its spelling, which is what makes the missing-`of`
   rule reachable at all. *)
Definition step_match (s : st) (t : tok) : st :=
  if Nat.eqb (st_mm s) 1
  then match t with
       | TFamTok f => do_fam_tok s f
       | _ => match_reject (mdiag_set s MD_FAM_UNKNOWN)
       end
  else if Nat.eqb (st_mm s) 2
  then match t with
       | TSemiMatch => do_semimatch s
       | TVarTok v => do_var_tok s v
       | _ => put_mm (put_mpend (match_reject (mdiag_set s MD_VAR_UNKNOWN)) None) 3
       end
  else match t with
       | TOf => do_of_match s true
       | _ => do_of_match s false
       end.

(* `DO-TOK1`, checker.f:9060-9110, in its own order: the dead-path gate first
   (a token that is not allowed on a dead path is rejected and NOT processed),
   then the match interception, then the ordinary dispatch — and then, always,
   the deferred linear taint scan and the failure-pin latch. *)
Definition post_tok (s : st) : st :=
  let s := lin_taint_scan s in
  if (negb (st_ok s) || st_unck s) && negb (st_failset s)
  then put_failset s true else s.

Definition step (c : cfg) (s : st) (t : tok) : st :=
  post_tok
    (if st_dead s && negb (dead_closer t) then fail s
     else if Nat.eqb (st_mm s) 0 then step_ctl c s t
     else step_match s t).

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
   RBROW unless the signature wrote a `|` clause.  `CHECK-RESET`
   (checker.f:9390-9404) clears everything else. *)
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
       false false [] [] 0 None [] []
       true false false 0 false.

(* `CHECK-VERDICT`, checker.f:9666-9668.  Three outcomes, ranked: any hard
   structural latch is a REJECT whatever else happened, then UNCHECKABLE
   outranks a plain `OK = 0`. *)
Inductive verdict : Type := VCert | VUncheckable | VReject.

Definition verdict_of (s : st) : verdict :=
  if st_hard s then VReject
  else if st_unck s then VUncheckable
  else if st_ok s then VCert else VReject.

Definition certifiedb (v : verdict) : bool :=
  match v with VCert => true | _ => false end.

(* `CHECK`, checker.f:9670-9695, in its own order:

     1. fold the early returns into the output row;
     2. latch `MD-TRUNC` for an unterminated `match` form;
     3. `CHECK-NO-BORROW` — the implicit-row seal, unchanged from Effects.v;
     4. join the reached output against the declared one, at `UK-COERCE`;
     5. reject if any control frame or `match` form is still open;
     6. return-row balance, or the declared return output at `UK-COERCE`.

   Effects.v's `check_body` cites `CHECK-DOES!` (checker.f:10126-10133) and
   runs the return balance BEFORE the output join.  `CHECK` runs it after, with
   the open-frame test in between.  Every one of these is a conjunctive latch on
   `OK`, so the ORDER cannot change a verdict — only which token a diagnostic
   is pinned to.  The order below is `CHECK`'s. *)
Definition finish_st (c : cfg) (s : st) : st :=
  let d := cfg_decl c in
  let e := decl_eff d in
  let s := fold_exits s in
  let s := if negb (Nat.eqb (st_mm s) 0) || negb (Nat.eqb (length (st_mf s)) 0)
           then mdiag_set s MD_TRUNC else s in
  let s := if cfg_sig c
           then (if seal_okb (st_sub s) d then s else fail s)
           else s in
  let s := if cfg_sig c then uni UkCoerce s (st_dcur s) (we_dout e) else s in
  let s := match st_cfs s with [] => s | _ :: _ => fail s end in
  let s := if Nat.eqb (st_mm s) 0 then s else fail s in
  let s := if we_hasr e
           then (if cfg_sig c then uni UkCoerce s (st_rcur s) (we_rout e) else s)
           else (if stack_eqb (resolve_row (st_sub s) (st_rcur s))
                              (resolve_row (st_sub s) (SRow (decl_rbrow d)))
                 then s else fail s) in
  s.

Definition finish (c : cfg) (s : st) : verdict := verdict_of (finish_st c s).

Definition check_ctl (c : cfg) (ts : list tok) : verdict :=
  finish c (run c (init c) ts).

(* The reason code the checker would name, for the examples that pin `MD-JOIN`
   to `match` and to nothing else.  0 is "no named reason".  It reads the latch
   AFTER the boundary, because `CHECK` itself latches `MD-TRUNC` there. *)
Definition check_reason (c : cfg) (ts : list tok) : nat :=
  st_mdiag (finish_st c (run c (init c) ts)).

(* `CHECK`'s control-flag record, checker.f:9696-9701: a CERTIFIED word whose
   body ended dead with no `exit`, or which raised a throw edge, is remembered,
   and every later call site reads those flags back (`DEAD-CUR?` /
   `THROW-CUR?`).  This is how `TCallCtl` arises. *)
Definition ctl_dead (c : cfg) (ts : list tok) : bool :=
  let s := run c (init c) ts in st_dead s && negb (st_xset s).

Definition ctl_throw (c : cfg) (ts : list tok) : bool :=
  st_thset (run c (init c) ts).

(* ------------------------------------------------------------------ *)
(* Definitional examples.                                              *)
(*                                                                     *)
(* Every example below was cross-checked against the shipped checker.   *)
(* Two harnesses were used, and which one matters, because the ENGINE   *)
(* refuses some texts before the checker ever sees them.                *)
(*                                                                     *)
(*   1. `HB_TMP=<scratch> bin/hb --load <fixture>.f ; echo $?`          *)
(*      on top of this prelude, which certifies (exit 0):               *)
(*                                                                     *)
(*        : STEP1   ( i64 -- i64 )      ;                               *)
(*        : MK-U8   ( -- u8 )         0 ;                               *)
(*        : MK-CELL ( -- cell )       0 ;                               *)
(*        : MK-I64  ( -- i64 )        0 ;                               *)
(*        : MK-BOOL ( -- bool )  0 0< ;                                 *)
(*        : DUP1    ( i64 -- i64 i64 ) dup ;                            *)
(*        : DROP1   ( i64 -- )        drop ;                            *)
(*        : MK-N    ( -- n )          0 ;                               *)
(*        : DROP-N  ( n -- )         drop ;                             *)
(*                                                                     *)
(*      Exit 0 means the whole file certified; exit 70 means the        *)
(*      checker rejected a definition and the load hook refused it.     *)
(*                                                                     *)
(*   2. `CHECK-CANDIDATE!` from `test/checker-assert.f`, which runs the *)
(*      CHECKER alone on one definition's text and answers -1           *)
(*      certified / 0 rejected / 1 uncheckable.  This is the only way   *)
(*      to observe the difference between a reject and an uncheckable,  *)
(*      and the only way to reach the rules the engine's own pairing    *)
(*      and `match` guards refuse first.                                *)
(*                                                                     *)
(* The harness, the text and the observed answer are quoted at each     *)
(* example.                                                             *)
(* ------------------------------------------------------------------ *)

(* The callee effects, one per prelude word, over their own row letter. *)
Definition wStep1 : word_eff := prim 1 [i64] [i64].
Definition wMkU8 : word_eff := prim 1 [] [u8].
Definition wMkCell : word_eff := prim 1 [] [cellt].
Definition wMkI64 : word_eff := prim 1 [] [i64].
Definition wMkBool : word_eff := prim 1 [] [boolt].
Definition wDup1 : word_eff := prim 1 [i64] [i64; i64].
Definition wDrop1 : word_eff := prim 1 [i64] [].
Definition wMkN : word_eff := prim 1 [] [nt].
Definition wDropN : word_eff := prim 1 [nt] [].

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

(* --- 3. Stack-neutral `begin` loops --------------------------------- *)

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

(* --- 4. `do` loops, `i`, `j` and `leave` ---------------------------- *)

(* `do` consumes two `n` cells and its body must be neutral, exactly as
   `begin`/`until` — but a `do` body does NOT produce a flag, and `?do` is the
   same rule.

   : D1 ( i64 -- i64 ) MK-N MK-N do  STEP1 loop ;  -> exit 0
   : D3 ( i64 -- i64 ) MK-N MK-N ?do STEP1 loop ;  -> exit 0
   : D2 ( i64 -- i64 ) MK-N MK-N do  DUP1  loop ;  -> exit 70,
       `at 'loop' expected: i64 actual: i64 i64` *)
Example do_loop_must_be_neutral :
  check_ctl (sig [i64] [i64])
            [TCall wMkN; TCall wMkN; TDo; TCall wStep1; TLoop] = VCert
  /\ check_ctl (sig [i64] [i64])
               [TCall wMkN; TCall wMkN; TDo; TCall wDup1; TLoop] = VReject.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* Both bounds are required, and `+loop` takes an increment on top.

   : D4 ( i64 -- i64 ) MK-N do STEP1 loop ;              -> exit 70, `at 'loop'`
   : D5 ( i64 -- i64 ) MK-N MK-N do STEP1 MK-N +loop ;   -> exit 0
   : D6 ( i64 -- i64 ) MK-N MK-N do STEP1 +loop ;        -> exit 70,
       `at '+loop' expected: i64 actual:` *)
Example do_takes_two_bounds_and_plus_loop_an_increment :
  check_ctl (sig [i64] [i64]) [TCall wMkN; TDo; TCall wStep1; TLoop] = VReject
  /\ check_ctl (sig [i64] [i64])
       [TCall wMkN; TCall wMkN; TDo; TCall wStep1; TCall wMkN; TPlusLoop] = VCert
  /\ check_ctl (sig [i64] [i64])
       [TCall wMkN; TCall wMkN; TDo; TCall wStep1; TPlusLoop] = VReject.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* `i` and `j` are not on the typed rows: nothing is popped, an `n` appears,
   and the only test is that enough `do` frames are open.

   : D8  ( -- ) MK-N MK-N do i DROP-N loop ;                     -> exit 0
   : D7  ( -- n ) MK-N MK-N do i loop ;                          -> exit 70,
       `at 'loop' expected: actual: n`   (the body stopped being neutral)
   : D9  ( -- ) i DROP-N ;                                       -> exit 70, `at 'i'`
   : D10 ( -- ) MK-N MK-N do j DROP-N loop ;                     -> exit 70, `at 'j'`
   : D11 ( -- ) MK-N MK-N do MK-N MK-N do j DROP-N loop loop ;   -> exit 0 *)
Example loop_indices_need_enclosing_do_frames :
  check_ctl (sig [] [])
            [TCall wMkN; TCall wMkN; TDo; TI; TCall wDropN; TLoop] = VCert
  /\ check_ctl (sig [] [nt]) [TCall wMkN; TCall wMkN; TDo; TI; TLoop] = VReject
  /\ check_ctl (sig [] []) [TI; TCall wDropN] = VReject
  /\ check_ctl (sig [] [])
       [TCall wMkN; TCall wMkN; TDo; TJ; TCall wDropN; TLoop] = VReject
  /\ check_ctl (sig [] [])
       [TCall wMkN; TCall wMkN; TDo; TCall wMkN; TCall wMkN; TDo;
        TJ; TCall wDropN; TLoop; TLoop] = VCert.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* `leave` must already be at the loop-exit row, and kills the path to `loop`.

   : L1 ( i64 -- i64 ) MK-N MK-N do MK-BOOL IF leave THEN STEP1 loop ;  -> exit 0
   : L2 ( i64 -- i64 ) MK-N MK-N do leave loop ;                        -> exit 0
   : L3 ( i64 -- i64 ) MK-N MK-N do DUP1 leave loop ;                   -> exit 70,
       `at 'leave' expected: i64 actual: i64 i64`
   : L4 ( i64 -- i64 ) leave ;                                          -> exit 70, `at 'leave'`
   : L5 ( i64 -- i64 ) MK-N MK-N do leave STEP1 loop ;                  -> exit 70,
       `at 'STEP1' after 'leave'`
   : L6 ( i64 -- i64 ) MK-N MK-N do unloop leave loop ;                 -> exit 0 *)
Example leave_matches_the_loop_exit_row :
  check_ctl (sig [i64] [i64])
            [TCall wMkN; TCall wMkN; TDo; TCall wMkBool; TIf; TLeave; TThen;
             TCall wStep1; TLoop] = VCert
  /\ check_ctl (sig [i64] [i64]) [TCall wMkN; TCall wMkN; TDo; TLeave; TLoop] = VCert
  /\ check_ctl (sig [i64] [i64])
       [TCall wMkN; TCall wMkN; TDo; TCall wDup1; TLeave; TLoop] = VReject
  /\ check_ctl (sig [i64] [i64]) [TLeave] = VReject
  /\ check_ctl (sig [i64] [i64])
       [TCall wMkN; TCall wMkN; TDo; TLeave; TCall wStep1; TLoop] = VReject
  /\ check_ctl (sig [i64] [i64])
       [TCall wMkN; TCall wMkN; TDo; TUnloop; TLeave; TLoop] = VCert.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* THE LOOP EXIT IS ALWAYS LIVE.  A body whose fall-through is dead does not
   make the code after `loop` dead — a zero-trip `?do` or a `leave` both arrive
   there — so `CF-LOOP` revives the path and takes the `do`-point row.  This is
   the one place a closer REVIVES rather than joins.

   : Z1 ( i64 -- i64 ) MK-N MK-N do DROP1 MK-I64 EXIT loop ;  -> exit 0
   : Z2 ( i64 -- i64 ) MK-N MK-N do DUP1 EXIT loop ;          -> exit 70, `at 'loop'`
   : Z3 ( i64 -- i64 ) MK-N MK-N do EXIT +loop ;              -> exit 0
   : Z6 ( i64 -- i64 ) MK-N MK-N do loop STEP1 ;              -> exit 0 *)
Example the_loop_exit_is_always_live :
  check_ctl (sig [i64] [i64])
            [TCall wMkN; TCall wMkN; TDo; TCall wDrop1; TCall wMkI64; TExit; TLoop]
    = VCert
  /\ check_ctl (sig [i64] [i64])
       [TCall wMkN; TCall wMkN; TDo; TCall wDup1; TExit; TLoop] = VReject
  /\ check_ctl (sig [i64] [i64]) [TCall wMkN; TCall wMkN; TDo; TExit; TPlusLoop]
    = VCert
  /\ check_ctl (sig [i64] [i64])
       [TCall wMkN; TCall wMkN; TDo; TLoop; TCall wStep1] = VCert.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* `loop` and `+loop` ARE dead closers, `leave` and `do` are not.  So the
   `EXIT` above closes at `loop` and the `STEP1` in L5 does not. *)
Example loop_is_a_dead_closer_leave_is_not :
  dead_closer TLoop = true /\ dead_closer TPlusLoop = true
  /\ dead_closer TLeave = false /\ dead_closer TDo = false
  /\ dead_closer TUntil = false /\ dead_closer TSemiMatch = false.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* A `do` frame is a frame like any other: a closer that does not match its
   opener fails by KIND.

   : Z7 ( i64 -- i64 ) do STEP1 loop ;                -> exit 70, `at 'loop'`
   : Z8 ( i64 -- i64 ) MK-N MK-N do STEP1 THEN ;      -> exit 70, `at 'THEN'`
   : A  ( i64 -- i64 ) MK-N MK-N do STEP1 until ;     -> exit 70
   : A  ( i64 -- i64 ) MK-N MK-N do STEP1 repeat ;    -> exit 70 *)
Example a_do_frame_is_closed_only_by_loop :
  check_ctl (sig [i64] [i64]) [TCall wMkN; TCall wMkN; TDo; TCall wStep1; TThen]
    = VReject
  /\ check_ctl (sig [i64] [i64])
       [TCall wMkN; TCall wMkN; TDo; TCall wStep1; TCall wMkBool; TUntil] = VReject
  /\ check_ctl (sig [i64] [i64])
       [TCall wMkN; TCall wMkN; TDo; TCall wStep1; TRepeat] = VReject.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* --- 5. `case` ------------------------------------------------------ *)

(* The polymorphic `drop`, used by the default arm below. *)
Definition wDropAny : word_eff := prim 1 [TVar 2] [].

(* The selector lives on the row for the whole form: `of` consumes the test
   value AND the selector, `endof` puts the selector back for the next test,
   and `endcase` drops it on the default path.

   : K1 ( n -- n ) case MK-N of MK-N endof MK-N of MK-N endof MK-N endcase ;
       -> exit 0
   : K4 ( n -- )   case MK-N of endof endcase ;                    -> exit 0
   : K3 ( n -- n ) case MK-N of MK-N MK-N endof MK-N endcase ;     -> exit 70,
       `at 'endcase' expected: n n actual: n`
   : K5 ( n -- n ) case MK-N of MK-N endof endcase ;               -> exit 70,
       `at 'endcase' expected: n actual:` *)
Example case_arms_join_over_the_selector :
  check_ctl (sig [nt] [nt])
            [TCase; TCall wMkN; TOf; TCall wMkN; TEndof;
                    TCall wMkN; TOf; TCall wMkN; TEndof;
                    TCall wMkN; TEndcase] = VCert
  /\ check_ctl (sig [nt] []) [TCase; TCall wMkN; TOf; TEndof; TEndcase] = VCert
  /\ check_ctl (sig [nt] [nt])
       [TCase; TCall wMkN; TOf; TCall wMkN; TCall wMkN; TEndof;
               TCall wMkN; TEndcase] = VReject
  /\ check_ctl (sig [nt] [nt])
       [TCase; TCall wMkN; TOf; TCall wMkN; TEndof; TEndcase] = VReject.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* The test expression must be neutral apart from the one `n` it produces:
   `CF-OF` unifies the row after popping the test value against the row at
   `case`.

   : K8 ( n -- n ) case DUP1 MK-N of MK-N endof MK-N endcase ;  -> exit 70,
       `at 'of' expected: n actual: i64 i64` *)
Example an_of_test_must_be_neutral :
  check_ctl (sig [nt] [nt])
            [TCase; TCall wDup1; TCall wMkN; TOf; TCall wMkN; TEndof;
             TCall wMkN; TEndcase] = VReject.
Proof. vm_compute; reflexivity. Qed.

(* The `case` join is EXACT, like every other join, and it is pinned on
   `endof` — the arm that disagrees, not the form.

   : K10 ( n -- cell ) case MK-N of MK-U8 endof MK-N of MK-CELL endof
                            drop MK-CELL endcase ;  -> exit 70,
       `at 'endof' expected: u8 actual: cell` *)
Example case_arms_join_exactly :
  check_ctl (sig [nt] [cellt])
            [TCase; TCall wMkN; TOf; TCall wMkU8; TEndof;
                    TCall wMkN; TOf; TCall wMkCell; TEndof;
                    TCall wDropAny; TCall wMkCell; TEndcase] = VReject.
Proof. vm_compute; reflexivity. Qed.

(* A dead arm contributes nothing to the accumulator, exactly as a dead `if`
   arm is excluded from its join — and when EVERY path left early the form
   itself goes dead and the early-return fold supplies the output.

   : K9  ( n -- n )    case MK-N of MK-N EXIT endof MK-N endcase ;  -> exit 0
   : K14 ( n -- )      case MK-N of EXIT endof endcase ;            -> exit 0
   : K13 ( n -- cell ) case MK-N of MK-CELL EXIT endof
                            MK-N of MK-CELL EXIT endof
                            drop MK-CELL EXIT endcase ;             -> exit 0 *)
Example a_dead_case_arm_is_excluded :
  check_ctl (sig [nt] [nt])
            [TCase; TCall wMkN; TOf; TCall wMkN; TExit; TEndof;
             TCall wMkN; TEndcase] = VCert
  /\ check_ctl (sig [nt] []) [TCase; TCall wMkN; TOf; TExit; TEndof; TEndcase]
     = VCert
  /\ check_ctl (sig [nt] [cellt])
       [TCase; TCall wMkN; TOf; TCall wMkCell; TExit; TEndof;
               TCall wMkN; TOf; TCall wMkCell; TExit; TEndof;
               TCall wDropAny; TCall wMkCell; TExit; TEndcase] = VCert.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* `endof` and `endcase` ARE dead closers, so an arm may end in `EXIT`; and the
   `case` frames are checked by KIND like every other frame.

   : K15 ( n -- n ) of ;                                -> exit 70, `at 'of'`
   : K18 ( n -- n ) case MK-N of MK-N endof ;           -> exit 70, `at 'endof'`
   (`: K16 ... endof ;` and `: K17 ... endcase ;` never reach the checker: the
   ENGINE reports `hb: control-flow closer without opener`.) *)
Example case_frames_are_matched_by_kind :
  check_ctl (sig [nt] [nt]) [TOf] = VReject
  /\ check_ctl (sig [nt] [nt])
       [TCase; TCall wMkN; TOf; TCall wMkN; TEndof] = VReject
  /\ check_ctl (sig [nt] [nt]) [TEndof] = VReject
  /\ check_ctl (sig [nt] [nt]) [TEndcase] = VReject
  /\ check_ctl (sig [nt] [nt]) [TCase; TThen] = VReject
  /\ dead_closer TEndof = true /\ dead_closer TEndcase = true.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* --- 6. The `MATCH` eliminator -------------------------------------- *)

(* The three families measured, declared in the fixture as

     SUMTYPE mres 0    VARIANT ok  n ;VARIANT   VARIANT err n ;VARIANT   ;SUMTYPE
     SUMTYPE mthree 0  VARIANT one n ;VARIANT   VARIANT two n ;VARIANT
                       VARIANT three n ;VARIANT                          ;SUMTYPE
     SUMTYPE mbool 0   VARIANT yes bool ;VARIANT VARIANT no n ;VARIANT   ;SUMTYPE

   Variant ids are declaration order, which is what `MSEEN` indexes. *)
Definition fmres : fam := MkFam 100 [[nt]; [nt]].
Definition fmthree : fam := MkFam 101 [[nt]; [nt]; [nt]].
Definition fmbool : fam := MkFam 102 [[boolt]; [nt]].

Definition wDropBool : word_eff := prim 1 [boolt] [].

(* Every example in this section was run through `CHECK-CANDIDATE!`, because
   the ENGINE refuses several of these texts before the checker sees them.

   T1 ( mres -- n ) MATCH mres ok OF ENDOF err OF ENDOF ;MATCH   -> -1
   T2 ( mres -- n ) MATCH mres ok OF ENDOF ;MATCH                -> 0,
       `at ';MATCH' bad match: missing variants: err`
   T6 ( mthree -- n ) MATCH mthree one OF ENDOF two OF ENDOF ;MATCH -> 0,
       `at ';MATCH' bad match: missing variants: three` *)
Example match_must_be_exhaustive :
  check_ctl (sig [TFam 100] [nt])
            [TMatch; TFamTok fmres;
             TVarTok 0; TOf; TEndof; TVarTok 1; TOf; TEndof; TSemiMatch] = VCert
  /\ check_ctl (sig [TFam 100] [nt])
       [TMatch; TFamTok fmres; TVarTok 0; TOf; TEndof; TSemiMatch] = VReject
  /\ check_reason (sig [TFam 100] [nt])
       [TMatch; TFamTok fmres; TVarTok 0; TOf; TEndof; TSemiMatch] = MD_NONEXH
  /\ check_ctl (sig [TFam 101] [nt])
       [TMatch; TFamTok fmthree;
        TVarTok 0; TOf; TEndof; TVarTok 1; TOf; TEndof; TSemiMatch] = VReject.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* PAYLOAD REFINEMENT.  Each branch starts at the row the bundle was popped
   down to plus THAT variant's payload, so the arms of one `match` genuinely
   start from different rows — which no other construct allows.  `mbool`'s two
   variants carry `bool` and `n`, and with empty branches those rows are what
   reaches the join.

   T7 ( mbool -- n ) MATCH mbool yes OF ENDOF no OF ENDOF ;MATCH  -> 0,
       `at 'ENDOF' bad match: branch output mismatch expected: bool actual: n`
   T8 ( mbool -- n ) MATCH mbool yes OF DROP-BOOL MK-N ENDOF no OF ENDOF ;MATCH
       -> -1 *)
Example match_branches_are_refined_by_their_payload :
  check_ctl (sig [TFam 102] [nt])
            [TMatch; TFamTok fmbool;
             TVarTok 0; TOf; TEndof; TVarTok 1; TOf; TEndof; TSemiMatch] = VReject
  /\ check_ctl (sig [TFam 102] [nt])
       [TMatch; TFamTok fmbool;
        TVarTok 0; TOf; TCall wDropBool; TCall wMkN; TEndof;
        TVarTok 1; TOf; TEndof; TSemiMatch] = VCert.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* `MD-JOIN` LIVES HERE AND NOWHERE ELSE.  `MATCH-ACCUM` (checker.f:8278) is
   the only site that latches it, and only when THIS unification is the one
   that failed.  An `if`/`else`/`then` disagreement — the join it is usually
   attributed to — raises no reason code at all.  Both halves measured above. *)
Example md_join_is_the_match_join_not_the_if_join :
  check_reason (sig [TFam 102] [nt])
               [TMatch; TFamTok fmbool;
                TVarTok 0; TOf; TEndof; TVarTok 1; TOf; TEndof; TSemiMatch]
    = MD_JOIN
  /\ check_reason (sig [i64] [i64])
       [TCall wMkBool; TIf; TCall wStep1; TElse; TCall wDrop1; TCall wMkCell;
        TThen] = 0
  /\ check_reason (sig [i64] [i64])
       [TBegin; TCall wDup1; TCall wMkBool; TUntil] = 0.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* An EARLIER failure inside the branch keeps its own diagnostic: `MD-JOIN` is
   latched only when the failure pin was still open when the join ran.  That
   guard is the whole reason `FAILSET` is modelled. *)
Example md_join_does_not_steal_an_earlier_failure :
  check_reason (sig [TFam 102] [nt])
               [TMatch; TFamTok fmbool;
                TVarTok 0; TOf; TCall wStep1; TEndof;
                TVarTok 1; TOf; TEndof; TSemiMatch] = 0.
Proof. vm_compute; reflexivity. Qed.

(* A variant token must be followed by `OF`, a variant may not repeat, and an
   undeclared variant is refused.  All three are HARD rejects (`MREJ`), which
   is what makes them outrank an `UNCK` raised later in the same body.

   T9 ( mres -- n ) MATCH mres ok DROP-N OF ENDOF err OF ENDOF ;MATCH -> 0,
       `at 'DROP-N' bad match: variant token must be followed by OF`
   T4 ( mres -- n ) MATCH mres ok OF ENDOF ok OF ENDOF ;MATCH -> 0,
       `at 'ok' bad match: duplicate variant`
   U3 ( mres -- n ) MATCH mres zork OF ENDOF err OF ENDOF ;MATCH -> 0,
       `at 'zork' bad match: unknown variant`
   U5 ( mres -- n ) ;MATCH -> 0, `at ';MATCH' bad match: misplaced match token` *)
Example match_form_shape_is_enforced :
  check_reason (sig [TFam 100] [nt])
               [TMatch; TFamTok fmres; TVarTok 0; TCall wDropN; TOf; TEndof;
                TVarTok 1; TOf; TEndof; TSemiMatch] = MD_MISSING_OF
  /\ check_ctl (sig [TFam 100] [nt])
       [TMatch; TFamTok fmres; TVarTok 0; TOf; TEndof;
        TVarTok 0; TOf; TEndof; TSemiMatch] = VReject
  /\ check_reason (sig [TFam 100] [nt])
       [TMatch; TFamTok fmres; TVarTok 0; TOf; TEndof;
        TVarTok 0; TOf; TEndof; TSemiMatch] = MD_VAR_DUP
  /\ check_reason (sig [TFam 100] [nt])
       [TMatch; TFamTok fmres; TVarTok 5; TOf; TEndof;
        TVarTok 1; TOf; TEndof; TSemiMatch] = MD_VAR_UNKNOWN
  /\ check_reason (sig [TFam 100] [nt]) [TSemiMatch] = MD_STRAY.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* A `match` left unterminated at `;` is `MD-TRUNC` (checker.f:9679), and the
   open kind-9 frame rejects on its own account too.

   U4 ( mres -- n ) MATCH mres ok OF ENDOF err OF ENDOF -> 0,
       `at 'ENDOF' bad match: unterminated match expected: n actual:` *)
Example an_unterminated_match_is_truncation :
  check_ctl (sig [TFam 100] [nt])
            [TMatch; TFamTok fmres; TVarTok 0; TOf; TEndof;
             TVarTok 1; TOf; TEndof] = VReject
  /\ check_reason (sig [TFam 100] [nt])
       [TMatch; TFamTok fmres; TVarTok 0; TOf; TEndof;
        TVarTok 1; TOf; TEndof] = MD_TRUNC.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* A dead branch contributes nothing to the join, and when EVERY branch left
   early the form itself goes dead — the same rule `if`, `case` and `loop` all
   use, reached through a fourth accumulator.

   TA ( mres -- n ) MATCH mres ok OF EXIT ENDOF err OF ENDOF ;MATCH     -> -1
   TB ( mres -- n ) MATCH mres ok OF EXIT ENDOF err OF EXIT ENDOF ;MATCH -> -1
   TC ( mres -- n ) MATCH mres ok OF MK-N throw ENDOF err OF ENDOF ;MATCH -> -1 *)
Example a_dead_match_branch_is_excluded :
  check_ctl (sig [TFam 100] [nt])
            [TMatch; TFamTok fmres; TVarTok 0; TOf; TExit; TEndof;
             TVarTok 1; TOf; TEndof; TSemiMatch] = VCert
  /\ check_ctl (sig [TFam 100] [nt])
       [TMatch; TFamTok fmres; TVarTok 0; TOf; TExit; TEndof;
        TVarTok 1; TOf; TExit; TEndof; TSemiMatch] = VCert
  /\ check_ctl (sig [TFam 100] [nt])
       [TMatch; TFamTok fmres; TVarTok 0; TOf; TCall wMkN; TThrow; TEndof;
        TVarTok 1; TOf; TEndof; TSemiMatch] = VCert.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* The scrutinee must be a value of the matched family.  The checker's rule is
   a width-expanded hidden-field bundle and the model's is one `TFam` cell, so
   only the VERDICT is claimed here, not the rendered prose: in both measured
   cases the pin lands on the family token and a later undefined-word latch
   supplies the message text.

   U1 ( n -- n ) MATCH mres ok OF ENDOF err OF ENDOF ;MATCH  -> 0
   U6 ( mres -- n ) MATCH mbool ok OF ... ;MATCH             -> 0
   U2 ( mres -- n ) [: MATCH mres ... ;MATCH ;] drop MK-N    -> 0
       (a quotation's rows carry no scrutinee at all: `MD-QUOT`) *)
Example the_scrutinee_must_be_the_matched_family :
  check_ctl (sig [nt] [nt])
            [TMatch; TFamTok fmres; TVarTok 0; TOf; TEndof;
             TVarTok 1; TOf; TEndof; TSemiMatch] = VReject
  /\ check_ctl (sig [TFam 100] [nt])
       [TMatch; TFamTok fmbool; TVarTok 0; TOf; TEndof;
        TVarTok 1; TOf; TEndof; TSemiMatch] = VReject
  /\ check_reason (sig [nt] [nt])
       [TMatch; TFamTok fmres; TVarTok 0; TOf; TEndof;
        TVarTok 1; TOf; TEndof; TSemiMatch] = MD_SCRUT
  /\ check_reason (sig [TFam 100] [nt])
       [TOpenQ; TMatch; TFamTok fmres] = MD_QUOT.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* --- 7. EXIT -------------------------------------------------------- *)

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

(* --- 8. Dead code, and where a dead path may close ------------------ *)

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

(* --- 9. `throw`, `die`, and the exceptional edge -------------------- *)

(* `throw` and `die` are SOURCES OF DEADNESS, and deadness RELAXES joins — so
   leaving them out of a control model makes it reject bodies the checker
   accepts, not the other way round.

   Unlike `exit`, a dead path from `throw` feeds NOTHING to the output
   accumulator: `XSET` stays clear, so the fall-through row still has to
   satisfy the declaration on its own.

   : T2 ( i64 -- i64 ) MK-N throw ;                                   -> exit 0
   : T1 ( i64 -- i64 ) DROP1 MK-N throw ;                             -> exit 70,
       `at 'throw' expected: i64 actual:`
   : T4 ( i64 -- i64 ) MK-N throw STEP1 ;                             -> exit 70,
       `at 'STEP1' after 'throw'`
   : T3 ( i64 -- i64 ) MK-BOOL IF DROP1 MK-N throw THEN STEP1 ;       -> exit 0
   : T5 ( i64 -- cell ) MK-BOOL IF MK-N throw THEN DROP1 MK-CELL ;    -> exit 0 *)
Example throw_consumes_a_code_and_kills_the_path :
  check_ctl (sig [i64] [i64]) [TCall wMkN; TThrow] = VCert
  /\ check_ctl (sig [i64] [i64]) [TCall wDrop1; TCall wMkN; TThrow] = VReject
  /\ check_ctl (sig [i64] [i64]) [TCall wMkN; TThrow; TCall wStep1] = VReject
  /\ check_ctl (sig [i64] [i64])
       [TCall wMkBool; TIf; TCall wDrop1; TCall wMkN; TThrow; TThen; TCall wStep1]
     = VCert
  /\ check_ctl (sig [i64] [cellt])
       [TCall wMkBool; TIf; TCall wMkN; TThrow; TThen; TCall wDrop1; TCall wMkCell]
     = VCert.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* The deadness a `throw` inside a branch produces is what lets the OTHER arm
   disagree with it — exactly the `EXIT` rule, reached by a different route.
   Without `throw` in the model this body has no reason to be accepted.

   : N1 ( bool -- cell ) IF MK-N throw THEN MK-CELL ;  -> exit 0 *)
Example throw_relaxes_a_branch_join :
  check_ctl (sig [boolt] [cellt])
            [TIf; TCall wMkN; TThrow; TThen; TCall wMkCell] = VCert.
Proof. vm_compute; reflexivity. Qed.

(* `throw` raises the edge AND kills the path; `die` only kills.  A definition
   that raised the edge is recorded as a throwing word (`CTL-THROW`,
   checker.f:9698) and one that ended dead with no `exit` as a no-return word
   (`CTL-DEAD`, checker.f:9697); every later call site reads them back. *)
Example throw_and_die_differ_only_in_the_edge :
  ctl_throw (sig [i64] [i64]) [TCall wMkN; TThrow] = true
  /\ ctl_dead (sig [i64] [i64]) [TCall wMkN; TThrow] = true
  /\ ctl_throw (sig [] []) [TCall (prim 1 [] [TPtr u8; nt; nt]); TDie] = false
  /\ ctl_dead (sig [] []) [TCall (prim 1 [] [TPtr u8; nt; nt]); TDie] = true
  (* an ordinary body records neither, and an `exit` is a RETURN, not deadness *)
  /\ ctl_dead (sig [i64] [i64]) [TCall wStep1] = false
  /\ ctl_dead (sig [i64] [i64]) [TExit] = false.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* And a call to such a word behaves like the token itself: the flags travel.
   This is `TCallCtl`, and it is why `DEAD-CUR?` consults the registry rather
   than a fixed list of spellings.  Measured over

     : NORET ( n -- ) throw ;

   which certifies and is recorded `CTL-DEAD | CTL-THROW`:

   : N2 ( i64 -- i64 ) MK-N NORET STEP1 ;             -> exit 70,
       `at 'STEP1' after 'NORET'`
   : N5 ( i64 -- i64 ) MK-N DROP-N STEP1 ;            -> exit 0  (no flags)
   : N4 ( bool -- cell ) IF MK-N NORET THEN MK-CELL ; -> exit 0 *)
Definition wNoReturn : word_eff := prim 1 [nt] [].

Example inherited_control_flags_kill_the_callers_path :
  check_ctl (sig [i64] [i64])
            [TCall wMkN; TCallCtl wNoReturn true true; TCall wStep1] = VReject
  /\ check_ctl (sig [i64] [i64])
       [TCall wMkN; TCall wNoReturn; TCall wStep1] = VCert
  /\ check_ctl (sig [boolt] [cellt])
       [TIf; TCall wMkN; TCallCtl wNoReturn true true; TThen; TCall wMkCell]
     = VCert.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* --- 10. Quotation application, and the flags it reads -------------- *)

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

(* UNSOUNDNESS CLOSED.  A quotation whose every path leaves early carries
   `Q>XDEAD`, and `RSEXEC` then KILLS the caller's path instead of installing
   the quotation's output rows (checker.f:2018-2021).  The previous revision of
   this file always installed them, and the divergence is observable in BOTH
   directions.

   ACCEPTING DIRECTION — the model certified, the checker rejects:
   : X2 ( i64 -- i64 i64 ) [: DUP1 MK-N throw ;] execute ;  -> exit 70,
       `at 'execute' expected: i64 i64 actual: i64`
   The quotation's declared output IS `i64 i64`.  Installing it satisfies the
   declaration; not installing it leaves a bare `i64`, which does not.

   : X1 ( i64 -- i64 ) [: MK-N throw ;] execute STEP1 ;     -> exit 70,
       `at 'STEP1' after 'throw'`
   The caller's path is dead, so the next token is dead code.  A model that
   stays live accepts this body.

   REJECTING DIRECTION — a body the checker accepts:
   : T9 ( i64 -- i64 ) [: DUP1 MK-N throw ;] execute ;      -> exit 0 *)
Definition qthrow_dup : list tok :=
  [TOpenQ; TCall wDup1; TCall wMkN; TThrow; TCloseQ; TExec].

Example xdead_quotation_does_not_install_its_output :
  check_ctl (sig [i64] [i64; i64]) qthrow_dup = VReject
  /\ check_ctl (sig [i64] [i64]) qthrow_dup = VCert
  /\ check_ctl (sig [i64] [i64])
       [TOpenQ; TCall wMkN; TThrow; TCloseQ; TExec; TCall wStep1] = VReject.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* The flags are what carry that decision, and `;]` computes them exactly as
   `QX!` does: `Q>XHAS` is `THSET`, `Q>XDEAD` is `DEADP AND NOT XSET`.  So a
   quotation that ends in `exit` is NOT xdead — the fold gave it a normal
   return — while one that ends in `throw` or `again` is. *)
Definition quot_flags (ts : list tok) : bool * bool :=
  let s := run (sig [] []) (init (sig [] [])) (TOpenQ :: ts ++ [TCloseQ]) in
  match resolve_row (st_sub s) (st_dcur s) with
  | SPush t _ => (q_xhas (resolve_ty (st_sub s) t),
                  q_xdead (resolve_ty (st_sub s) t))
  | SRow _ => (false, false)
  end.

Example semiq_computes_the_control_flags :
  quot_flags [TCall wStep1] = (false, false)
  /\ quot_flags [TCall wMkN; TThrow] = (true, true)
  /\ quot_flags [TCall wMkI64; TExit] = (false, false)
  /\ quot_flags [TBegin; TCall wMkN; TCall wDropN; TAgain] = (false, true)
  (* a CONDITIONAL throw raises the edge but keeps a normal return *)
  /\ quot_flags [TCall wMkBool; TIf; TCall wMkN; TThrow; TThen] = (true, false).
Proof. repeat split; vm_compute; reflexivity. Qed.

(* `RSEXEC` propagates the edge: executing a quotation that can throw makes the
   CALLER a throwing word.  `catch` does not — recovering the throw is what it
   is for.

   : X10 ( i64 -- i64 ) [: MK-BOOL IF MK-N throw THEN ;] execute ;       -> exit 0
   : X11 ( i64 -- i64 ) [: MK-BOOL IF MK-N throw THEN ;] execute STEP1 ; -> exit 0
   : X12 ( i64 -- i64 ) [: MK-BOOL IF MK-N throw THEN ;] catch DROP-N ;  -> exit 0 *)
Definition qmay_throw : list tok :=
  [TOpenQ; TCall wMkBool; TIf; TCall wMkN; TThrow; TThen; TCloseQ].

Example execute_propagates_the_throw_edge_and_catch_does_not :
  check_ctl (sig [i64] [i64]) (qmay_throw ++ [TExec]) = VCert
  /\ check_ctl (sig [i64] [i64]) (qmay_throw ++ [TExec; TCall wStep1]) = VCert
  /\ ctl_throw (sig [i64] [i64]) (qmay_throw ++ [TExec]) = true
  /\ check_ctl (sig [i64] [i64]) (qmay_throw ++ [TCatch; TCall wDropN]) = VCert
  /\ ctl_throw (sig [i64] [i64]) (qmay_throw ++ [TCatch; TCall wDropN]) = false.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* `catch` splits three ways on the flags, and the third case pushes NO code.

   : X6  ( i64 -- i64 )   [: DUP1 MK-N throw ;] catch DROP-N ;          -> exit 0
   : X4  ( i64 -- i64 )   [: BEGIN MK-N DROP-N AGAIN ;] catch ;         -> exit 0
   : X4b ( i64 -- i64 n ) [: BEGIN MK-N DROP-N AGAIN ;] catch ;         -> exit 70,
       `at 'catch' expected: i64 n actual: i64`
   : X9  ( i64 -- i64 )   [: BEGIN MK-N DROP-N AGAIN ;] catch STEP1 ;   -> exit 70,
       `at 'STEP1' after 'AGAIN'` *)
Definition qforever : list tok :=
  [TOpenQ; TBegin; TCall wMkN; TCall wDropN; TAgain; TCloseQ].

Definition qthrow_dup_catch : list tok :=
  [TOpenQ; TCall wDup1; TCall wMkN; TThrow; TCloseQ; TCatch; TCall wDropN].

Example catch_of_a_never_returning_quotation_pushes_no_code :
  (* always throws, and `catch` always recovers: the code IS pushed *)
  check_ctl (sig [i64] [i64]) qthrow_dup_catch = VCert
  (* never returns at all: no code, and the caller goes dead *)
  /\ check_ctl (sig [i64] [i64]) (qforever ++ [TCatch]) = VCert
  /\ check_ctl (sig [i64] [i64; nt]) (qforever ++ [TCatch]) = VReject
  /\ check_ctl (sig [i64] [i64]) (qforever ++ [TCatch; TCall wStep1]) = VReject.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* --- 11. RECURSE ---------------------------------------------------- *)

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

(* --- 12. Unbalanced control ----------------------------------------- *)

(* A definition that ends with a frame still open is rejected at the boundary.

   : C29 ( i64 -- i64 ) MK-BOOL IF ;  -> exit 70, `at 'IF'` (bare pin) *)
Example an_unclosed_frame_rejects :
  check_ctl (sig [i64] [i64]) [TCall wMkBool; TIf] = VReject.
Proof. vm_compute; reflexivity. Qed.

(* A closer with no opener runs `CF-FAIL`.  Note that in a top-level definition
   the ENGINE rejects several of these before the checker ever sees them —
   `: C28 ( -- ) THEN ;` reports `hb: control-flow closer without opener: THEN`,
   not a checker diagnostic.  So these arms are defensive; the model keeps
   them, and rejecting here is at worst stricter than the checker.  Through
   `CHECK-CANDIDATE!`, which the engine's pairing check does not gate:
   `BL1 ( -- ) loop` and `BL2 ( -- ) endof` both answer 0. *)
Example a_closer_without_an_opener_rejects :
  check_ctl (sig [] []) [TThen] = VReject
  /\ check_ctl (sig [] []) [TRepeat] = VReject
  /\ check_ctl (sig [] []) [TElse] = VReject
  /\ check_ctl (sig [] []) [TCloseQ] = VReject
  /\ check_ctl (sig [] []) [TLoop] = VReject.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* Mismatched openers and closers are rejected by frame KIND, not by nesting
   depth: `begin ... then` has a frame to pop but the wrong one. *)
Example a_closer_must_match_its_opener :
  check_ctl (sig [] []) [TBegin; TThen] = VReject
  /\ check_ctl (sig [] []) [TCall wMkBool; TIf; TUntil] = VReject
  /\ check_ctl (sig [] []) [TBegin; TRepeat] = VReject.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* --- 13. The frame stack is BOUNDED --------------------------------- *)

(* UNSOUNDNESS CLOSED.  `CFS` is a 32-record array and `CF-PUSH` turns the
   33rd open frame into `UNCK` (checker.f:7601, 7652).  An unbounded frame list
   certifies bodies the checker refuses to certify — and, because UNCHECKABLE
   outranks a plain reject, it also turns some rejects into uncheckables.

   Both halves measured with `CHECK-CANDIDATE!`, which is the only harness that
   can tell -1 from 1 from 0:

     32 nested `BEGIN`, unclosed  -> 0   (rejected: unbalanced)
     33 nested `BEGIN`, unclosed  -> 1   (UNCHECKABLE: the 33rd push overflowed)
     32 nested `BEGIN`/`UNTIL`    -> -1  (certified)
     33 nested `BEGIN`/`UNTIL`    -> 1
     32 nested `IF`/`THEN`        -> -1
     33 nested `IF`/`THEN`        -> 1

   The unclosed pair is the sharpest statement of the rule: nothing about those
   two bodies differs except the count, and the verdict changes CLASS. *)
Definition opens (n : nat) : list tok := repeat TBegin n.

Example frame_depth_is_bounded :
  check_ctl (sig [i64] [i64]) (opens 32) = VReject
  /\ check_ctl (sig [i64] [i64]) (opens 33) = VUncheckable
  /\ length (st_cfs (run (sig [i64] [i64]) (init (sig [i64] [i64])) (opens 33)))
     = cfs_cap.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* And a `match` never reaches that overflow: it refuses at `#CFC > 30`
   (checker.f:8255) with a HARD reject, precisely so it can never install one
   of its two frames and not the other.

   BL3 ( mres -- n ) BEGIN x31  MATCH mres ok OF ENDOF err OF ENDOF ;MATCH -> 0
   (the rendered prose is `undefined word 'mres'`: `MATCH-REJECT` clears `MM`,
   so the variant tokens that follow become unknown words and their latch wins
   the render.  The VERDICT is the hard reject `MD-DEPTH` asks for.) *)
Example match_refuses_before_the_overflow :
  check_ctl (sig [TFam 100] [nt]) (opens 31 ++ [TMatch; TFamTok fmres]) = VReject
  /\ check_reason (sig [TFam 100] [nt])
       (opens 31 ++ [TMatch; TFamTok fmres]) = MD_DEPTH.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* The stronger statement, and the expensive one: a body that CERTIFIES at 32
   frames becomes UNCHECKABLE at 33. *)
Definition nest_until (n : nat) : list tok :=
  repeat TBegin n ++ concat (repeat [TCall wMkBool; TUntil] n).

Example a_certifying_body_becomes_uncheckable_at_33_frames :
  check_ctl (sig [i64] [i64]) (nest_until 32) = VCert
  /\ check_ctl (sig [i64] [i64]) (nest_until 33) = VUncheckable.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* --- 14. Branch-scoped locals --------------------------------------- *)

(* `{: x :}` pops the value into a fresh variable and a reference re-pushes it.
   The CONTROL rule is that the binding dies at the enclosing construct's
   closer, because every frame saved `#LOC` and every closer restores it.

   All measured with `CHECK-CANDIDATE!`:
   P1 ( i64 -- i64 ) {: x:i64 :} x                             -> -1
   P2 ( i64 -- i64 ) MK-BOOL IF {: x:i64 :} x THEN             -> -1
   P3 ( i64 -- i64 ) MK-BOOL IF {: x:i64 :} x ELSE STEP1 THEN  -> -1
   P5 ( i64 -- i64 ) {: x:i64 :} MK-BOOL IF x ELSE x THEN      -> -1
   P4 ( i64 -- i64 ) MK-BOOL IF {: x:i64 :} x THEN x           ->  1
   P6 ( i64 -- i64 ) MK-BOOL IF {: x:i64 :} x ELSE x THEN      ->  1

   The last two answer 1, not 0: after the closer the name is not a local, so
   it is an unknown WORD, and an unknown word is uncheckable.

   The model's locals are UNTYPED: `LOC-ANN` (checker.f:7386) records the
   `x:i64` annotation and `LOC-ANN-BIND-CHECK` (checker.f:7424) asserts it,
   which is an extra constraint on top of the binding unification and never a
   weaker one.  In every fixture above the annotation is the type the value
   already had, so both machines agree. *)
Example locals_are_scoped_to_their_frame :
  check_ctl (sig [i64] [i64]) [TLocals 1; TLocRef 0] = VCert
  /\ check_ctl (sig [i64] [i64])
       [TCall wMkBool; TIf; TLocals 1; TLocRef 0; TThen] = VCert
  /\ check_ctl (sig [i64] [i64])
       [TCall wMkBool; TIf; TLocals 1; TLocRef 0; TElse; TCall wStep1; TThen]
     = VCert
  /\ check_ctl (sig [i64] [i64])
       [TLocals 1; TCall wMkBool; TIf; TLocRef 0; TElse; TLocRef 0; TThen] = VCert
  /\ check_ctl (sig [i64] [i64])
       [TCall wMkBool; TIf; TLocals 1; TLocRef 0; TThen; TLocRef 0] = VUncheckable
  /\ check_ctl (sig [i64] [i64])
       [TCall wMkBool; TIf; TLocals 1; TLocRef 0; TElse; TLocRef 0; TThen]
     = VUncheckable.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* Every closer restores the mark, so the rule is not special to `if`: a loop
   body, a `case` arm and a `match` arm all drop their locals too.  `while` is
   the exception, because its frame stays open.

   PW4 ( i64 -- i64 ) BEGIN {: x:i64 :} x MK-BOOL UNTIL             -> -1
   PW1 ( i64 -- i64 ) BEGIN {: x:i64 :} x MK-BOOL UNTIL x           ->  1
   PW5 ( i64 -- i64 ) MK-N MK-N do {: x:i64 :} x loop               -> -1
   PW2 ( i64 -- i64 ) MK-N MK-N do {: x:i64 :} x loop x             ->  1
   PW3 ( i64 -- i64 ) BEGIN {: x:i64 :} x MK-BOOL WHILE x DROP1 REPEAT -> -1 *)
Example every_closer_restores_the_locals_mark :
  check_ctl (sig [i64] [i64])
            [TBegin; TLocals 1; TLocRef 0; TCall wMkBool; TUntil; TLocRef 0]
    = VUncheckable
  /\ check_ctl (sig [i64] [i64])
       [TCall wMkN; TCall wMkN; TDo; TLocals 1; TLocRef 0; TLoop; TLocRef 0]
     = VUncheckable
  (* `while` keeps its frame open, so a local bound before it survives to `repeat` *)
  /\ check_ctl (sig [i64] [i64])
       [TBegin; TLocals 1; TLocRef 0; TCall wMkBool; TWhile; TLocRef 0;
        TCall wDrop1; TRepeat] = VCert.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* Locals and quotations do not mix, in either direction, and both are HARD
   rejects rather than uncheckables.

   P7 ( i64 -- i64 ) [: {: x:i64 :} x ;] execute   -> 0
   P8 ( i64 -- i64 ) {: x:i64 :} [: x ;] drop x    -> 0 *)
Example locals_are_refused_inside_a_quotation :
  check_ctl (sig [i64] [i64]) [TOpenQ; TLocals 1; TLocRef 0; TCloseQ; TExec]
    = VReject
  /\ check_ctl (sig [i64] [i64])
       [TLocals 1; TOpenQ; TLocRef 0; TCloseQ; TCall wDropAny; TLocRef 0]
     = VReject.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* --- 15. Linear-once conservation ----------------------------------- *)

(* A `deflinear ltok` name is an ordinary concrete con of the linear class.
   The fixtures below were all run through `CHECK-CANDIDATE!` over

     deflinear ltok
     : DUP-POLY ( a -- a a ) dup ;
     : DROP-POLY ( a -- ) drop ;
     : KEEP-POLY ( a -- a ) ;

   all four of which certify on their own. *)
Definition ltok : ty := TCon (CLinear 0).
Definition wDupAny : word_eff := prim 1 [TVar 2] [TVar 2; TVar 2].
Definition wKeepAny : word_eff := prim 1 [TVar 2] [TVar 2].
Definition wSwapAny : word_eff := prim 1 [TVar 2; TVar 3] [TVar 3; TVar 2].

(* (a) CONCRETE COUNT CONSERVATION.  A step whose declared rows do not
   themselves name a linear may not change how many linear cons are live.

   Q1 ( ltok -- ltok )                      -> -1
   Q2 ( ltok -- ltok ltok ) dup             ->  0, `at 'dup'`
   Q3 ( ltok -- ) drop                      ->  0, `at 'drop'`
   Q4 ( ltok n -- n ltok ) swap             -> -1   (a permutation conserves)
   Q5 ( ltok -- ltok ) MK-BOOL IF THEN      -> -1
   Q8 ( ltok -- ltok ) [: ;] execute        -> -1 *)
Example linear_count_is_conserved_per_step :
  check_ctl (sig [ltok] [ltok]) [] = VCert
  /\ check_ctl (sig [ltok] [ltok; ltok]) [TCall wDupAny] = VReject
  /\ check_ctl (sig [ltok] []) [TCall wDropAny] = VReject
  /\ check_ctl (sig [ltok; nt] [nt; ltok]) [TCall wSwapAny] = VCert
  /\ check_ctl (sig [ltok] [ltok]) [TCall wMkBool; TIf; TThen] = VCert
  /\ check_ctl (sig [ltok] [ltok]) [TOpenQ; TCloseQ; TExec] = VCert.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* The count is over BOTH rows together, which is why a `>r`/`r>` round trip
   conserves even though each half changes the data row on its own.

   Q6 ( ltok -- ltok ) >r r>   -> -1

   And this is exactly why `>r` is its own rule rather than a word: an ORDINARY
   word declared with the same effect runs `LIN-CHECK` while the value is on
   neither row, and rejects.  `Effects.to_r` is that ordinary word; `TToR` is
   the real one. *)
Definition wToRAsWord : word_eff :=
  MkWordEff (Eff (stack_of 1 [TVar 3]) (SRow 1) (SRow 2) (stack_of 2 [TVar 3])) true.

Example linear_count_spans_both_rows :
  check_ctl (sig [ltok] [ltok]) [TToR; TFromR] = VCert
  /\ check_ctl (sig [ltok] [ltok]) [TCall wToRAsWord; TFromR] = VReject
  (* with nothing linear in play the two are indistinguishable *)
  /\ check_ctl (sig [i64] [i64]) [TToR; TFromR] = VCert
  /\ check_ctl (sig [i64] [i64]) [TCall wToRAsWord; TFromR] = VCert.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* The round trip brings back the SAME type, not a fresh variable — the rule
   `Effects.return_row_round_trips` states over word effects, restated over the
   real transfer tokens. *)
Example return_row_round_trips_through_the_real_transfers :
  check_ctl (sig [i64] [i64]) [TToR; TFromR] = VCert
  /\ check_ctl (sig [i64] [cellt]) [TToR; TFromR] = VCert
  /\ check_ctl (sig [i64] [strt]) [TToR; TFromR] = VReject
  (* and a value left on the return row is caught by the base-row seal *)
  /\ check_ctl (sig [i64] []) [TToR] = VReject.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* (b) DEFERRED TAINT.  A polymorphic copy or drop is legal in itself, and
   becomes a rejection the moment the variable it laundered resolves linear.
   The taint OUTLIVES the construct that created it, which is the point: the
   quotation below is checked on its own fresh rows, where nothing is linear
   yet, and only `execute` binds them.

   R3 ( ltok -- ltok ) KEEP-POLY                       -> -1
   R4 ( n -- n n ) DUP-POLY                            -> -1
   R1 ( ltok -- ltok ltok ) DUP-POLY                   ->  0, `at 'DUP-POLY'`
   R2 ( ltok -- ) DROP-POLY                            ->  0, `at 'DROP-POLY'`
   R6 ( ltok -- ltok ) MK-BOOL IF DUP-POLY DROP-POLY THEN -> 0, `at 'DUP-POLY'`
   R5 ( ltok -- ltok ) [: DUP-POLY drop ;] execute     ->  0, `at 'execute'` *)
Example polymorphic_laundering_is_caught :
  check_ctl (sig [ltok] [ltok]) [TCall wKeepAny] = VCert
  /\ check_ctl (sig [nt] [nt; nt]) [TCall wDupAny] = VCert
  /\ check_ctl (sig [ltok] [ltok; ltok]) [TCall wDupAny] = VReject
  /\ check_ctl (sig [ltok] []) [TCall wDropAny] = VReject
  /\ check_ctl (sig [ltok] [ltok])
       [TCall wMkBool; TIf; TCall wDupAny; TCall wDropAny; TThen] = VReject
  /\ check_ctl (sig [ltok] [ltok])
       [TOpenQ; TCall wDupAny; TCall wDropAny; TCloseQ; TExec] = VReject.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* A linear may not be bound to a local at all: a reference re-pushes the
   binding without any step for the conservation check to see, so the copy or
   the leak would be invisible.  This is a HARD reject.

   Q7 ( ltok -- ltok ) {: x:ltok :} x  -> 0,
       `E-LINEAR-LOCAL ... linear value cannot be bound to a local` *)
Example a_linear_may_not_become_a_local :
  check_ctl (sig [ltok] [ltok]) [TLocals 1; TLocRef 0] = VReject
  /\ check_ctl (sig [i64] [i64]) [TLocals 1; TLocRef 0] = VCert.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* --- 16. Agreement with the straight-line model --------------------- *)

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
   post-hoc seal buys: no control construct has to know about it.  It holds for
   the `do` shape too.  Measured over `: TIMG ( img -- img ) ;`:

   : UF2 ( -- ) BEGIN TIMG MK-BOOL UNTIL ;    -> exit 70, `at 'UNTIL'`
   : UF1 ( -- ) MK-N MK-N do TIMG loop ;      -> exit 70, `at 'loop'`
   : UF3 ( img -- img ) MK-N MK-N do TIMG loop ;  -> exit 0 *)
Example the_loop_rule_is_silent_about_underflow :
  check_ctl (MkCfg declared_empty 8 true)
            [TBegin; TCall trusted_img; TCall wMkBool; TUntil] = VReject
  /\ check_ctl (MkCfg declared_empty 8 true)
       [TCall wMkN; TCall wMkN; TDo; TCall trusted_img; TLoop] = VReject.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* --- 17. Failure is still a value ------------------------------------ *)

(* Nothing above can raise, diverge, or be partial.  `step` is total, `run` is
   structural on the token list, and every verdict is one of three values. *)
Example failure_is_a_value :
  certifiedb (check_ctl (sig [i64] [i64]) [TExit; TCall wStep1]) = false
  /\ certifiedb (check_ctl (sig [i64] [i64]) [TCall wStep1]) = true.
Proof. repeat split; vm_compute; reflexivity. Qed.
