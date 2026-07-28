(* Habu.Common.Effects — a model of the checked stack-effect language.

   `docs/effects.md` is the written specification.  `src/core/checker.f` is the
   implementation, and it is the ground truth wherever the two disagree.  Every
   definition below names the checker word it models, with a line reference, so
   a reader can check the model against the code rather than against prose.

   Scope of this file: the syntax of types, stacks, and four-row effects;
   executable unification of two stacks; executable sequential composition of
   two effects; and the predicate that decides whether a checked definition's
   implicit row survived its body.  Laws, algebraic properties, and the
   soundness statement (acceptance implies the body cannot underflow or
   type-mismatch at runtime) are deliberately absent — they are later work.

   Deliberate omissions from the modelled fragment, each of which the checker
   also decides and each of which is a place a later soundness proof would not
   speak about:
     - T-ATOM rigid host identities (region / extent / generation),
     - type families of arity > 0 (layouts, sums, products, `uniform<T>`),
       and with them the hidden-field / logical-bundle machinery,
     - VALUE-RECORD field cells and their coercion,
     - the linear-once conservation pass,
     - control flow, throw edges, and quotation application rules.
   Arity-0 nominal families (what `DEFTYPE` mints) ARE modelled, because they
   are named by the signature grammar.

   Every total function here returns `option` or `bool`.  Where a walk is
   bounded by fuel, exhausting the fuel is fail-closed: an occurs check reports
   an occurrence and unification reports failure.  So the model can only ever
   reject more programs than the checker on that account, never accept more. *)

From Stdlib Require Import Bool List PeanoNat.
Import ListNotations.

(* ------------------------------------------------------------------ *)
(* Variables.                                                          *)
(*                                                                     *)
(* The checker draws type variables and row variables from ONE counter *)
(* (`FRESH`, checker.f:1704), so a single `nat` id space is faithful;  *)
(* the two stores `TVT` and `RVT` are indexed by that shared id.       *)
(* Surface letters are a per-signature naming device only: `NMAP`      *)
(* maps `a`..`z` and `ROWMAP` maps `A`..`Z` to ids, and both are reset *)
(* at every signature (`PSIG`, checker.f:3066).  That reset is exactly *)
(* what makes "the same letter means the same variable" hold within    *)
(* one signature and not across signatures.                            *)
(* ------------------------------------------------------------------ *)

Definition tyvar : Type := nat.
Definition rowvar : Type := nat.

(* ------------------------------------------------------------------ *)
(* The concrete type table (`CT-INIT`, checker.f:1121-1153).           *)
(* ------------------------------------------------------------------ *)

(* docs/effects.md:16 lists only `i64 u8 u32 cell bool char str addr` as
   concrete names.  `CT-INIT` also registers `n`, `f`, `r`, `f32`, and `u16`,
   and the specification's own examples use `n` (docs/effects.md:43, 172).
   The grammar as written cannot parse the document it belongs to. *)
Inductive con : Type :=
  | CN | CF | CR
  | CI64 | CU8 | CU32 | CCell | CChar | CStr | CAddr | CBool
  | CIdx | CLen | CCount | COff | CFd | CRc | CPid | CMs | CNs | CTok
  | CReg | CLabel | CVa | CSymidx | CAsm | CImg | CSnap
  | CF32 | CU16
  | CLinear (slot : nat).   (* a `DEFLINEAR` name: `CT-ADD-LINEAR`, checker.f:2766 *)

(* `CC-MAX`, checker.f:987.  A declared linear type takes the next free code. *)
Definition con_max : nat := 31.

Definition con_code (c : con) : nat :=
  match c with
  | CN => 1 | CF => 2 | CR => 3
  | CI64 => 4 | CU8 => 5 | CU32 => 6 | CCell => 7
  | CChar => 8 | CStr => 9 | CAddr => 10 | CBool => 11
  | CIdx => 12 | CLen => 13 | CCount => 14 | COff => 15
  | CFd => 16 | CRc => 17 | CPid => 18 | CMs => 19
  | CNs => 20 | CTok => 21 | CReg => 22 | CLabel => 23
  | CVa => 24 | CSymidx => 25 | CAsm => 26 | CImg => 27
  | CSnap => 28 | CF32 => 29 | CU16 => 30
  | CLinear slot => con_max + slot
  end.

Definition con_eqb (a b : con) : bool := Nat.eqb (con_code a) (con_code b).

(* `CT-NONE`..`CT-LINEAR`, checker.f:995-1001. *)
Inductive cls : Type :=
  | ClsInt | ClsRole | ClsBool | ClsFloat | ClsObj | ClsLinear.

Definition cls_code (c : cls) : nat :=
  match c with
  | ClsInt => 1 | ClsRole => 2 | ClsBool => 3
  | ClsFloat => 4 | ClsObj => 5 | ClsLinear => 6
  end.

Definition cls_eqb (a b : cls) : bool := Nat.eqb (cls_code a) (cls_code b).

(* `CS-NONE`..`CS-ADDR`, checker.f:1003-1007. *)
Inductive sgn : Type := SgNone | SgGeneric | SgSigned | SgUnsigned | SgAddr.

Definition sgn_code (s : sgn) : nat :=
  match s with
  | SgNone => 0 | SgGeneric => 1 | SgSigned => 2
  | SgUnsigned => 3 | SgAddr => 4
  end.

Definition sgn_eqb (a b : sgn) : bool := Nat.eqb (sgn_code a) (sgn_code b).

Definition con_cls (c : con) : cls :=
  match c with
  | CN | CI64 | CU8 | CU32 | CCell | CChar | CAddr | CU16 => ClsInt
  | CF | CBool => ClsBool
  | CR | CF32 => ClsFloat
  | CStr => ClsObj
  | CIdx | CLen | CCount | COff | CFd | CRc | CPid | CMs | CNs | CTok
  | CReg | CLabel | CVa | CSymidx | CAsm | CImg | CSnap => ClsRole
  | CLinear _ => ClsLinear
  end.

Definition con_width (c : con) : nat :=
  match c with
  | CN | CI64 | CCell | CAddr | CR => 64
  | CU8 | CChar => 8
  | CU32 | CF32 => 32
  | CU16 => 16
  | CF | CBool => 1
  | CStr => 0
  | CIdx | CLen | CCount | COff | CFd | CRc | CPid | CMs | CNs | CTok
  | CReg | CLabel | CVa | CSymidx | CAsm | CImg | CSnap => 64
  | CLinear _ => 64
  end.

Definition con_sgn (c : con) : sgn :=
  match c with
  | CN | CCell => SgGeneric
  | CI64 => SgSigned
  | CU8 | CU32 | CChar | CU16 => SgUnsigned
  | CAddr => SgAddr
  | _ => SgNone
  end.

(* `CT-INT?`, checker.f:1169. *)
Definition int_famb (c : con) : bool := cls_eqb (con_cls c) ClsInt.

(* `CT-LINEAR?`, checker.f:1172. *)
Definition linear_conb (c : con) : bool := cls_eqb (con_cls c) ClsLinear.

(* `INT-WIDENS?`, checker.f:1192-1203.  `got` flows into `want`.  Roles never
   participate: they are `ClsRole`, so the second test already rejects them in
   both directions, which is the fail-closed nominal discipline of
   docs/effects.md:47-57. *)
Definition int_widensb (got want : con) : bool :=
  if con_eqb got want then true
  else if negb (int_famb got && int_famb want) then false
  else if con_eqb got CN then true
  else if con_eqb want CN then true
  else if negb (Nat.leb (con_width got) (con_width want)) then false
  else if sgn_eqb (con_sgn got) SgGeneric then true
  else if sgn_eqb (con_sgn want) SgGeneric then true
  else if sgn_eqb (con_sgn got) (con_sgn want) then true
  else sgn_eqb (con_sgn got) SgUnsigned
       && sgn_eqb (con_sgn want) SgSigned
       && Nat.ltb (con_width got) (con_width want).

(* `UNIFY-KIND`: `UK-EXACT` / `UK-INPUT` / `UK-COERCE`, checker.f:1671-1685.
   `UkCoerce` additionally admits VALUE-RECORD field coercion (`FIELD-COERCE?`,
   checker.f:1601), which this fragment does not model; here it behaves as
   `UkInput`. *)
Inductive ukind : Type := UkExact | UkInput | UkCoerce.

(* `UNIFY-WIDEN?`, checker.f:1205-1207. *)
Definition uk_widenb (k : ukind) : bool :=
  match k with UkExact => false | UkInput => true | UkCoerce => true end.

(* `CON-OK?`, checker.f:1211-1217.  `strict` is `CUR-STRICT`, set by
   `PAIR-STRICT` when descending into a pointer pointee and inherited by every
   pair below it — the pointee invariance of docs/effects.md:155-163. *)
Definition con_okb (k : ukind) (strict : bool) (got want : con) : bool :=
  if con_eqb got want then true
  else if uk_widenb k && negb strict then int_widensb got want
  else if con_eqb got CN && int_famb want then true
  else if con_eqb want CN && int_famb got then true
  else false.

(* ------------------------------------------------------------------ *)
(* Terms.                                                              *)
(*                                                                     *)
(* `ty` is the checker's tagged term (`T-VAR`/`T-CON`/`T-PTR`/`T-QUOT`/ *)
(* `T-PARAM`, checker.f:1-3).  `stack` is its row: `S-PUSH` cons cells  *)
(* over an `S-ROW` bottom (`MK-PUSH`/`MK-ROW`, checker.f:692-700).      *)
(*                                                                     *)
(* Two representation facts matter and are easy to get wrong.          *)
(*                                                                     *)
(* 1. The HEAD of an `SPush` is the TOP of the stack.  `MK-PUSH` takes  *)
(*    ( type rest -- push ), so the deepest cell is nearest the row     *)
(*    variable.  `stack_of` below takes types in surface order          *)
(*    (bottom first), which is how a signature is written and how       *)
(*    `PSTACK` folds them (checker.f:2977-2985).                        *)
(*                                                                     *)
(* 2. There is NO closed/empty stack.  Every row bottoms out in a row   *)
(*    variable: `PSTACK` uses the leading upper-case letter if there is *)
(*    one and otherwise the implicit tail it was handed.  A stack       *)
(*    written without a row variable is therefore OPEN, sharing one     *)
(*    implicit row with the rest of the signature.  This is why an      *)
(*    arity mismatch surfaces as a row occurs-check failure rather than *)
(*    as a nil/cons clash.                                              *)
(* ------------------------------------------------------------------ *)

(* A note on the CONCRETE syntax, which this file does not parse but which a
   reader will otherwise trip over.  There are two different spellings of the
   return clause, and the specification documents only one of them.

     - A top-level signature is `( Din | Rin -- Dout | Rout )`.  `PSIDE`
       (checker.f:3057-3062) reads the `|` INSIDE each side and `PSIG`
       (checker.f:3064-3072) puts the `--` between the two sides.
     - A quotation is `[ in -- out | rin -- rout ]`: the `|` comes after the
       data `--` (`PSTACK`, checker.f:2986-3010).

   docs/effects.md:13 gives the quotation shape as the grammar for both, and
   its own `>R` / `R>` examples (docs/effects.md:174-175) are written that way.
   Measured against bin/hb: `( R a | S -- R | S a )` certifies a `>R`/`R>`
   round trip and `( R a -- R | S -- S a )` does not.  The four-row abstract
   syntax below is right either way; only the surface spelling differs. *)

Inductive ty : Type :=
  | TVar : tyvar -> ty                (* T-VAR:   a lower-case signature letter *)
  | TCon : con -> ty                  (* T-CON:   a table type or role *)
  (* T-PARAM at arity 0.  docs/effects.md:18 puts `DEFTYPE` and `DEFLINEAR`
     names in one grammar class, but they are two different things in the
     code.  `DEFLINEAR` adds a global row to the concrete type table
     (`CT-ADD-LINEAR`, checker.f:2766), so it is a `TCon` above.  `DEFTYPE`
     mints a package-scoped arity-0 TYPE FAMILY (`VNOM:MINT` ->
     `CHECKER-DEFFAMILY`, lib/type/deftype.f:106-112), which is this
     constructor; lib/type/deftype.f:22 records that the table route was
     retired.  Measured: a `DEFTYPE SERIAL` mismatch reports
     `expected: n actual: serial<>`, the family-application rendering, not
     the bare name docs/effects.md:70 promises. *)
  | TFam : nat -> ty
  | TPtr : ty -> ty                   (* T-PTR:   `ptr t` *)
  | TQuot : eff -> ty                 (* T-QUOT:  `[ in -- out ]` *)
with eff : Type :=
  | Eff : stack -> stack -> stack -> stack -> eff   (* Din Dout Rin Rout *)
with stack : Type :=
  | SRow : rowvar -> stack
  | SPush : ty -> stack -> stack.

Definition eff_din (e : eff) : stack := match e with Eff d _ _ _ => d end.
Definition eff_dout (e : eff) : stack := match e with Eff _ d _ _ => d end.
Definition eff_rin (e : eff) : stack := match e with Eff _ _ r _ => r end.
Definition eff_rout (e : eff) : stack := match e with Eff _ _ _ r => r end.

(* A stored word effect also records whether its signature actually wrote a
   `|` clause (`SGHASR` / `ER.HASR`, checker.f:3060-3062).  A word with no
   return clause leaves the caller's return row untouched at a call site
   (`EFF-APPLY`, checker.f:4629-4637); it does not unify against it. *)
Structure word_eff : Type :=
  MkWordEff { we_eff : eff; we_hasr : bool }.

Definition we_din (w : word_eff) : stack := eff_din (we_eff w).
Definition we_dout (w : word_eff) : stack := eff_dout (we_eff w).
Definition we_rin (w : word_eff) : stack := eff_rin (we_eff w).
Definition we_rout (w : word_eff) : stack := eff_rout (we_eff w).

(* Surface order: `stack_of r [a; b]` is the stack written `R a b`, i.e. `b`
   on top. *)
Definition stack_of (r : rowvar) (ts : list ty) : stack :=
  fold_left (fun s t => SPush t s) ts (SRow r).

(* --- sizes, used only as fuel bounds ------------------------------- *)

Fixpoint ty_size (t : ty) : nat :=
  match t with
  | TVar _ => 1
  | TCon _ => 1
  | TFam _ => 1
  | TPtr u => S (ty_size u)
  | TQuot e => S (eff_size e)
  end
with eff_size (e : eff) : nat :=
  match e with
  | Eff a b c d => S (stack_size a + stack_size b + stack_size c + stack_size d)
  end
with stack_size (s : stack) : nat :=
  match s with
  | SRow _ => 1
  | SPush t rest => S (ty_size t + stack_size rest)
  end.

(* --- structural equality ------------------------------------------- *)

Fixpoint ty_eqb (a b : ty) : bool :=
  match a, b with
  | TVar x, TVar y => Nat.eqb x y
  | TCon x, TCon y => con_eqb x y
  | TFam x, TFam y => Nat.eqb x y
  | TPtr x, TPtr y => ty_eqb x y
  | TQuot x, TQuot y => eff_eqb x y
  | _, _ => false
  end
with eff_eqb (a b : eff) : bool :=
  match a, b with
  | Eff a1 a2 a3 a4, Eff b1 b2 b3 b4 =>
      stack_eqb a1 b1 && stack_eqb a2 b2 && stack_eqb a3 b3 && stack_eqb a4 b4
  end
with stack_eqb (a b : stack) : bool :=
  match a, b with
  | SRow x, SRow y => Nat.eqb x y
  | SPush t1 r1, SPush t2 r2 => ty_eqb t1 t2 && stack_eqb r1 r2
  | _, _ => false
  end.

(* ------------------------------------------------------------------ *)
(* Substitution.                                                       *)
(*                                                                     *)
(* The checker's `TVT`/`RVT` arrays are a chained store: binding a      *)
(* variable records one term, which may itself mention bound variables, *)
(* and `T-RES`/`R-RES` (checker.f:817-834) walk that chain to a         *)
(* non-variable head or an unbound variable.  `sub_raw` records the     *)
(* `TVK-RAW` kind (checker.f:660-680) that raw storage definers stamp   *)
(* on the cells they publish.                                          *)
(* ------------------------------------------------------------------ *)

Structure subst : Type := MkSubst {
  sub_ty : list (tyvar * ty);
  sub_row : list (rowvar * stack);
  sub_raw : list tyvar
}.

Definition empty_subst : subst := MkSubst [] [] [].

Fixpoint lookup_ty (l : list (tyvar * ty)) (v : tyvar) : option ty :=
  match l with
  | [] => None
  | (w, t) :: rest => if Nat.eqb w v then Some t else lookup_ty rest v
  end.

Fixpoint lookup_row (l : list (rowvar * stack)) (r : rowvar) : option stack :=
  match l with
  | [] => None
  | (q, s) :: rest => if Nat.eqb q r then Some s else lookup_row rest r
  end.

Fixpoint mem_nat (v : nat) (l : list nat) : bool :=
  match l with
  | [] => false
  | w :: rest => if Nat.eqb w v then true else mem_nat v rest
  end.

Definition bind_ty (s : subst) (v : tyvar) (t : ty) : subst :=
  MkSubst ((v, t) :: sub_ty s) (sub_row s) (sub_raw s).

Definition bind_row (s : subst) (r : rowvar) (x : stack) : subst :=
  MkSubst (sub_ty s) ((r, x) :: sub_row s) (sub_raw s).

(* `TVK-RAISE`, checker.f:674: meeting a RAW cell raises the other side. *)
Definition raise_raw (s : subst) (v : tyvar) : subst :=
  if mem_nat v (sub_raw s) then s
  else MkSubst (sub_ty s) (sub_row s) (v :: sub_raw s).

Definition raw_varb (s : subst) (v : tyvar) : bool := mem_nat v (sub_raw s).

Definition sub_size (s : subst) : nat :=
  fold_left (fun n p => n + ty_size (snd p)) (sub_ty s) 0
  + fold_left (fun n p => n + stack_size (snd p)) (sub_row s) 0.

(* Head resolution consumes one distinct binding per hop, so the number of
   bindings is a sufficient bound.  Walks that descend through resolved terms
   need more; `walk_budget` is a deliberately generous quadratic bound, and no
   theorem here proves it sufficient — exhausting it is fail-closed. *)
Definition res_budget (s : subst) : nat :=
  S (length (sub_ty s) + length (sub_row s)).

Definition walk_budget (s : subst) (n : nat) : nat :=
  S (sub_size s + n) * S (sub_size s + n).

(* `T-RES`, checker.f:817-834: resolve the HEAD only. *)
Fixpoint resolve_ty_fuel (fuel : nat) (s : subst) (t : ty) : ty :=
  match fuel with
  | 0 => t
  | S f =>
      match t with
      | TVar v =>
          match lookup_ty (sub_ty s) v with
          | Some t' => resolve_ty_fuel f s t'
          | None => t
          end
      | _ => t
      end
  end.

Definition resolve_ty (s : subst) (t : ty) : ty :=
  resolve_ty_fuel (res_budget s) s t.

(* `R-RES`, checker.f:832-834. *)
Fixpoint resolve_row_fuel (fuel : nat) (s : subst) (x : stack) : stack :=
  match fuel with
  | 0 => x
  | S f =>
      match x with
      | SRow r =>
          match lookup_row (sub_row s) r with
          | Some x' => resolve_row_fuel f s x'
          | None => x
          end
      | SPush _ _ => x
      end
  end.

Definition resolve_row (s : subst) (x : stack) : stack :=
  resolve_row_fuel (res_budget s) s x.

(* Deep resolution.  The checker never needs this — it inspects terms through
   `T-RES`/`R-RES` one head at a time — but stating what a composed effect IS
   requires pushing the substitution all the way down.  It decides nothing, so
   an exhausted budget returning the term unchanged is harmless here. *)
Fixpoint zonk_ty_fuel (fuel : nat) (s : subst) (t : ty) : ty :=
  match fuel with
  | 0 => t
  | S f =>
      match resolve_ty s t with
      | TVar v => TVar v
      | TCon c => TCon c
      | TFam n => TFam n
      | TPtr u => TPtr (zonk_ty_fuel f s u)
      | TQuot (Eff a b c d) =>
          TQuot (Eff (zonk_row_fuel f s a) (zonk_row_fuel f s b)
                     (zonk_row_fuel f s c) (zonk_row_fuel f s d))
      end
  end
with zonk_row_fuel (fuel : nat) (s : subst) (x : stack) : stack :=
  match fuel with
  | 0 => x
  | S f =>
      match resolve_row s x with
      | SRow r => SRow r
      | SPush t rest => SPush (zonk_ty_fuel f s t) (zonk_row_fuel f s rest)
      end
  end.

Definition zonk_ty (s : subst) (t : ty) : ty :=
  zonk_ty_fuel (walk_budget s (ty_size t)) s t.

Definition zonk_row (s : subst) (x : stack) : stack :=
  zonk_row_fuel (walk_budget s (stack_size x)) s x.

(* ------------------------------------------------------------------ *)
(* Occurs checks.                                                      *)
(*                                                                     *)
(* `TY-OCC?` (checker.f:1261-1287) descends through pointers, all four  *)
(* rows of a quotation, and family arguments.  `ROW-OCC?`               *)
(* (checker.f:963-977) walks a spine and descends through a cell's      *)
(* pointer chain into a quotation's four rows — the omega combinator    *)
(* must reject rather than loop.  Both resolve as they go.              *)
(* ------------------------------------------------------------------ *)

Fixpoint ty_occ_fuel (fuel : nat) (s : subst) (v : tyvar) (t : ty) : bool :=
  match fuel with
  | 0 => true   (* fail closed: an exhausted walk reports an occurrence *)
  | S f =>
      match resolve_ty s t with
      | TVar w => Nat.eqb w v
      | TCon _ => false
      | TFam _ => false
      | TPtr u => ty_occ_fuel f s v u
      | TQuot (Eff a b c d) =>
          ty_occ_row_fuel f s v a || ty_occ_row_fuel f s v b
          || ty_occ_row_fuel f s v c || ty_occ_row_fuel f s v d
      end
  end
with ty_occ_row_fuel (fuel : nat) (s : subst) (v : tyvar) (x : stack) : bool :=
  match fuel with
  | 0 => true
  | S f =>
      match resolve_row s x with
      | SRow _ => false
      | SPush t rest => ty_occ_fuel f s v t || ty_occ_row_fuel f s v rest
      end
  end.

Definition ty_occurs (s : subst) (v : tyvar) (t : ty) : bool :=
  ty_occ_fuel (walk_budget s (ty_size t)) s v t.

Fixpoint row_occ_ty_fuel (fuel : nat) (s : subst) (r : rowvar) (t : ty) : bool :=
  match fuel with
  | 0 => true
  | S f =>
      match resolve_ty s t with
      | TPtr u => row_occ_ty_fuel f s r u
      | TQuot (Eff a b c d) =>
          row_occ_fuel f s r a || row_occ_fuel f s r b
          || row_occ_fuel f s r c || row_occ_fuel f s r d
      | _ => false
      end
  end
with row_occ_fuel (fuel : nat) (s : subst) (r : rowvar) (x : stack) : bool :=
  match fuel with
  | 0 => true
  | S f =>
      match resolve_row s x with
      | SRow q => Nat.eqb q r
      | SPush t rest => row_occ_ty_fuel f s r t || row_occ_fuel f s r rest
      end
  end.

Definition row_occurs (s : subst) (r : rowvar) (x : stack) : bool :=
  row_occ_fuel (walk_budget s (stack_size x)) s r x.

(* ------------------------------------------------------------------ *)
(* The RAW storage discipline (`RAW-OK?` / `RAW-BLOCK?`,                *)
(* checker.f:672-682).  A cell published by `here`/`create`/`variable`/ *)
(* `constant` is marked RAW; it admits a plain scalar or role and a     *)
(* pointer whose pointee is likewise admissible, raises any variable it *)
(* meets to RAW, and rejects a nominal family or a linear con.          *)
(* ------------------------------------------------------------------ *)

Fixpoint raw_ok_fuel (fuel : nat) (s : subst) (t : ty) : option subst :=
  match fuel with
  | 0 => None
  | S f =>
      match resolve_ty s t with
      | TVar v => Some (raise_raw s v)
      | TFam _ => None
      | TCon c => if linear_conb c then None else Some s
      | TPtr u => raw_ok_fuel f s u
      | TQuot _ => Some s        (* the engine legitimately raw-stores xts *)
      end
  end.

Definition raw_blockb (s : subst) (v : tyvar) (t : ty) : option subst :=
  if negb (raw_varb s v) then Some s
  else raw_ok_fuel (walk_budget s (ty_size t)) s t.

(* ------------------------------------------------------------------ *)
(* Unification.                                                        *)
(*                                                                     *)
(* `UNIFY` (checker.f:1662-1669) is a worklist over pairs; a pair whose *)
(* first element is a row runs `U-ROW`, otherwise `U-TYPE`.  Each pair  *)
(* carries the strictness it inherited (`PAIR` / `PAIR-STRICT`,         *)
(* checker.f:862-870).  The got/want orientation is the caller's:       *)
(* `CHECKER-STEP` pairs the CURRENT row first and the DECLARED row      *)
(* second, so widening is "got flows into want".                       *)
(* ------------------------------------------------------------------ *)

Inductive upair : Type :=
  | URow (strict : bool) (a b : stack)
  | UTy (strict : bool) (a b : ty).

(* `U-TYPE`, checker.f:1584-1610.  Arm order follows the checker's, minus the
   atom, field, and layout arms this fragment does not model. *)
Definition u_ty (k : ukind) (s : subst) (strict : bool) (a b : ty)
  : option (subst * list upair) :=
  let ra := resolve_ty s a in
  let rb := resolve_ty s b in
  if ty_eqb ra rb then Some (s, [])
  else
    match ra, rb with
    | TQuot (Eff d1 o1 r1 q1), TQuot (Eff d2 o2 r2 q2) =>
        Some (s, [URow strict q1 q2; URow strict r1 r2;
                  URow strict o1 o2; URow strict d1 d2])
    (* PAIR-STRICT: a pointee never widens, at any nesting depth. *)
    | TPtr u, TPtr w => Some (s, [UTy true u w])
    | TVar v, _ =>
        if ty_occurs s v rb then None
        else match raw_blockb s v rb with
             | None => None
             | Some s' => Some (bind_ty s' v rb, [])
             end
    | _, TVar v =>
        if ty_occurs s v ra then None
        else match raw_blockb s v ra with
             | None => None
             | Some s' => Some (bind_ty s' v ra, [])
             end
    | TCon c1, TCon c2 =>
        if con_okb k strict c1 c2 then Some (s, []) else None
    (* PARAM-FAM-OK?, checker.f:1229: identity is the resolved family id. *)
    | TFam f1, TFam f2 => if Nat.eqb f1 f2 then Some (s, []) else None
    | _, _ => None
    end.

(* `U-ROW`, checker.f:1654-1660.  A row is only ever `S-ROW` or `S-PUSH`, so
   the only way this fails is the occurs check. *)
Definition u_row (s : subst) (strict : bool) (a b : stack)
  : option (subst * list upair) :=
  let ra := resolve_row s a in
  let rb := resolve_row s b in
  if stack_eqb ra rb then Some (s, [])
  else
    match ra, rb with
    | SRow r, _ => if row_occurs s r rb then None else Some (bind_row s r rb, [])
    | _, SRow r => if row_occurs s r ra then None else Some (bind_row s r ra, [])
    | SPush t1 rest1, SPush t2 rest2 =>
        Some (s, [UTy strict t1 t2; URow strict rest1 rest2])
    end.

Fixpoint unify_loop (fuel : nat) (k : ukind) (s : subst) (wl : list upair)
  : option subst :=
  match fuel with
  | 0 => None   (* fail closed *)
  | S f =>
      match wl with
      | [] => Some s
      | URow st a b :: rest =>
          match u_row s st a b with
          | None => None
          | Some (s', gen) => unify_loop f k s' (gen ++ rest)
          end
      | UTy st a b :: rest =>
          match u_ty k s st a b with
          | None => None
          | Some (s', gen) => unify_loop f k s' (gen ++ rest)
          end
      end
  end.

Definition unify_budget (s : subst) (n : nat) : nat :=
  walk_budget s n * S n.

(* `UNIFY` opens at `CUR-STRICT = 0` (checker.f:1663). *)
Definition unify_stack (k : ukind) (s : subst) (got want : stack) : option subst :=
  unify_loop (unify_budget s (stack_size got + stack_size want)) k s
             [URow false got want].

Definition unify_ty (k : ukind) (s : subst) (got want : ty) : option subst :=
  unify_loop (unify_budget s (ty_size got + ty_size want)) k s
             [UTy false got want].

(* The verdict alone, when the resulting substitution is not the point. *)
Definition unifiesb (r : option subst) : bool :=
  match r with Some _ => true | None => false end.

(* ------------------------------------------------------------------ *)
(* Instantiation.                                                      *)
(*                                                                     *)
(* A stored effect is instantiated with fresh variables at every call   *)
(* site (`E-INST`, used by `EFF-APPLY`, checker.f:4629-4633).  Because  *)
(* the checker's variables come from one counter, a uniform shift above *)
(* every id already in play is a faithful freshening.                   *)
(* ------------------------------------------------------------------ *)

Fixpoint shift_ty (d : nat) (t : ty) : ty :=
  match t with
  | TVar v => TVar (d + v)
  | TCon c => TCon c
  | TFam f => TFam f
  | TPtr u => TPtr (shift_ty d u)
  | TQuot e => TQuot (shift_eff d e)
  end
with shift_eff (d : nat) (e : eff) : eff :=
  match e with
  | Eff a b c q => Eff (shift_row d a) (shift_row d b) (shift_row d c) (shift_row d q)
  end
with shift_row (d : nat) (x : stack) : stack :=
  match x with
  | SRow r => SRow (d + r)
  | SPush t rest => SPush (shift_ty d t) (shift_row d rest)
  end.

Fixpoint max_ty (t : ty) : nat :=
  match t with
  | TVar v => v
  | TCon _ => 0
  | TFam _ => 0
  | TPtr u => max_ty u
  | TQuot e => max_eff e
  end
with max_eff (e : eff) : nat :=
  match e with
  | Eff a b c q => Nat.max (Nat.max (max_row a) (max_row b))
                           (Nat.max (max_row c) (max_row q))
  end
with max_row (x : stack) : nat :=
  match x with
  | SRow r => r
  | SPush t rest => Nat.max (max_ty t) (max_row rest)
  end.

Definition next_eff (e : eff) : nat := S (max_eff e).

Definition next_sub (s : subst) : nat :=
  S (fold_left (fun n p => Nat.max n (Nat.max (fst p) (max_ty (snd p))))
               (sub_ty s)
               (fold_left (fun n p => Nat.max n (Nat.max (fst p) (max_row (snd p))))
                          (sub_row s) 0)).

Definition instantiate (above : nat) (w : word_eff) : word_eff :=
  MkWordEff (shift_eff above (we_eff w)) (we_hasr w).

(* ------------------------------------------------------------------ *)
(* Sequential composition.                                             *)
(*                                                                     *)
(* This is exactly what the checker does at a call site.  `CHECKER-STEP` *)
(* (checker.f:1928-1936) unifies the CURRENT data row against the       *)
(* callee's declared input row under `UK-INPUT`, then REPLACES the      *)
(* current row with the callee's declared output row.  `EFF-APPLY`      *)
(* (checker.f:4629-4637) does the same for the return rows, but ONLY if *)
(* the callee's signature actually declared a `|` clause; otherwise the *)
(* caller's return row is left untouched.                               *)
(*                                                                     *)
(* `compose s e1 e2` therefore assumes e1 and e2 already share one      *)
(* variable space (e1 is the running state, e2 the freshly instantiated *)
(* callee).  `compose_fresh` does the instantiation first, which is     *)
(* what a call site does.                                               *)
(* ------------------------------------------------------------------ *)

Definition compose (s : subst) (e1 e2 : word_eff) : option (subst * word_eff) :=
  match unify_stack UkInput s (we_dout e1) (we_din e2) with
  | None => None
  | Some s1 =>
      if we_hasr e2
      then match unify_stack UkInput s1 (we_rout e1) (we_rin e2) with
           | None => None
           | Some s2 =>
               Some (s2, MkWordEff (Eff (we_din e1) (we_dout e2)
                                        (we_rin e1) (we_rout e2)) true)
           end
      else Some (s1, MkWordEff (Eff (we_din e1) (we_dout e2)
                                    (we_rin e1) (we_rout e1)) (we_hasr e1))
  end.

Definition compose_fresh (s : subst) (e1 e2 : word_eff) : option (subst * word_eff) :=
  let above := Nat.max (next_sub s) (Nat.max (next_eff (we_eff e1)) (next_eff (we_eff e2))) in
  compose s e1 (instantiate above e2).

(* The running state a body starts from: the declared input rows, unchanged. *)
Definition id_eff (din rin : stack) : word_eff :=
  MkWordEff (Eff din din rin rin) true.

Fixpoint run_calls (s : subst) (cur : word_eff) (calls : list word_eff)
  : option (subst * word_eff) :=
  match calls with
  | [] => Some (s, cur)
  | c :: cs =>
      match compose_fresh s cur c with
      | None => None
      | Some (s', cur') => run_calls s' cur' cs
      end
  end.

(* ------------------------------------------------------------------ *)
(* The declared signature and its sealed implicit row.                 *)
(*                                                                     *)
(* `PSIG` (checker.f:3064-3072) allocates ONE fresh row variable and    *)
(* hands it to both sides of the signature as the implicit tail, so a   *)
(* declaration written without row letters has the SAME open row below  *)
(* its inputs and below its outputs.  That variable is remembered as    *)
(* `PD-BASE` and latched into `SGDBASE`; the return-side counterpart    *)
(* `RR-SHARED` is allocated only when a `|` clause appears, so it is    *)
(* absent for an ordinary signature.                                    *)
(*                                                                     *)
(* The seal is `CHECK-NO-BORROW` (checker.f:7715-7721), and its shape   *)
(* is the single most important thing to get right here: it is NOT a    *)
(* rigidity flag consulted during unification.  Unification proceeds    *)
(* normally, a callee is free to bind the base row while the body is    *)
(* being checked, and only at the END of the definition does the        *)
(* checker resolve the base row and require it to still be a ROW —      *)
(* `ROW-OPEN?`.  So a callee that merely passes the row along (binding  *)
(* it to another row variable) is fine, and a callee that consumed      *)
(* below the declared inputs left an `S-PUSH` there and is rejected.    *)
(* That is the hidden-underflow rule of docs/effects.md:28-31.          *)
(* ------------------------------------------------------------------ *)

Structure decl : Type := MkDecl {
  decl_dbase : rowvar;              (* PD-BASE / SGDBASE *)
  decl_rbase : option rowvar;       (* RR-SHARED / SGRBASE; None when no `|` clause *)
  decl_rbrow : rowvar;              (* RBROW: always allocated, checker.f:1714 *)
  decl_eff : word_eff
}.

(* Build a declaration the way `PSIG` does: one implicit data row shared by
   both sides.  A return row variable always exists (`CHECK-RESET` allocates
   `RBROW`), but it is a DECLARED base — and so a sealed one — only when the
   signature actually wrote a `|` clause. *)
Definition decl_plain (dbase rbrow : rowvar) (din dout : list ty) : decl :=
  MkDecl dbase None rbrow
    (MkWordEff (Eff (stack_of dbase din) (stack_of dbase dout)
                    (SRow rbrow) (SRow rbrow)) false).

Definition decl_with_return (dbase rbase : rowvar) (din dout rin rout : list ty) : decl :=
  MkDecl dbase (Some rbase) rbase
    (MkWordEff (Eff (stack_of dbase din) (stack_of dbase dout)
                    (stack_of rbase rin) (stack_of rbase rout)) true).

(* `ROW-OPEN?`, checker.f:7712-7713. *)
Definition row_openb (s : subst) (r : rowvar) : bool :=
  match resolve_row s (SRow r) with
  | SRow _ => true
  | SPush _ _ => false
  end.

(* `CHECK-ROW-NOT-BORROWED`, checker.f:7715-7717: a base row of 0 (never
   allocated) is skipped, which is `None` here. *)
Definition row_not_borrowedb (s : subst) (r : option rowvar) : bool :=
  match r with
  | None => true
  | Some q => row_openb s q
  end.

(* `CHECK-NO-BORROW`, checker.f:7719-7721. *)
Definition seal_okb (s : subst) (d : decl) : bool :=
  row_openb s (decl_dbase d) && row_not_borrowedb s (decl_rbase d).

(* checker.f:10132.  A definition with no `|` clause must hand the return row
   back exactly as it found it: `RCUR` must resolve to `RBROW`.  This is a
   term-identity test, not a unification, so it cannot bind anything. *)
Definition return_balancedb (s : subst) (d : decl) (cur : word_eff) : bool :=
  if we_hasr (decl_eff d) then true
  else stack_eqb (resolve_row s (we_rout cur)) (resolve_row s (SRow (decl_rbrow d))).

(* The definition boundary, in the checker's order (checker.f:10126-10133):
   scan the body, THEN the seal, THEN the return-row balance, THEN join the
   reached output row against the declared one under `UK-COERCE`.  Modelling
   the order matters: a body that both underflows and mismatches its output is
   rejected by the seal first. *)
Definition check_body (d : decl) (calls : list word_eff) : bool :=
  match run_calls empty_subst
                  (id_eff (we_din (decl_eff d)) (we_rin (decl_eff d))) calls with
  | None => false
  | Some (s, cur) =>
      if negb (seal_okb s d) then false
      else if negb (return_balancedb s d cur) then false
      else match unify_stack UkCoerce s (we_dout cur) (we_dout (decl_eff d)) with
           | None => false
           | Some _ => true
           end
  end.

(* ------------------------------------------------------------------ *)
(* Definitional examples.                                              *)
(*                                                                     *)
(* Convenience spellings for the examples below.  Row variable 0 is the *)
(* implicit data row of a declaration; callee effects use their own low *)
(* ids and are shifted above everything at each call.                   *)
(* ------------------------------------------------------------------ *)

Definition i64 : ty := TCon CI64.
Definition u8 : ty := TCon CU8.
Definition u32 : ty := TCon CU32.
Definition cellt : ty := TCon CCell.
Definition boolt : ty := TCon CBool.
Definition strt : ty := TCon CStr.
Definition nt : ty := TCon CN.
Definition addrt : ty := TCon CAddr.
Definition idxt : ty := TCon CIdx.
Definition lent : ty := TCon CLen.
Definition imgt : ty := TCon CImg.

(* A word with no `|` clause, written over its own row letter. *)
Definition prim (r : rowvar) (din dout : list ty) : word_eff :=
  MkWordEff (Eff (stack_of r din) (stack_of r dout) (SRow r) (SRow r)) false.

(* --- 1. Composition of two concrete effects ------------------------ *)

(* `DUP` at i64, then `+`.  Composing `( R i64 -- R i64 i64 )` with
   `( S i64 i64 -- S i64 )` must unify S with R and leave `( R i64 -- R i64 )`. *)

Definition dup_i64 : word_eff := prim 1 [i64] [i64; i64].
Definition add_i64 : word_eff := prim 1 [i64; i64] [i64].

(* Written over the SAME row letter, the two effects meet without binding
   anything, and the composite is exactly `( R i64 -- R i64 )`. *)
Example compose_two_concrete_effects :
  compose empty_subst dup_i64 add_i64 = Some (empty_subst, prim 1 [i64] [i64]).
Proof. vm_compute; reflexivity. Qed.

(* A real call site instantiates the callee first, so the two effects start in
   disjoint variable spaces and unification joins their rows.  The composite
   still has one shared open tail under a single i64. *)
Example compose_fresh_instantiates_the_callee :
  match compose_fresh empty_subst dup_i64 add_i64 with
  | Some (s, w) =>
      stack_eqb (zonk_row s (we_din w)) (zonk_row s (we_dout w)) = true
      /\ match zonk_row s (we_din w) with
         | SPush t (SRow _) => ty_eqb t i64
         | _ => false
         end = true
  | None => False
  end.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* Composition threads the tail: the same two effects composed under a caller
   whose row already carries a `str` below leave that `str` in place. *)
Definition dup_add_under_str : option (subst * word_eff) :=
  compose_fresh empty_subst
    (MkWordEff (Eff (SPush strt (SRow 1)) (SPush i64 (SPush strt (SRow 1)))
                    (SRow 1) (SRow 1)) false)
    add_i64.

Example composition_needs_both_operands :
  dup_add_under_str = None.
Proof. vm_compute; reflexivity. Qed.

(* --- 2. A row variable capturing a deeper tail --------------------- *)

(* A word that pushes one literal, then a word that consumes two.  The second
   reaches one cell BELOW what the first supplied, so the caller's row
   variable is bound to a spine — it captured a deeper tail. *)

Definition push_i64 : word_eff := prim 1 [] [i64].
Definition lt_i64 : word_eff := prim 1 [i64; i64] [boolt].

Definition literal_then_compare : option (subst * word_eff) :=
  compose_fresh empty_subst push_i64 lt_i64.

Example row_var_captures_deeper_tail :
  match literal_then_compare with
  | Some (s, _) =>
      (* row 1 is no longer open: it absorbed the extra i64 the callee wanted *)
      row_openb s 1 = false
      /\ match resolve_row s (SRow 1) with
         | SPush t _ => ty_eqb (resolve_ty s t) i64
         | SRow _ => false
         end = true
  | None => False
  end.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* --- 3. Type-variable binding, and same letter means same type ----- *)

(* `a` is signature letter `a` of one word.  Unifying `R a a` against
   `R i64 i64` binds it once and succeeds. *)

Example tyvar_binds_once :
  match unify_stack UkExact empty_subst
          (stack_of 0 [i64; i64]) (stack_of 1 [TVar 2; TVar 2]) with
  | Some s => ty_eqb (resolve_ty s (TVar 2)) i64 = true
  | None => False
  end.
Proof. vm_compute; reflexivity. Qed.

(* The same letter twice is a real constraint: `R a a` does not accept
   `R i64 bool`. *)
Example same_letter_means_same_type :
  unify_stack UkExact empty_subst
    (stack_of 0 [i64; boolt]) (stack_of 1 [TVar 2; TVar 2]) = None.
Proof. vm_compute; reflexivity. Qed.

(* Two DIFFERENT letters accept the same two types — the constraint is
   sharing, not distinctness. *)
Example different_letters_are_unconstrained :
  match unify_stack UkExact empty_subst
          (stack_of 0 [i64; boolt]) (stack_of 1 [TVar 2; TVar 3]) with
  | Some s =>
      ty_eqb (resolve_ty s (TVar 2)) i64 = true
      /\ ty_eqb (resolve_ty s (TVar 3)) boolt = true
  | None => False
  end.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* --- 4. Rejected unifications -------------------------------------- *)

(* Arity mismatch against the SAME row variable.  There is no closed stack in
   this language, so this is the row occurs check: `R` cannot equal `R i64`. *)
Example arity_mismatch_rejects :
  unify_stack UkExact empty_subst (stack_of 0 [i64]) (stack_of 0 [i64; i64]) = None.
Proof. vm_compute; reflexivity. Qed.

(* Against DIFFERENT row variables the same shapes unify — that is row
   polymorphism, and it is why the arity check above needs the shared row. *)
Example different_rows_absorb_arity :
  match unify_stack UkExact empty_subst
          (stack_of 0 [i64]) (stack_of 1 [i64; i64]) with
  | Some s => row_openb s 0 = false
  | None => False
  end.
Proof. vm_compute; reflexivity. Qed.

(* Conflicting concrete types. *)
Example conflicting_cons_reject :
  unify_stack UkInput empty_subst (stack_of 0 [i64]) (stack_of 0 [strt]) = None
  /\ unify_stack UkInput empty_subst (stack_of 0 [boolt]) (stack_of 0 [i64]) = None.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* Nominal roles are fail-closed: distinct from each other and from `n`, in
   both directions, under every unification kind. *)
Example roles_never_widen :
  unify_stack UkInput empty_subst (stack_of 0 [idxt]) (stack_of 0 [lent]) = None
  /\ unify_stack UkInput empty_subst (stack_of 0 [idxt]) (stack_of 0 [nt]) = None
  /\ unify_stack UkInput empty_subst (stack_of 0 [nt]) (stack_of 0 [idxt]) = None
  /\ unify_stack UkCoerce empty_subst (stack_of 0 [idxt]) (stack_of 0 [nt]) = None.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* Integer widening is a property of the unification KIND, not of the types:
   `u8` reaches an input slot declared `cell`, but an exact join refuses. *)
Example widening_is_input_only :
  unifiesb (unify_stack UkInput empty_subst (stack_of 0 [u8]) (stack_of 0 [cellt])) = true
  /\ unifiesb (unify_stack UkExact empty_subst (stack_of 0 [u8]) (stack_of 0 [cellt])) = false
  /\ unifiesb (unify_stack UkInput empty_subst (stack_of 0 [cellt]) (stack_of 0 [u8])) = false
  (* and `n` still subsumes any integer family in both directions *)
  /\ unifiesb (unify_stack UkExact empty_subst (stack_of 0 [nt]) (stack_of 0 [u8])) = true
  /\ unifiesb (unify_stack UkExact empty_subst (stack_of 0 [u8]) (stack_of 0 [nt])) = true.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* `addr` is in the integer class but carries its own sign (`CS-ADDR`,
   checker.f:1007), which is neither generic nor signed nor unsigned.  So it
   takes an `n` but refuses a narrower concrete integer, even at an input
   position.  Nothing in docs/effects.md says so; only `INT-WIDENS?` does. *)
Example addr_takes_n_but_not_a_narrower_int :
  unifiesb (unify_stack UkInput empty_subst (stack_of 0 [nt]) (stack_of 0 [addrt])) = true
  /\ unifiesb (unify_stack UkInput empty_subst (stack_of 0 [u8]) (stack_of 0 [addrt])) = false
  /\ unifiesb (unify_stack UkInput empty_subst (stack_of 0 [u32]) (stack_of 0 [addrt])) = false.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* A pointee is invariant at any depth: the strictness set on entering a `ptr`
   is inherited, so the widening above is unavailable inside one. *)
Example pointee_is_invariant :
  unify_stack UkInput empty_subst
    (stack_of 0 [TPtr u8]) (stack_of 0 [TPtr cellt]) = None
  /\ unify_stack UkInput empty_subst
       (stack_of 0 [TPtr (TPtr u8)]) (stack_of 0 [TPtr (TPtr cellt)]) = None.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* The row occurs check looks INSIDE a quotation's effect rows, so a row
   variable cannot be bound to a stack that mentions it through a quotation. *)
Example occurs_check_sees_through_quotations :
  unify_stack UkExact empty_subst
    (SRow 0)
    (SPush (TQuot (Eff (SRow 0) (SRow 1) (SRow 2) (SRow 2))) (SRow 3)) = None.
Proof. vm_compute; reflexivity. Qed.

(* Quotation cells unify structurally, through all four of their rows. *)
Example quotation_rows_unify_pairwise :
  match unify_stack UkExact empty_subst
          (stack_of 0 [TQuot (Eff (stack_of 1 [i64]) (stack_of 1 [boolt])
                                  (SRow 2) (SRow 2))])
          (stack_of 0 [TQuot (Eff (stack_of 1 [TVar 3]) (stack_of 1 [TVar 4])
                                  (SRow 2) (SRow 2))]) with
  | Some s =>
      ty_eqb (resolve_ty s (TVar 3)) i64 = true
      /\ ty_eqb (resolve_ty s (TVar 4)) boolt = true
  | None => False
  end.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* Deep resolution recovers the whole quotation effect, not just its head. *)
Example zonk_recovers_a_whole_quotation :
  match unify_stack UkExact empty_subst
          (stack_of 0 [TQuot (Eff (stack_of 1 [i64]) (stack_of 1 [boolt])
                                  (SRow 2) (SRow 2))])
          (stack_of 0 [TQuot (Eff (stack_of 1 [TVar 3]) (stack_of 1 [TVar 4])
                                  (SRow 2) (SRow 2))]) with
  | Some s =>
      ty_eqb
        (zonk_ty s (TQuot (Eff (stack_of 1 [TVar 3]) (stack_of 1 [TVar 4])
                               (SRow 2) (SRow 2))))
        (TQuot (Eff (stack_of 1 [i64]) (stack_of 1 [boolt]) (SRow 2) (SRow 2)))
      = true
  | None => False
  end.
Proof. vm_compute; reflexivity. Qed.

Example quotation_effect_mismatch_rejects :
  unify_stack UkExact empty_subst
    (stack_of 0 [TQuot (Eff (stack_of 1 [i64]) (stack_of 1 [boolt]) (SRow 2) (SRow 2))])
    (stack_of 0 [TQuot (Eff (stack_of 1 [i64]) (stack_of 1 [strt]) (SRow 2) (SRow 2))])
  = None.
Proof. vm_compute; reflexivity. Qed.

(* An arity-0 nominal family (what `DEFTYPE` mints) is identified by family id
   and never meets a table type. *)
Example declared_nominals_are_distinct :
  unifiesb (unify_stack UkInput empty_subst (stack_of 0 [TFam 7]) (stack_of 0 [TFam 8])) = false
  /\ unifiesb (unify_stack UkInput empty_subst (stack_of 0 [TFam 7]) (stack_of 0 [nt])) = false
  /\ unifiesb (unify_stack UkInput empty_subst (stack_of 0 [nt]) (stack_of 0 [TFam 7])) = false
  /\ unifiesb (unify_stack UkInput empty_subst (stack_of 0 [TFam 7]) (stack_of 0 [TFam 7])) = true.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* A cell published by a raw storage definer refuses a nominal family, which is
   the value-position half of the pointee seal. *)
Example raw_cell_refuses_a_nominal :
  let s := MkSubst [] [] [2] in
  unifiesb (unify_stack UkInput s (stack_of 0 [TVar 2]) (stack_of 0 [TFam 7])) = false
  /\ unifiesb (unify_stack UkInput s (stack_of 0 [TVar 2]) (stack_of 0 [nt])) = true
  (* an ordinary variable is unaffected *)
  /\ unifiesb (unify_stack UkInput empty_subst
                 (stack_of 0 [TVar 2]) (stack_of 0 [TFam 7])) = true.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* A `DEFLINEAR` name is an ordinary table con in its own right, distinct from
   every other type; its linearity is a separate conservation pass this
   fragment does not model, but the raw-storage seal already sees it. *)
Example linear_cons_are_nominal :
  unifiesb (unify_stack UkInput empty_subst
              (stack_of 0 [TCon (CLinear 0)]) (stack_of 0 [TCon (CLinear 1)])) = false
  /\ unifiesb (unify_stack UkInput empty_subst
                (stack_of 0 [TCon (CLinear 0)]) (stack_of 0 [nt])) = false
  /\ unifiesb (unify_stack UkInput empty_subst
                (stack_of 0 [TCon (CLinear 0)]) (stack_of 0 [TCon (CLinear 0)])) = true
  /\ unifiesb (unify_stack UkInput (MkSubst [] [] [2])
                (stack_of 0 [TVar 2]) (stack_of 0 [TCon (CLinear 0)])) = false.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* Unifying two types directly is the same relation, entered at a type pair
   instead of a row pair. *)
Example unify_ty_agrees_with_unify_stack :
  unifiesb (unify_ty UkInput empty_subst (TPtr (TVar 1)) (TPtr i64)) = true
  /\ unifiesb (unify_ty UkInput empty_subst (TPtr u8) (TPtr cellt)) = false
  /\ unifiesb (unify_ty UkInput empty_subst u8 cellt) = true.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* --- 5. The sealed implicit row ------------------------------------ *)

(* The motivating case from docs/effects.md:28-31.  A word declared `( -- )`
   calls a trusted `img -- img` boundary.  Unification SUCCEEDS — the callee's
   input is simply taken from below — but the definition's implicit row is no
   longer open, so the seal rejects it. *)

Definition declared_empty : decl := decl_plain 0 9 [] [].
Definition trusted_img : word_eff := prim 1 [imgt] [imgt].
Definition balanced_word : word_eff := prim 1 [] [].

(* This is the whole point of the rule, so it is worth pinning both halves in
   one place: the body composes, AND its output row still joins the declared
   `( -- )` output — unification has no complaint anywhere.  The definition is
   rejected only because the implicit row came back bound to a spine. *)
Example hidden_underflow_unifies_but_fails_the_seal :
  match run_calls empty_subst
          (id_eff (we_din (decl_eff declared_empty)) (we_rin (decl_eff declared_empty)))
          [trusted_img] with
  | Some (s, cur) =>
      seal_okb s declared_empty = false
      /\ unifiesb (unify_stack UkCoerce s (we_dout cur)
                     (we_dout (decl_eff declared_empty))) = true
  | None => False
  end.
Proof. repeat split; vm_compute; reflexivity. Qed.

Example sealed_row_rejects_hidden_underflow :
  check_body declared_empty [trusted_img] = false.
Proof. vm_compute; reflexivity. Qed.

(* A callee that preserves the row leaves it open, and the definition stands. *)
Example sealed_row_admits_a_preserving_callee :
  check_body declared_empty [balanced_word] = true
  /\ check_body declared_empty [balanced_word; balanced_word] = true.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* The seal is about BINDING TO A SPINE, not about being untouched: a callee
   that merely renames the row (binding it to another row variable) preserves
   it, exactly as `ROW-OPEN?` decides. *)
Example preserving_callee_may_rename_the_row :
  match run_calls empty_subst
          (id_eff (we_din (decl_eff declared_empty)) (we_rin (decl_eff declared_empty)))
          [balanced_word] with
  | Some (s, _) => row_openb s 0 = true
  | None => False
  end.
Proof. vm_compute; reflexivity. Qed.

(* A declaration that DOES take the value is fine: the cell the callee consumes
   comes from the declared inputs, not from below them. *)
Definition declared_img : decl := decl_plain 0 9 [imgt] [imgt].

Example declared_input_is_not_underflow :
  check_body declared_img [trusted_img] = true.
Proof. vm_compute; reflexivity. Qed.

(* And an output mismatch is still caught after the seal passes. *)
Definition producer : word_eff := prim 1 [] [i64].

Example output_join_still_applies :
  check_body (decl_plain 0 9 [] []) [producer] = false
  /\ check_body (decl_plain 0 9 [] [i64]) [producer] = true.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* --- 6. The return row --------------------------------------------- *)

(* `>R` moves one cell from the data row to the return row.  A callee that
   declared no `|` clause leaves the caller's return row alone. *)

Definition to_r : word_eff :=
  MkWordEff (Eff (stack_of 1 [TVar 3]) (SRow 1) (SRow 2) (stack_of 2 [TVar 3])) true.

Definition from_r : word_eff :=
  MkWordEff (Eff (SRow 1) (stack_of 1 [TVar 3]) (stack_of 2 [TVar 3]) (SRow 2)) true.

Example return_row_round_trips :
  match compose_fresh empty_subst (id_eff (stack_of 0 [i64]) (SRow 5)) to_r with
  | Some (s, w) =>
      (* after `>R` the data row is the bare tail: the i64 is gone *)
      stack_eqb (zonk_row s (we_dout w)) (zonk_row s (SRow 0)) = true
      /\ match compose_fresh s w from_r with
         | Some (s', w') =>
             (* and `R>` brings the SAME i64 back, not a fresh variable *)
             stack_eqb (zonk_row s' (we_dout w')) (SPush i64 (zonk_row s' (SRow 0)))
         | None => false
         end = true
  | None => False
  end.
Proof. repeat split; vm_compute; reflexivity. Qed.

Example callee_without_a_return_clause_leaves_it_alone :
  match compose_fresh empty_subst (id_eff (SRow 0) (SRow 5)) balanced_word with
  | Some (s, w) => stack_eqb (resolve_row s (we_rout w)) (SRow 5) = true
  | None => False
  end.
Proof. vm_compute; reflexivity. Qed.

(* A definition may not leave a value stranded on the return row: the base
   return row is sealed exactly as the data row is. *)
Definition declared_with_return : decl := decl_with_return 0 1 [i64] [] [] [].

Example return_base_row_is_sealed_too :
  match run_calls empty_subst
          (id_eff (we_din (decl_eff declared_with_return))
                  (we_rin (decl_eff declared_with_return)))
          [from_r] with
  | Some (s, _) => seal_okb s declared_with_return = false
  | None => False
  end.
Proof. vm_compute; reflexivity. Qed.

(* --- 7. Failure is a value ------------------------------------------ *)

(* Everything above is decided by total functions.  Nothing here can raise,
   diverge, or be partial: unification returns `option subst` and the boundary
   returns `bool`. *)

Example failure_is_a_value :
  unify_stack UkExact empty_subst (stack_of 0 [i64]) (stack_of 0 [strt]) = None
  /\ check_body declared_empty [trusted_img] = false.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* A signature type is a term, not a string: nothing here can be built by
   spelling. *)
Fail Definition rows_are_not_types : ty := SRow 0.
