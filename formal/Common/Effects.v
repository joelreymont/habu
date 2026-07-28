(* Habu.Common.Effects — a model of the checked stack-effect language.

   `docs/effects.md` is the written specification.  `src/core/checker.f` is the
   implementation, and it is the ground truth wherever the two disagree.  Every
   definition below names the checker word it models, with a line reference, so
   a reader can check the model against the code rather than against prose.

   Scope of this file: the syntax of types, stacks, and four-row effects; the
   type-family registry and the layout width expansion a family value goes
   through on its way onto a row; executable unification of two stacks;
   executable sequential composition of two effects; and the predicate that
   decides whether a checked definition's implicit row survived its body.
   Laws, algebraic properties, and the soundness statement (acceptance implies
   the body cannot underflow or type-mismatch at runtime) are deliberately
   absent — they are later work.

   Deliberate omissions from the modelled fragment, each of which the checker
   also decides and each of which is a place a later soundness proof would not
   speak about:
     - T-ATOM rigid host identities (region / extent / generation),
     - VALUE-RECORD field cells and their coercion (`FIELD-PAIR?` /
       `FIELD-COERCE?`, checker.f:1596-1597),
     - the whole-bundle TRANSPORT ops (`XPORT-STEP?`, checker.f:7215) and the
       generated-accessor introduction window.  Each arms one checker register
       — `LAYOUT-XPORT` / `LAYOUT-INTRO`, checker.f:1412-1413 — and those two
       registers are the ONLY thing that opens `LAYOUT-BLOCK?`.  This file
       models the ORDINARY window, in which both are clear, because that is
       what every call site, every join and the definition boundary see,
     - `construct` (`CONM`) and field projection, which are row surgery over
       the same layout machinery.
   Type families of arity > 0 — layouts, sums, products, `uniform<T>` — ARE
   modelled here, together with layout width expansion and the hidden-field /
   logical-bundle machinery, because those decide ordinary programs: a
   two-cell `option<n>` cannot be dropped by a one-cell `drop`, and a
   polymorphic `( a -- a a )` cannot copy it.
   Control flow, throw edges, the quotation APPLICATION rules, and the
   linear-once conservation pass are modelled in `Habu.Common.Control`, which
   is why `TQuot` below carries the two control flags those rules read.
   Arity-0 nominal families (what `DEFTYPE` mints) are the degenerate case of
   the same constructor: no arguments, no layout, one cell.

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
(* The type-family registry.                                           *)
(*                                                                     *)
(* A `T-PARAM` term does not carry its own shape.  It carries a family  *)
(* ID, and every question about that family — how many arguments it     *)
(* takes, whether it occupies an ADT layout, how many stack cells it    *)
(* is worth — is answered by a registry that lives in                   *)
(* `src/core/type-family.f` and is reached from the checker through     *)
(* deferred hooks (`TFAM-ARITY-XT` .. `TFAM-INST-WIDTH-XT`,             *)
(* checker.f:461-467).  The registry is global ambient state during a   *)
(* check, so it is a parameter to everything below that inspects a      *)
(* family, and an id that is not registered answers the                 *)
(* registry-not-loaded defaults of `TFAM-QUERY-DEFAULTS`                *)
(* (checker.f:758-766): arity 0, not a layout, not a cell kind, one     *)
(* cell.  That is exactly the behaviour an arity-0 nominal wants, so an *)
(* empty registry reproduces the arity-0 fragment unchanged.            *)
(* ------------------------------------------------------------------ *)

(* `TK-CELL` .. `TK-EVIDENCE`, type-family.f:15-19. *)
Inductive tkind : Type :=
  | TkCell | TkProduct | TkSum | TkEnum | TkEvidence.

(* One node of a variant-payload or product-field SCHEMA, as much of it as the
   width function reads (`SCH-NODE-IWIDTH`, type-family.f:990-993).  A schema
   node is either the family's own parameter slot `i`, an application of some
   OTHER family, or anything else — and anything else is one cell. *)
Inductive sch : Type :=
  | SchParam : nat -> sch      (* SCHEMA-PARAM?: this family's slot i *)
  | SchApp : nat -> sch        (* SCHEMA-APP?:   another family, at its declared width *)
  | SchCell.                   (* everything else: one cell *)

Record famdef : Type := MkFamDef {
  fd_arity  : nat;                 (* TFAM-ARITY@,  type-family.f:303 *)
  fd_kind   : tkind;               (* TFAM-KIND@ *)
  (* TFAM-BOXED-OR-NICHE?, type-family.f:348: a layout policy that collapses
     the value to one cell whatever its kind.  type-family.f:344-347 records
     that no declaration form reaches it today — every declaration rejects at
     the policy clause — so it is modelled and never exercised. *)
  fd_boxed  : bool;
  fd_slots  : nat;                 (* TFAM-SLOTS@: declared payload/field slots *)
  fd_conlin : bool;                (* TFAM-CON-LIN: a schema holds a concrete linear *)
  fd_vars   : list (list sch);     (* sum/enum variants, each its payload schemas *)
  fd_flds   : list sch             (* product fields *)
}.

(* An arity-0, non-layout, one-cell family: what `DEFTYPE` mints
   (`VNOM:MINT` -> `CHECKER-DEFFAMILY`, lib/type/deftype.f:106-112). *)
Definition nominal_famdef : famdef := MkFamDef 0 TkCell false 0 false [] [].

Definition fenv : Type := list (nat * famdef).

Fixpoint lookup_fam (e : fenv) (f : nat) : option famdef :=
  match e with
  | [] => None
  | (g, d) :: rest => if Nat.eqb g f then Some d else lookup_fam rest f
  end.

(* The registry-not-loaded defaults, checker.f:758-766. *)
Definition fam_arity (e : fenv) (f : nat) : nat :=
  match lookup_fam e f with Some d => fd_arity d | None => 0 end.

(* `TFAM-LAYOUT?`, type-family.f:324: product, sum and enum occupy a layout;
   a cell kind and an evidence family do not. *)
Definition fam_layoutb (e : fenv) (f : nat) : bool :=
  match lookup_fam e f with
  | Some d => match fd_kind d with
              | TkProduct | TkSum | TkEnum => true
              | TkCell | TkEvidence => false
              end
  | None => false
  end.

(* `TFAM-CELL?`, type-family.f:319. *)
Definition fam_cellb (e : fenv) (f : nat) : bool :=
  match lookup_fam e f with
  | Some d => match fd_kind d with TkCell => true | _ => false end
  | None => false
  end.

(* `TFAM-CON-LIN`, the "this family's schemas hold a concrete linear" flag. *)
Definition fam_conlinb (e : fenv) (f : nat) : bool :=
  match lookup_fam e f with Some d => fd_conlin d | None => false end.

(* `TFAM-WIDTH@`, type-family.f:355-359 — the DECLARED width, which counts
   every parameter as one cell.  A sum or enum is its slots plus one tag cell,
   a product is its slots with no tag, everything else is one cell.  This is
   what a NESTED concrete family contributes to the instantiated width below,
   and it is the boot fallback the checker uses before the registry arms
   (checker.f:2585).  Measured through `REFLECT:WIDTH` (test/checker-assert.f):
   `option` is arity 1, TK-SUM, slots 1, width 2; `result` is arity 2, slots 1,
   width 2. *)
Definition fam_width (e : fenv) (f : nat) : nat :=
  match lookup_fam e f with
  | None => 1
  | Some d =>
      if fd_boxed d then 1
      else match fd_kind d with
           | TkSum | TkEnum => S (fd_slots d)
           | TkProduct => fd_slots d
           | TkCell | TkEvidence => 1
           end
  end.

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
  (* T-PARAM: an application of a type family.  `MK-PARAM` (checker.f:657-677)
     stores three things and this constructor carries all three, in the same
     order: the resolved family id (`PARAM>FAM`, the IDENTITY — not the folded
     spelling, checker.f:1229), the hidden physical-field slot PLUS ONE
     (`PARAM>HID`, checker.f:643, where 0 means "logical"), and the argument
     run (`PARAM>ARG`, index 0 first).

     docs/effects.md:18 puts `DEFTYPE` and `DEFLINEAR` names in one grammar
     class, but they are two different things in the code.  `DEFLINEAR` adds a
     global row to the concrete type table (`CT-ADD-LINEAR`, checker.f:2766),
     so it is a `TCon` above.  `DEFTYPE` mints a package-scoped arity-0 TYPE
     FAMILY (`VNOM:MINT` -> `CHECKER-DEFFAMILY`, lib/type/deftype.f:106-112),
     which is `fam0` below; lib/type/deftype.f:22 records that the table route
     was retired.  Measured: a `DEFTYPE SERIAL` mismatch reports
     `expected: n actual: serial<>`, the family-application rendering, not
     the bare name docs/effects.md:70 promises.

     `tys` is an ordinary list of terms, written as its own inductive because
     the mutual block needs it: Rocq's guard checker will not let a fixpoint
     over `ty` recurse through `list ty`, which is not in the block.  Nothing
     but that is different — `args_of` / `args_list` convert both ways and
     the smart constructors below take ordinary lists, so no example ever
     writes `TCons`. *)
  | TFam : nat -> nat -> tys -> ty
  | TPtr : ty -> ty                   (* T-PTR:   `ptr t` *)
  (* T-QUOT: `[ in -- out ]`.  A quotation term is NOT just four rows.
     `MK-QUOT` (checker.f:293-302) allocates four row slots AND four control
     slots, and `QX!` (checker.f:310-314) fills them at `;]`:
       `Q>XHAS`  (checker.f:306) — the body has a throw edge;
       `Q>XDEAD` (checker.f:307) — the body has NO normal return;
       `Q>XDOUT` / `Q>XROUT` (checker.f:308-309) — the rows at the first throw.
     The two BOOLEANS decide programs: `RSEXEC` (checker.f:2015-2021) raises the
     caller's throw edge on `Q>XHAS` and KILLS the caller's path on `Q>XDEAD`
     INSTEAD of installing the quotation's output rows, and `RSCATCH`
     (checker.f:2051-2052) uses both to decide whether a throw code is pushed
     at all.  Carrying only the rows makes a model of `execute` unsound in the
     accepting direction, so they live here.  The two ROWS decide nothing: they
     are read only by the image serialisers (checker.f:2433-2434, 4197-4198),
     so they are deliberately not carried.
     Flag order: xhas, then xdead. *)
  | TQuot : bool -> bool -> eff -> ty
with tys : Type :=
  | TNil : tys
  | TCons : ty -> tys -> tys
with eff : Type :=
  | Eff : stack -> stack -> stack -> stack -> eff   (* Din Dout Rin Rout *)
with stack : Type :=
  | SRow : rowvar -> stack
  | SPush : ty -> stack -> stack.

Fixpoint args_of (l : list ty) : tys :=
  match l with [] => TNil | u :: rest => TCons u (args_of rest) end.

Fixpoint args_list (l : tys) : list ty :=
  match l with TNil => [] | TCons u rest => u :: args_list rest end.

(* The three spellings of a family application a reader will meet.
   `fam0` is an arity-0 nominal, `fam_app` a logical family value, and
   `fam_hid f slot args` the hidden physical field at slot `slot` of one
   (`MK-HIDDEN`, checker.f:1355-1367, which stamps `slot + 1`). *)
Definition fam0 (f : nat) : ty := TFam f 0 TNil.
Definition fam_app (f : nat) (args : list ty) : ty := TFam f 0 (args_of args).
Definition fam_hid (f slot : nat) (args : list ty) : ty := TFam f (S slot) (args_of args).

(* The overwhelmingly common quotation: a body with a normal return and no
   throw edge.  Every quotation `MK-QUOT` builds starts this way (checker.f:297-
   298 zero both flag cells) and only `QX!` at `;]` can set them. *)
Definition quot (e : eff) : ty := TQuot false false e.

Definition q_xhas (t : ty) : bool :=
  match t with TQuot h _ _ => h | _ => false end.

Definition q_xdead (t : ty) : bool :=
  match t with TQuot _ d _ => d | _ => false end.

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
  | TFam _ _ args => S (args_size args)
  | TPtr u => S (ty_size u)
  | TQuot _ _ e => S (eff_size e)
  end
with args_size (l : tys) : nat :=
  match l with TNil => 0 | TCons u rest => S (ty_size u + args_size rest) end
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
  (* Structural equality of the whole T-PARAM record: family, hidden slot and
     the argument run.  `U-TYPE`'s fast path (`2dup =`) is arena-pointer
     identity, which likewise separates two params that differ anywhere. *)
  | TFam f1 h1 a1, TFam f2 h2 a2 =>
      Nat.eqb f1 f2 && Nat.eqb h1 h2 && args_eqb a1 a2
  | TPtr x, TPtr y => ty_eqb x y
  (* Structural equality of TERMS, so the control flags are part of it: two
     quotations with the same rows and different flags are different terms.
     `U-TYPE`'s fast path is `2dup =`, pointer identity of arena entries
     (checker.f:1586), which likewise separates them. *)
  | TQuot h1 d1 x, TQuot h2 d2 y =>
      Bool.eqb h1 h2 && Bool.eqb d1 d2 && eff_eqb x y
  | _, _ => false
  end
with args_eqb (a b : tys) : bool :=
  match a, b with
  | TNil, TNil => true
  | TCons u r, TCons v q => ty_eqb u v && args_eqb r q
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
      | TFam n h args => TFam n h (args_of (map (zonk_ty_fuel f s) (args_list args)))
      | TPtr u => TPtr (zonk_ty_fuel f s u)
      | TQuot h dd (Eff a b c d) =>
          TQuot h dd (Eff (zonk_row_fuel f s a) (zonk_row_fuel f s b)
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
      (* `TY-OCC?`'s T-PARAM arm, checker.f:1278-1285: a type variable can
         hide inside a family ARGUMENT, so the occurs check descends there. *)
      | TFam _ _ args => existsb (fun u => ty_occ_fuel f s v u) (args_list args)
      | TPtr u => ty_occ_fuel f s v u
      | TQuot _ _ (Eff a b c d) =>
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
      | TQuot _ _ (Eff a b c d) =>
          row_occ_fuel f s r a || row_occ_fuel f s r b
          || row_occ_fuel f s r c || row_occ_fuel f s r d
      (* DIVERGENCE BETWEEN THE TWO OCCURS CHECKS, and it is in the code.
         `TY-OCC?` descends into family arguments (checker.f:1278) but
         `ROW-OCC?` (checker.f:963-977) walks only the spine and the pointer /
         quotation chain — it never enters a `T-PARAM`.  A family argument CAN
         hold rows (`SIG-PARSE-QUOT`, checker.f:3027, parses a quotation as an
         argument), so a row variable reachable only through an argument
         escapes the row occurs check.  This arm follows the code. *)
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
(* Layouts: width, hidden fields, and the signature expansion.         *)
(*                                                                     *)
(* This is the machinery that makes a family value worth more than one *)
(* stack cell, and it is where a reader is most likely to expect a     *)
(* record and find a ROW instead.  There is no bundle VALUE anywhere   *)
(* in the checker.  A layout value that reaches a row is TORN INTO     *)
(* CELLS at the signature seam (`PUSH-LOGICAL`, checker.f:2964-2971)   *)
(* and put back together by nothing: the W cells sit side by side on   *)
(* the row, each a `T-PARAM` of the same family carrying its own slot  *)
(* number, and the ONLY thing that keeps them together is that none of *)
(* them will unify with anything except the same family at the same    *)
(* slot.                                                               *)
(*                                                                     *)
(* Measured, and this pair is the whole rule:                          *)
(*   `: W ( option<n> -- ) drop ;`         -> exit 0                   *)
(*   `: W ( option<n> -- n ) drop ;`       -> exit 70,                 *)
(*       `at 'drop' expected: n actual:`                               *)
(* One `drop` took BOTH cells, because `option<n>` is two cells and    *)
(* `drop` is a whole-bundle transport; there was no `n` left under it. *)
(* ------------------------------------------------------------------ *)

(* Physical slots run 0 .. W-1 with the TAG at W-1 (docs §5, and
   `LAYOUT-PUSH-FIELDS` checker.f:1383-1390 pushes them in that order, so slot
   0 is deepest and the tag is on top). *)

(* `HIDDEN-PARAM?` / `HIDDEN-SLOT@`, checker.f:1345-1350.  Both take a term
   that is already resolved. *)
Definition hidden_paramb (t : ty) : bool :=
  match t with TFam _ h _ => Nat.ltb 0 h | _ => false end.

Definition hidden_slot (t : ty) : nat :=
  match t with TFam _ h _ => pred h | _ => 0 end.

Definition param_famb (t : ty) : option nat :=
  match t with TFam f _ _ => Some f | _ => None end.

Definition param_args (t : ty) : list ty :=
  match t with TFam _ _ args => args_list args | _ => [] end.

(* `LAYOUT-PARAM?`, checker.f:1297-1300.  Note what it does NOT ask: the
   hidden slot.  A hidden field of a layout family is itself a layout param,
   which is why a hidden cell is refused a variable by the same guard the
   logical cell is. *)
Definition layout_paramb (e : fenv) (s : subst) (t : ty) : bool :=
  match resolve_ty s t with
  | TFam f _ _ => fam_layoutb e f
  | _ => false
  end.

(* `NOM-SCALAR?`, checker.f:1310-1316: a LOGICAL arity-0 cell-kinded family.
   One raw cell whose entire meaning is its family identity. *)
Definition nom_scalarb (e : fenv) (s : subst) (t : ty) : bool :=
  match resolve_ty s t with
  | TFam f h _ =>
      Nat.eqb h 0 && fam_cellb e f && Nat.eqb (fam_arity e f) 0
  | _ => false
  end.

(* `LAYOUT-ARGS-OPEN?`, checker.f:1505-1509: is any argument still an
   unresolved variable?  This is the WIDTH question — an open argument means
   the width is not yet known, so the value stays one conservative logical
   cell.  Measured, and this is the sharpest statement of the rule because
   nothing differs but the argument:
     `: W ( option<n> -- ) drop ;`  -> exit 0
     `: W ( option<a> -- ) drop ;`  -> exit 70,
         `at 'drop' expected: a actual: option<b>` *)
Definition layout_args_openb (s : subst) (t : ty) : bool :=
  existsb (fun u => match resolve_ty s u with TVar _ => true | _ => false end)
          (param_args t).

(* `T-WIDTH` (checker.f:1329-1335) and, under it, `TFAM-INST-WIDTH@`
   (type-family.f:1020-1025).  Every non-layout term is one cell.  A layout
   term asks the INSTANTIATED width: the registry walks the family's variant
   or field schemas and substitutes each parameter slot by the width of the
   matching ARGUMENT, so a layout argument widens the payload.  For an
   all-arguments-one-cell instantiation it equals the declared `fam_width`.

   Measured through `MATCH`, which refines a branch with the instantiated
   payload, so the payload width is directly observable:
     `option<option<n>>`'s `some` payload is an `option<n>` — a two-cell
     bundle.  Handing that branch to `( option<n> -- n )` certifies (exit 0);
     handing it to `( n -- )` is refused with
     `at 'DROP-N' expected: n actual: option<n>` (exit 70).

   The walk is fuelled because `resolve_ty` breaks structural descent.  An
   exhausted budget answers ONE — the checker's own answer for every
   non-layout term, and the same answer an open-argument layout gets — so it
   un-expands a bundle rather than inventing cells.  That cannot make a
   program certify: a one-cell logical bundle still refuses every variable
   (`layout_blockb` below), and against a row that DID expand it mismatches. *)
Fixpoint t_width_fuel (fuel : nat) (e : fenv) (s : subst) (t : ty) : nat :=
  match fuel with
  | 0 => 1
  | S f =>
      match resolve_ty s t with
      | TFam g _ args =>
          match lookup_fam e g with
          | None => 1
          | Some d =>
              if negb (fam_layoutb e g) then 1
              else if fd_boxed d then 1
              else
                let node w :=
                  match w with
                  (* `TFAM-SCH-ARITY` (type-family.f:1503) rejects a schema
                     whose parameter index is outside the family's arity at
                     DECLARATION time, so the default below is unreachable for
                     any registry that was accepted. *)
                  | SchParam i => t_width_fuel f e s (nth i (args_list args) (TCon CN))
                  | SchApp g' => fam_width e g'
                  | SchCell => 1
                  end in
                match fd_kind d with
                | TkProduct => fold_left (fun acc w => acc + node w) (fd_flds d) 0
                | TkSum | TkEnum =>
                    S (fold_left
                         (fun acc v => Nat.max acc
                            (fold_left (fun a w => a + node w) v 0))
                         (fd_vars d) 0)
                | TkCell | TkEvidence => 1
                end
          end
      | _ => 1
      end
  end.

Definition t_width (e : fenv) (s : subst) (t : ty) : nat :=
  t_width_fuel (walk_budget s (ty_size t)) e s t.

(* `MK-HIDDEN`, checker.f:1355-1367: the hidden field for slot `slot` of a
   RESOLVED logical layout term, reusing the source's family id and argument
   run and stamping `slot + 1`. *)
Definition mk_hidden (src : ty) (slot : nat) : ty :=
  match src with TFam f _ args => TFam f (S slot) args | _ => src end.

(* `LAYOUT-PUSH-FIELDS`, checker.f:1383-1390: push the W hidden fields onto a
   row in physical order — slot 0 deepest, the tag on top. *)
Definition layout_push_fields (e : fenv) (s : subst) (t : ty) (row : stack) : stack :=
  let src := resolve_ty s t in
  fold_left (fun r slot => SPush (mk_hidden src slot) r)
            (seq 0 (t_width e s src)) row.

(* `PUSH-LOGICAL`, checker.f:2964-2971 — THE seam.  Every ordinary type and
   every cell family pushes one cell.  A LOGICAL layout family whose arguments
   are all resolved expands to its W hidden fields.  A possibly-open one stays
   one logical cell, and note that the fallback pushes the term the caller
   handed over, not the resolved one. *)
Definition push_logical (e : fenv) (s : subst) (t : ty) (row : stack) : stack :=
  let r := resolve_ty s t in
  if layout_paramb e s r && negb (hidden_paramb r) && negb (layout_args_openb s r)
  then layout_push_fields e s r row
  else SPush t row.

(* `PSTACK`'s type fold, checker.f:3011-3016.  Every parsed signature type
   reaches the row THROUGH `PUSH-LOGICAL`, so a declared row holds PHYSICAL
   cells, not the types as they were written: `( option<n> -- )` declares a
   TWO-cell input.  `stack_of` above is the raw cons — it is what an example
   uses to write a row of cells directly — and the two agree on every type
   that is not a closed layout.  At signature-parse time nothing is bound yet,
   which is why the expansion runs under the empty substitution. *)
Definition sig_stack (e : fenv) (r : rowvar) (ts : list ty) : stack :=
  fold_left (fun s t => push_logical e empty_subst t s) ts (SRow r).

(* `LAYOUT-ARG-LIN-N` / `LAYOUT-LINEAR-COUNT`, checker.f:1482-1503.  A layout
   value's linear units are the linear cons among its arguments plus, for a
   NESTED family argument, that subtree's own count, plus one if the family's
   own schemas hold a concrete linear.  `Habu.Common.Control`'s `LIN-TYPE-COUNT`
   samples this ONCE per logical value — at the tag cell of an expanded
   bundle — so a bundle is never double counted. *)
Fixpoint layout_arg_lin_fuel (fuel : nat) (e : fenv) (s : subst) (t : ty) : nat :=
  match fuel with
  | 0 => 0
  | S f =>
      match resolve_ty s t with
      | TFam g _ args =>
          fold_left (fun acc u => acc + layout_arg_lin_fuel f e s u) (args_list args) 0
          + (if fam_conlinb e g then 1 else 0)
      | TCon c => if linear_conb c then 1 else 0
      | _ => 0
      end
  end.

Definition layout_linear_count (e : fenv) (s : subst) (t : ty) : nat :=
  match resolve_ty s t with
  | TFam g _ args =>
      fold_left
        (fun acc u => acc + layout_arg_lin_fuel (walk_budget s (ty_size u)) e s u)
        (args_list args) 0
      + (if fam_conlinb e g then 1 else 0)
  | _ => 0
  end.

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
      (* `RAW-OK?`, checker.f:1576: a T-PARAM is refused whatever its arity,
         layout or hidden slot — the arm tests the TAG and nothing else. *)
      | TFam _ _ _ => None
      | TCon c => if linear_conb c then None else Some s
      | TPtr u => raw_ok_fuel f s u
      | TQuot _ _ _ => Some s    (* the engine legitimately raw-stores xts *)
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

(* `NOMPTR-BLOCK?`, checker.f:1539-1543.  Inside a POINTEE — and only there —
   a type variable may not absorb a nominal-scalar family: a raw pointer from
   `variable` / `create` would acquire the family identity by ordinary
   unification, bypassing the one introduction form.  In value position a
   nominal scalar is an ordinary one-cell value.  The `LAYOUT-INTRO` window
   that relaxes this is the generated-accessor window, which this file does
   not model. *)
Definition nomptr_blockb (e : fenv) (s : subst) (strict : bool) (a b : ty) : bool :=
  if negb strict then false
  else (nom_scalarb e s a && match resolve_ty s b with TVar _ => true | _ => false end)
       || (nom_scalarb e s b && match resolve_ty s a with TVar _ => true | _ => false end).

(* `LAYOUT-BLOCK?`, checker.f:1545-1554, at the ORDINARY window.  With
   `LAYOUT-XPORT` clear, `LAYOUT-XPORT-ALLOW?` is false; with `LAYOUT-INTRO`
   clear, the middle clause is skipped.  What is left is stark: if either side
   is a layout param the pairing is REFUSED, and if neither is, only the
   pointee rule above applies.  Since the two T-PARAM terms were already dealt
   with by the arm above, "either side is a layout param" here means a layout
   cell meeting a variable, a con, a pointer or a quotation.

   That is why a polymorphic word cannot touch a bundle.  Measured over
   `: DUPA ( a -- a a ) dup ;`, which certifies on its own:
     `: W ( option<n> -- option<n> option<n> ) DUPA ;` -> exit 70,
         `at 'DUPA' expected: a actual: option<n>`
   The cell `DUPA` met was the bundle's TAG — a hidden field — and a hidden
   field never binds a variable. *)
Definition layout_blockb (e : fenv) (s : subst) (strict : bool) (a b : ty) : bool :=
  if negb (layout_paramb e s a || layout_paramb e s b)
  then nomptr_blockb e s strict a b
  else true.

(* `PARAM-HID-OK?`, checker.f:1395-1399: neither hidden is an ordinary logical
   pair; exactly one hidden is refused, because a hidden field must never
   unify with the whole logical value; both hidden must be at the SAME slot. *)
Definition param_hid_okb (a b : ty) : bool :=
  match a, b with
  | TFam _ h1 _, TFam _ h2 _ =>
      if Nat.eqb (h1 + h2) 0 then true
      else if negb (Nat.ltb 0 h1 && Nat.ltb 0 h2) then false
      else Nat.eqb h1 h2
  | _, _ => false
  end.

(* `PARAM-PAIR-ARGS`, checker.f:1231-1238: equal argument count, equal family
   id (`PARAM-FAM-OK?`, checker.f:1229 — identity is the RESOLVED family id,
   not the folded spelling), then the arguments pairwise.  The checker PUSHES
   argument pairs 0, 1, .. onto its worklist and pops last-first, so the
   generated list is reversed here exactly as the quotation arm's is.

   Measured: `result<n,idx>` against `result<idx,n>` is refused with
   `at 'W' expected: result<idx,n> actual: result<n,idx>`, and `option<n>`
   against `result<n,n>` with `expected: result<n,n> actual: option<n>`. *)
Definition param_pair_args (strict : bool) (a b : ty) : option (list upair) :=
  match a, b with
  | TFam f1 _ a1, TFam f2 _ a2 =>
      let l1 := args_list a1 in
      let l2 := args_list a2 in
      if negb (Nat.eqb (length l1) (length l2)) then None
      else if negb (Nat.eqb f1 f2) then None
      else Some (rev (map (fun p => UTy strict (fst p) (snd p)) (combine l1 l2)))
  | _, _ => None
  end.

(* `U-TYPE`, checker.f:1584-1610.  Arm order follows the checker's, minus the
   atom and VALUE-RECORD field arms this fragment does not model — and the
   order MATTERS here: the T-PARAM/T-PARAM arm and `LAYOUT-BLOCK?` both come
   BEFORE the variable arms, which is what stops a bundle being absorbed by a
   type variable. *)
Definition u_ty (e : fenv) (k : ukind) (s : subst) (strict : bool) (a b : ty)
  : option (subst * list upair) :=
  let ra := resolve_ty s a in
  let rb := resolve_ty s b in
  if ty_eqb ra rb then Some (s, [])
  else
    match ra, rb with
    (* `U-TYPE`'s quotation arm pairs the four ROWS and nothing else
       (checker.f:1587-1591): the control flags are not unified, so a quotation
       variable bound to a throwing quotation keeps THAT term's flags. *)
    | TQuot _ _ (Eff d1 o1 r1 q1), TQuot _ _ (Eff d2 o2 r2 q2) =>
        Some (s, [URow strict q1 q2; URow strict r1 r2;
                  URow strict o1 o2; URow strict d1 d2])
    (* PAIR-STRICT: a pointee never widens, at any nesting depth. *)
    | TPtr u, TPtr w => Some (s, [UTy true u w])
    | TFam _ _ _, TFam _ _ _ =>
        if negb (param_hid_okb ra rb) then None
        else match param_pair_args strict ra rb with
             | None => None
             | Some gen => Some (s, gen)
             end
    | _, _ =>
        if layout_blockb e s strict ra rb then None
        else
          match ra, rb with
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
          | _, _ => None
          end
    end.

(* `LOGHID-AT?` / `LOGHID-EXPAND`, checker.f:1623-1636 — where a logical cell
   MEETS a width expansion, and the reason a signature may still be written
   with an open argument.  A stored effect keeps a parametric layout value as
   ONE logical cell whenever an argument is unresolved.  At a call site that
   cell can meet the W-cell hidden run of the SAME family, whose arguments are
   by the `PUSH-LOGICAL` invariant fully resolved.  Unifying the two proves
   the logical side's arguments equal to the hidden side's, so the logical
   cell is expanded in place and the two rows re-pair cell for cell.

   The test is that the HIDDEN side is at its TAG — slot W-1, the top of a
   whole group — which is exactly what "the rows are aligned at a bundle
   boundary" means when the only thing on the row is cells.

   Measured, and it is the direct witness that expansion happens:
     `: ID-OPT ( option<a> -- option<a> ) ;`
     `: W ( option<n> -- option<n> ) ID-OPT ;`      -> exit 0
   The callee declares ONE cell, the caller holds TWO, and the load
   certifies.  The mirror `( option<n> -- option<n> )` called from an
   `option<a>` caller certifies too. *)
Definition loghid_atb (e : fenv) (s : subst) (rh rl : stack) : bool :=
  match rh, rl with
  | SPush th0 _, SPush tl0 _ =>
      let th := resolve_ty s th0 in
      let tl := resolve_ty s tl0 in
      hidden_paramb th && negb (hidden_paramb tl) && layout_paramb e s tl
      && (match param_famb th, param_famb tl with
          | Some f, Some g => Nat.eqb f g
          | _, _ => false
          end)
      && Nat.eqb (hidden_slot th) (pred (t_width e s th))
  | _, _ => false
  end.

Definition loghid_expand (e : fenv) (s : subst) (rl : stack) : stack :=
  match rl with
  | SPush t rest => layout_push_fields e s (resolve_ty s t) rest
  | SRow _ => rl
  end.

(* `U-ROW`, checker.f:1654-1659.  Apart from the two LOGHID arms a row is only
   ever `S-ROW` or `S-PUSH`, so the only way this fails is the occurs check.

   One faithfulness note.  The checker's second LOGHID orientation is
   `2dup swap LOGHID-AT? IF ... swap LOGHID-EXPAND` (checker.f:1658): the
   `swap` before `LOGHID-EXPAND` re-pairs the rows the other way round, so the
   got/want orientation is FLIPPED for that arm.  Under `UK-INPUT` that
   decides which side may widen.  The arm below reproduces it. *)
Definition u_row (e : fenv) (s : subst) (strict : bool) (a b : stack)
  : option (subst * list upair) :=
  let ra := resolve_row s a in
  let rb := resolve_row s b in
  if stack_eqb ra rb then Some (s, [])
  else
    match ra, rb with
    | SRow r, _ => if row_occurs s r rb then None else Some (bind_row s r rb, [])
    | _, SRow r => if row_occurs s r ra then None else Some (bind_row s r ra, [])
    | SPush t1 rest1, SPush t2 rest2 =>
        if loghid_atb e s ra rb
        then Some (s, [URow strict ra (loghid_expand e s rb)])
        else if loghid_atb e s rb ra
        then Some (s, [URow strict rb (loghid_expand e s ra)])
        else Some (s, [UTy strict t1 t2; URow strict rest1 rest2])
    end.

Fixpoint unify_loop (fuel : nat) (e : fenv) (k : ukind) (s : subst)
                    (wl : list upair) : option subst :=
  match fuel with
  | 0 => None   (* fail closed *)
  | S f =>
      match wl with
      | [] => Some s
      | URow st a b :: rest =>
          match u_row e s st a b with
          | None => None
          | Some (s', gen) => unify_loop f e k s' (gen ++ rest)
          end
      | UTy st a b :: rest =>
          match u_ty e k s st a b with
          | None => None
          | Some (s', gen) => unify_loop f e k s' (gen ++ rest)
          end
      end
  end.

(* A LOGHID expansion re-pairs a row with W-1 EXTRA cells, so the worklist can
   grow beyond the two terms it started from.  The budget therefore allows for
   the widest bundle any registered family can reach. *)
Definition fenv_width (e : fenv) : nat :=
  S (fold_left (fun n p => Nat.max n (S (fd_slots (snd p)))) e 0).

Definition unify_budget (e : fenv) (s : subst) (n : nat) : nat :=
  walk_budget s n * S n * fenv_width e.

(* `UNIFY` opens at `CUR-STRICT = 0` (checker.f:1663). *)
Definition unify_stack (e : fenv) (k : ukind) (s : subst) (got want : stack)
  : option subst :=
  unify_loop (unify_budget e s (stack_size got + stack_size want)) e k s
             [URow false got want].

Definition unify_ty (e : fenv) (k : ukind) (s : subst) (got want : ty)
  : option subst :=
  unify_loop (unify_budget e s (ty_size got + ty_size want)) e k s
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
  | TFam f h args => TFam f h (shift_args d args)
  | TPtr u => TPtr (shift_ty d u)
  | TQuot h dd e => TQuot h dd (shift_eff d e)
  end
with shift_args (d : nat) (l : tys) : tys :=
  match l with
  | TNil => TNil
  | TCons u rest => TCons (shift_ty d u) (shift_args d rest)
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
  | TFam _ _ args => max_args args
  | TPtr u => max_ty u
  | TQuot _ _ e => max_eff e
  end
with max_args (l : tys) : nat :=
  match l with TNil => 0 | TCons u rest => Nat.max (max_ty u) (max_args rest) end
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

Definition compose (e : fenv) (s : subst) (e1 e2 : word_eff)
  : option (subst * word_eff) :=
  match unify_stack e UkInput s (we_dout e1) (we_din e2) with
  | None => None
  | Some s1 =>
      if we_hasr e2
      then match unify_stack e UkInput s1 (we_rout e1) (we_rin e2) with
           | None => None
           | Some s2 =>
               Some (s2, MkWordEff (Eff (we_din e1) (we_dout e2)
                                        (we_rin e1) (we_rout e2)) true)
           end
      else Some (s1, MkWordEff (Eff (we_din e1) (we_dout e2)
                                    (we_rin e1) (we_rout e1)) (we_hasr e1))
  end.

Definition compose_fresh (e : fenv) (s : subst) (e1 e2 : word_eff)
  : option (subst * word_eff) :=
  let above := Nat.max (next_sub s) (Nat.max (next_eff (we_eff e1)) (next_eff (we_eff e2))) in
  compose e s e1 (instantiate above e2).

(* The running state a body starts from: the declared input rows, unchanged. *)
Definition id_eff (din rin : stack) : word_eff :=
  MkWordEff (Eff din din rin rin) true.

Fixpoint run_calls (e : fenv) (s : subst) (cur : word_eff) (calls : list word_eff)
  : option (subst * word_eff) :=
  match calls with
  | [] => Some (s, cur)
  | c :: cs =>
      match compose_fresh e s cur c with
      | None => None
      | Some (s', cur') => run_calls e s' cur' cs
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
Definition decl_plain (e : fenv) (dbase rbrow : rowvar) (din dout : list ty) : decl :=
  MkDecl dbase None rbrow
    (MkWordEff (Eff (sig_stack e dbase din) (sig_stack e dbase dout)
                    (SRow rbrow) (SRow rbrow)) false).

Definition decl_with_return (e : fenv) (dbase rbase : rowvar)
                            (din dout rin rout : list ty) : decl :=
  MkDecl dbase (Some rbase) rbase
    (MkWordEff (Eff (sig_stack e dbase din) (sig_stack e dbase dout)
                    (sig_stack e rbase rin) (sig_stack e rbase rout)) true).

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
Definition check_body (e : fenv) (d : decl) (calls : list word_eff) : bool :=
  match run_calls e empty_subst
                  (id_eff (we_din (decl_eff d)) (we_rin (decl_eff d))) calls with
  | None => false
  | Some (s, cur) =>
      if negb (seal_okb s d) then false
      else if negb (return_balancedb s d cur) then false
      else match unify_stack e UkCoerce s (we_dout cur) (we_dout (decl_eff d)) with
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
Definition prim (e : fenv) (r : rowvar) (din dout : list ty) : word_eff :=
  MkWordEff (Eff (sig_stack e r din) (sig_stack e r dout) (SRow r) (SRow r)) false.

(* --- 1. Composition of two concrete effects ------------------------ *)

(* `DUP` at i64, then `+`.  Composing `( R i64 -- R i64 i64 )` with
   `( S i64 i64 -- S i64 )` must unify S with R and leave `( R i64 -- R i64 )`. *)

Definition dup_i64 : word_eff := prim [] 1 [i64] [i64; i64].
Definition add_i64 : word_eff := prim [] 1 [i64; i64] [i64].

(* Written over the SAME row letter, the two effects meet without binding
   anything, and the composite is exactly `( R i64 -- R i64 )`. *)
Example compose_two_concrete_effects :
  compose [] empty_subst dup_i64 add_i64 = Some (empty_subst, prim [] 1 [i64] [i64]).
Proof. vm_compute; reflexivity. Qed.

(* A real call site instantiates the callee first, so the two effects start in
   disjoint variable spaces and unification joins their rows.  The composite
   still has one shared open tail under a single i64. *)
Example compose_fresh_instantiates_the_callee :
  match compose_fresh [] empty_subst dup_i64 add_i64 with
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
  compose_fresh [] empty_subst
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

Definition push_i64 : word_eff := prim [] 1 [] [i64].
Definition lt_i64 : word_eff := prim [] 1 [i64; i64] [boolt].

Definition literal_then_compare : option (subst * word_eff) :=
  compose_fresh [] empty_subst push_i64 lt_i64.

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
  match unify_stack [] UkExact empty_subst
          (stack_of 0 [i64; i64]) (stack_of 1 [TVar 2; TVar 2]) with
  | Some s => ty_eqb (resolve_ty s (TVar 2)) i64 = true
  | None => False
  end.
Proof. vm_compute; reflexivity. Qed.

(* The same letter twice is a real constraint: `R a a` does not accept
   `R i64 bool`. *)
Example same_letter_means_same_type :
  unify_stack [] UkExact empty_subst
    (stack_of 0 [i64; boolt]) (stack_of 1 [TVar 2; TVar 2]) = None.
Proof. vm_compute; reflexivity. Qed.

(* Two DIFFERENT letters accept the same two types — the constraint is
   sharing, not distinctness. *)
Example different_letters_are_unconstrained :
  match unify_stack [] UkExact empty_subst
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
  unify_stack [] UkExact empty_subst (stack_of 0 [i64]) (stack_of 0 [i64; i64]) = None.
Proof. vm_compute; reflexivity. Qed.

(* Against DIFFERENT row variables the same shapes unify — that is row
   polymorphism, and it is why the arity check above needs the shared row. *)
Example different_rows_absorb_arity :
  match unify_stack [] UkExact empty_subst
          (stack_of 0 [i64]) (stack_of 1 [i64; i64]) with
  | Some s => row_openb s 0 = false
  | None => False
  end.
Proof. vm_compute; reflexivity. Qed.

(* Conflicting concrete types. *)
Example conflicting_cons_reject :
  unify_stack [] UkInput empty_subst (stack_of 0 [i64]) (stack_of 0 [strt]) = None
  /\ unify_stack [] UkInput empty_subst (stack_of 0 [boolt]) (stack_of 0 [i64]) = None.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* Nominal roles are fail-closed: distinct from each other and from `n`, in
   both directions, under every unification kind. *)
Example roles_never_widen :
  unify_stack [] UkInput empty_subst (stack_of 0 [idxt]) (stack_of 0 [lent]) = None
  /\ unify_stack [] UkInput empty_subst (stack_of 0 [idxt]) (stack_of 0 [nt]) = None
  /\ unify_stack [] UkInput empty_subst (stack_of 0 [nt]) (stack_of 0 [idxt]) = None
  /\ unify_stack [] UkCoerce empty_subst (stack_of 0 [idxt]) (stack_of 0 [nt]) = None.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* Integer widening is a property of the unification KIND, not of the types:
   `u8` reaches an input slot declared `cell`, but an exact join refuses. *)
Example widening_is_input_only :
  unifiesb (unify_stack [] UkInput empty_subst (stack_of 0 [u8]) (stack_of 0 [cellt])) = true
  /\ unifiesb (unify_stack [] UkExact empty_subst (stack_of 0 [u8]) (stack_of 0 [cellt])) = false
  /\ unifiesb (unify_stack [] UkInput empty_subst (stack_of 0 [cellt]) (stack_of 0 [u8])) = false
  (* and `n` still subsumes any integer family in both directions *)
  /\ unifiesb (unify_stack [] UkExact empty_subst (stack_of 0 [nt]) (stack_of 0 [u8])) = true
  /\ unifiesb (unify_stack [] UkExact empty_subst (stack_of 0 [u8]) (stack_of 0 [nt])) = true.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* `addr` is in the integer class but carries its own sign (`CS-ADDR`,
   checker.f:1007), which is neither generic nor signed nor unsigned.  So it
   takes an `n` but refuses a narrower concrete integer, even at an input
   position.  Nothing in docs/effects.md says so; only `INT-WIDENS?` does. *)
Example addr_takes_n_but_not_a_narrower_int :
  unifiesb (unify_stack [] UkInput empty_subst (stack_of 0 [nt]) (stack_of 0 [addrt])) = true
  /\ unifiesb (unify_stack [] UkInput empty_subst (stack_of 0 [u8]) (stack_of 0 [addrt])) = false
  /\ unifiesb (unify_stack [] UkInput empty_subst (stack_of 0 [u32]) (stack_of 0 [addrt])) = false.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* A pointee is invariant at any depth: the strictness set on entering a `ptr`
   is inherited, so the widening above is unavailable inside one. *)
Example pointee_is_invariant :
  unify_stack [] UkInput empty_subst
    (stack_of 0 [TPtr u8]) (stack_of 0 [TPtr cellt]) = None
  /\ unify_stack [] UkInput empty_subst
       (stack_of 0 [TPtr (TPtr u8)]) (stack_of 0 [TPtr (TPtr cellt)]) = None.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* The row occurs check looks INSIDE a quotation's effect rows, so a row
   variable cannot be bound to a stack that mentions it through a quotation. *)
Example occurs_check_sees_through_quotations :
  unify_stack [] UkExact empty_subst
    (SRow 0)
    (SPush (quot (Eff (SRow 0) (SRow 1) (SRow 2) (SRow 2))) (SRow 3)) = None.
Proof. vm_compute; reflexivity. Qed.

(* Quotation cells unify structurally, through all four of their rows. *)
Example quotation_rows_unify_pairwise :
  match unify_stack [] UkExact empty_subst
          (stack_of 0 [quot (Eff (stack_of 1 [i64]) (stack_of 1 [boolt])
                                  (SRow 2) (SRow 2))])
          (stack_of 0 [quot (Eff (stack_of 1 [TVar 3]) (stack_of 1 [TVar 4])
                                  (SRow 2) (SRow 2))]) with
  | Some s =>
      ty_eqb (resolve_ty s (TVar 3)) i64 = true
      /\ ty_eqb (resolve_ty s (TVar 4)) boolt = true
  | None => False
  end.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* Deep resolution recovers the whole quotation effect, not just its head. *)
Example zonk_recovers_a_whole_quotation :
  match unify_stack [] UkExact empty_subst
          (stack_of 0 [quot (Eff (stack_of 1 [i64]) (stack_of 1 [boolt])
                                  (SRow 2) (SRow 2))])
          (stack_of 0 [quot (Eff (stack_of 1 [TVar 3]) (stack_of 1 [TVar 4])
                                  (SRow 2) (SRow 2))]) with
  | Some s =>
      ty_eqb
        (zonk_ty s (quot (Eff (stack_of 1 [TVar 3]) (stack_of 1 [TVar 4])
                               (SRow 2) (SRow 2))))
        (quot (Eff (stack_of 1 [i64]) (stack_of 1 [boolt]) (SRow 2) (SRow 2)))
      = true
  | None => False
  end.
Proof. vm_compute; reflexivity. Qed.

Example quotation_effect_mismatch_rejects :
  unify_stack [] UkExact empty_subst
    (stack_of 0 [quot (Eff (stack_of 1 [i64]) (stack_of 1 [boolt]) (SRow 2) (SRow 2))])
    (stack_of 0 [quot (Eff (stack_of 1 [i64]) (stack_of 1 [strt]) (SRow 2) (SRow 2))])
  = None.
Proof. vm_compute; reflexivity. Qed.

(* The control flags are part of the TERM but not part of the UNIFICATION.
   `U-TYPE`'s quotation arm (checker.f:1587-1591) pairs the four rows and
   nothing else, so a quotation variable happily accepts a throwing quotation —
   and then RESOLVES to that term, flags and all.  That is exactly why
   `Control.do_exec` can read them off the resolved term. *)
Definition thrower : ty :=
  TQuot true true (Eff (stack_of 1 [i64]) (stack_of 1 [boolt]) (SRow 2) (SRow 2)).
Definition plain_quot : ty :=
  quot (Eff (stack_of 1 [i64]) (stack_of 1 [boolt]) (SRow 2) (SRow 2)).

Example control_flags_are_carried_not_unified :
  (* different terms *)
  ty_eqb thrower plain_quot = false
  (* yet the rows unify, at every kind *)
  /\ unifiesb (unify_ty [] UkExact empty_subst thrower plain_quot) = true
  (* and a variable bound to the throwing quotation resolves to IT *)
  /\ match unify_ty [] UkExact empty_subst (TVar 5) thrower with
     | Some s => q_xdead (resolve_ty s (TVar 5)) = true
                 /\ q_xhas (resolve_ty s (TVar 5)) = true
     | None => False
     end.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* An arity-0 nominal family (what `DEFTYPE` mints) is identified by family id
   and never meets a table type. *)
Example declared_nominals_are_distinct :
  unifiesb (unify_stack [] UkInput empty_subst (stack_of 0 [fam0 7]) (stack_of 0 [fam0 8])) = false
  /\ unifiesb (unify_stack [] UkInput empty_subst (stack_of 0 [fam0 7]) (stack_of 0 [nt])) = false
  /\ unifiesb (unify_stack [] UkInput empty_subst (stack_of 0 [nt]) (stack_of 0 [fam0 7])) = false
  /\ unifiesb (unify_stack [] UkInput empty_subst (stack_of 0 [fam0 7]) (stack_of 0 [fam0 7])) = true.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* A cell published by a raw storage definer refuses a nominal family, which is
   the value-position half of the pointee seal. *)
Example raw_cell_refuses_a_nominal :
  let s := MkSubst [] [] [2] in
  unifiesb (unify_stack [] UkInput s (stack_of 0 [TVar 2]) (stack_of 0 [fam0 7])) = false
  /\ unifiesb (unify_stack [] UkInput s (stack_of 0 [TVar 2]) (stack_of 0 [nt])) = true
  (* an ordinary variable is unaffected *)
  /\ unifiesb (unify_stack [] UkInput empty_subst
                 (stack_of 0 [TVar 2]) (stack_of 0 [fam0 7])) = true.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* A `DEFLINEAR` name is an ordinary table con in its own right, distinct from
   every other type; its linearity is a separate conservation pass this
   fragment does not model, but the raw-storage seal already sees it. *)
Example linear_cons_are_nominal :
  unifiesb (unify_stack [] UkInput empty_subst
              (stack_of 0 [TCon (CLinear 0)]) (stack_of 0 [TCon (CLinear 1)])) = false
  /\ unifiesb (unify_stack [] UkInput empty_subst
                (stack_of 0 [TCon (CLinear 0)]) (stack_of 0 [nt])) = false
  /\ unifiesb (unify_stack [] UkInput empty_subst
                (stack_of 0 [TCon (CLinear 0)]) (stack_of 0 [TCon (CLinear 0)])) = true
  /\ unifiesb (unify_stack [] UkInput (MkSubst [] [] [2])
                (stack_of 0 [TVar 2]) (stack_of 0 [TCon (CLinear 0)])) = false.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* Unifying two types directly is the same relation, entered at a type pair
   instead of a row pair. *)
Example unify_ty_agrees_with_unify_stack :
  unifiesb (unify_ty [] UkInput empty_subst (TPtr (TVar 1)) (TPtr i64)) = true
  /\ unifiesb (unify_ty [] UkInput empty_subst (TPtr u8) (TPtr cellt)) = false
  /\ unifiesb (unify_ty [] UkInput empty_subst u8 cellt) = true.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* --- 5. The sealed implicit row ------------------------------------ *)

(* The motivating case from docs/effects.md:28-31.  A word declared `( -- )`
   calls a trusted `img -- img` boundary.  Unification SUCCEEDS — the callee's
   input is simply taken from below — but the definition's implicit row is no
   longer open, so the seal rejects it. *)

Definition declared_empty : decl := decl_plain [] 0 9 [] [].
Definition trusted_img : word_eff := prim [] 1 [imgt] [imgt].
Definition balanced_word : word_eff := prim [] 1 [] [].

(* This is the whole point of the rule, so it is worth pinning both halves in
   one place: the body composes, AND its output row still joins the declared
   `( -- )` output — unification has no complaint anywhere.  The definition is
   rejected only because the implicit row came back bound to a spine. *)
Example hidden_underflow_unifies_but_fails_the_seal :
  match run_calls [] empty_subst
          (id_eff (we_din (decl_eff declared_empty)) (we_rin (decl_eff declared_empty)))
          [trusted_img] with
  | Some (s, cur) =>
      seal_okb s declared_empty = false
      /\ unifiesb (unify_stack [] UkCoerce s (we_dout cur)
                     (we_dout (decl_eff declared_empty))) = true
  | None => False
  end.
Proof. repeat split; vm_compute; reflexivity. Qed.

Example sealed_row_rejects_hidden_underflow :
  check_body [] declared_empty [trusted_img] = false.
Proof. vm_compute; reflexivity. Qed.

(* A callee that preserves the row leaves it open, and the definition stands. *)
Example sealed_row_admits_a_preserving_callee :
  check_body [] declared_empty [balanced_word] = true
  /\ check_body [] declared_empty [balanced_word; balanced_word] = true.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* The seal is about BINDING TO A SPINE, not about being untouched: a callee
   that merely renames the row (binding it to another row variable) preserves
   it, exactly as `ROW-OPEN?` decides. *)
Example preserving_callee_may_rename_the_row :
  match run_calls [] empty_subst
          (id_eff (we_din (decl_eff declared_empty)) (we_rin (decl_eff declared_empty)))
          [balanced_word] with
  | Some (s, _) => row_openb s 0 = true
  | None => False
  end.
Proof. vm_compute; reflexivity. Qed.

(* A declaration that DOES take the value is fine: the cell the callee consumes
   comes from the declared inputs, not from below them. *)
Definition declared_img : decl := decl_plain [] 0 9 [imgt] [imgt].

Example declared_input_is_not_underflow :
  check_body [] declared_img [trusted_img] = true.
Proof. vm_compute; reflexivity. Qed.

(* And an output mismatch is still caught after the seal passes. *)
Definition producer : word_eff := prim [] 1 [] [i64].

Example output_join_still_applies :
  check_body [] (decl_plain [] 0 9 [] []) [producer] = false
  /\ check_body [] (decl_plain [] 0 9 [] [i64]) [producer] = true.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* --- 6. The return row --------------------------------------------- *)

(* `>R` moves one cell from the data row to the return row.  A callee that
   declared no `|` clause leaves the caller's return row alone. *)

Definition to_r : word_eff :=
  MkWordEff (Eff (stack_of 1 [TVar 3]) (SRow 1) (SRow 2) (stack_of 2 [TVar 3])) true.

Definition from_r : word_eff :=
  MkWordEff (Eff (SRow 1) (stack_of 1 [TVar 3]) (stack_of 2 [TVar 3]) (SRow 2)) true.

Example return_row_round_trips :
  match compose_fresh [] empty_subst (id_eff (stack_of 0 [i64]) (SRow 5)) to_r with
  | Some (s, w) =>
      (* after `>R` the data row is the bare tail: the i64 is gone *)
      stack_eqb (zonk_row s (we_dout w)) (zonk_row s (SRow 0)) = true
      /\ match compose_fresh [] s w from_r with
         | Some (s', w') =>
             (* and `R>` brings the SAME i64 back, not a fresh variable *)
             stack_eqb (zonk_row s' (we_dout w')) (SPush i64 (zonk_row s' (SRow 0)))
         | None => false
         end = true
  | None => False
  end.
Proof. repeat split; vm_compute; reflexivity. Qed.

Example callee_without_a_return_clause_leaves_it_alone :
  match compose_fresh [] empty_subst (id_eff (SRow 0) (SRow 5)) balanced_word with
  | Some (s, w) => stack_eqb (resolve_row s (we_rout w)) (SRow 5) = true
  | None => False
  end.
Proof. vm_compute; reflexivity. Qed.

(* A definition may not leave a value stranded on the return row: the base
   return row is sealed exactly as the data row is. *)
Definition declared_with_return : decl := decl_with_return [] 0 1 [i64] [] [] [].

Example return_base_row_is_sealed_too :
  match run_calls [] empty_subst
          (id_eff (we_din (decl_eff declared_with_return))
                  (we_rin (decl_eff declared_with_return)))
          [from_r] with
  | Some (s, _) => seal_okb s declared_with_return = false
  | None => False
  end.
Proof. vm_compute; reflexivity. Qed.

(* --- 7. Type families of arity n: widths and hidden fields ---------- *)

(* The three families below are the ones the measurements were taken over.
   Each is written exactly as the fixture declared it, and each recorded shape
   was read back out of the live registry with `REFLECT` (test/checker-assert.f)
   rather than assumed.

     SUMTYPE option 1
       VARIANT none    ;VARIANT
       VARIANT some a  ;VARIANT
     ;SUMTYPE                                   (lib/adt/option.f)
       -> arity 1, TK-SUM, slots 1, width 2

     SUMTYPE result 2
       VARIANT ok  a ;VARIANT
       VARIANT err b ;VARIANT
     ;SUMTYPE                                   (lib/adt/result.f)
       -> arity 2, TK-SUM, slots 1, width 2

     package PP public
     STRUCTURE pair 1
        FIELD x a
        FIELD y n
     ;STRUCTURE
     ;package
       -> arity 1, TK-PRODUCT, slots 2, width 2

   Family ids are the registry's own allocation order and mean nothing beyond
   identity, so any distinct numbers do. *)

Definition f_option : nat := 100.
Definition f_result : nat := 101.
Definition f_pair : nat := 102.
Definition f_serial : nat := 103.   (* an arity-0 `DEFTYPE` nominal *)

Definition d_option : famdef :=
  MkFamDef 1 TkSum false 1 false [[]; [SchParam 0]] [].
Definition d_result : famdef :=
  MkFamDef 2 TkSum false 1 false [[SchParam 0]; [SchParam 1]] [].
Definition d_pair : famdef :=
  MkFamDef 1 TkProduct false 2 false [] [SchParam 0; SchCell].

Definition adts : fenv :=
  [(f_option, d_option); (f_result, d_result); (f_pair, d_pair);
   (f_serial, nominal_famdef)].

Definition optn : ty := fam_app f_option [nt].
Definition optidx : ty := fam_app f_option [idxt].
Definition optopt : ty := fam_app f_option [optn].
Definition opta : ty := fam_app f_option [TVar 2].
Definition pairn : ty := fam_app f_pair [nt].
Definition serial : ty := fam0 f_serial.

(* The declared widths, which is what `REFLECT:WIDTH` reports. *)
Example declared_widths_match_the_registry :
  (fam_arity adts f_option, fam_width adts f_option) = (1, 2)
  /\ (fam_arity adts f_result, fam_width adts f_result) = (2, 2)
  /\ (fam_arity adts f_pair, fam_width adts f_pair) = (1, 2)
  /\ (fam_arity adts f_serial, fam_width adts f_serial) = (0, 1).
Proof. repeat split; vm_compute; reflexivity. Qed.

(* A MULTI-CELL LAYOUT VALUE.  `option<n>` is two stack cells, and that is not
   a bookkeeping detail — it decides programs.  Measured:

     : W ( option<n> -- ) drop ;      -> exit 0
     : W ( option<n> -- n ) drop ;    -> exit 70,
         `at 'drop' expected: n actual:`
     : W ( option<n> -- ) drop drop ; -> exit 70, at the second `drop`
     : W ( option<n> -- ) ;           -> exit 70,
         `at 'W' expected: actual: option<n>`

   One `drop` takes both cells, because `drop` is a whole-bundle transport;
   there is no `n` under it and there is no second cell for a second `drop`. *)
Example a_layout_value_is_more_than_one_cell :
  t_width adts empty_subst optn = 2
  /\ t_width adts empty_subst pairn = 2
  /\ t_width adts empty_subst serial = 1
  /\ t_width adts empty_subst nt = 1
  /\ t_width adts empty_subst (TPtr optn) = 1.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* WIDTH IS ARG-AWARE.  An argument that is itself a layout widens the bundle,
   because `TFAM-INST-WIDTH@` substitutes each parameter slot by the width of
   the matching argument rather than by one cell.

   Measured through `MATCH`, which refines a branch with the INSTANTIATED
   payload and so exposes the payload's width directly:

     : W2 ( option<n> -- n )
         MATCH option none OF MK-N ENDOF some OF ENDOF ;MATCH ;
     : W ( option<option<n>> -- n )
         MATCH option none OF MK-N ENDOF some OF W2 ENDOF ;MATCH ;   -> exit 0

   The `some` branch of an `option<option<n>>` hands its payload to a word
   that wants a two-cell `option<n>`, and it fits.  Giving that same branch a
   one-cell consumer does not:

     : W ( option<option<n>> -- n )
         MATCH option none OF MK-N ENDOF some OF DROP-N ENDOF ;MATCH ;
       -> exit 70, `at 'DROP-N' expected: n actual: option<n>` *)
Example a_layout_argument_widens_the_bundle :
  t_width adts empty_subst optopt = 3
  /\ t_width adts empty_subst (fam_app f_pair [optn]) = 3
  (* and every all-one-cell instantiation agrees with the DECLARED width *)
  /\ t_width adts empty_subst optidx = fam_width adts f_option
  /\ t_width adts empty_subst pairn = fam_width adts f_pair.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* THE EXPANSION ITSELF.  `PUSH-LOGICAL` is where a layout value stops being
   one thing: slot 0 goes down first and the tag ends up on top. *)
Example push_logical_expands_a_closed_layout :
  push_logical adts empty_subst optn (SRow 0)
    = SPush (fam_hid f_option 1 [nt]) (SPush (fam_hid f_option 0 [nt]) (SRow 0))
  (* a nominal, a con and a pointer each push exactly one cell *)
  /\ push_logical adts empty_subst serial (SRow 0) = SPush serial (SRow 0)
  /\ push_logical adts empty_subst nt (SRow 0) = SPush nt (SRow 0)
  /\ push_logical adts empty_subst (TPtr optn) (SRow 0) = SPush (TPtr optn) (SRow 0).
Proof. repeat split; vm_compute; reflexivity. Qed.

(* AN OPEN ARGUMENT IS NOT EXPANDED.  If any argument is still an unresolved
   variable the width is not yet known, so the value stays ONE conservative
   logical cell.  Measured, and nothing differs between these two but the
   argument:

     : W ( option<n> -- ) drop ;  -> exit 0
     : W ( option<a> -- ) drop ;  -> exit 70,
         `at 'drop' expected: a actual: option<b>`

   The second is refused because a one-cell logical bundle is still a layout
   param, and `LAYOUT-BLOCK?` refuses it a variable — the transport machinery
   that moves an expanded group has nothing to move.

   Note also what `MK-HIDDEN` copies: the source's argument TERMS, not their
   resolutions.  A hidden field of an `option<a>` whose `a` has since been
   bound to `n` still spells its argument `a`. *)
Example an_open_argument_is_not_expanded :
  layout_args_openb empty_subst opta = true
  /\ layout_args_openb empty_subst optn = false
  /\ push_logical adts empty_subst opta (SRow 0) = SPush opta (SRow 0)
  /\ let s := MkSubst [(2, nt)] [] [] in
     layout_args_openb s opta = false
     /\ push_logical adts s opta (SRow 0)
        = SPush (fam_hid f_option 1 [TVar 2])
                (SPush (fam_hid f_option 0 [TVar 2]) (SRow 0)).
Proof. repeat split; vm_compute; reflexivity. Qed.

(* NO LAYOUT CELL EVER BINDS A TYPE VARIABLE — hidden or logical, expanded or
   not.  This is `LAYOUT-BLOCK?` at the ordinary window, and it is why a
   polymorphic word cannot copy, drop or capture a bundle.  Measured over
   `: DUPA ( a -- a a ) dup ;`, which certifies on its own:

     : W ( option<n> -- option<n> option<n> ) DUPA ;  -> exit 70,
         `at 'DUPA' expected: a actual: option<n>`
     : W ( option<a> -- option<a> option<a> ) DUPA ;  -> exit 70,
         `at 'DUPA' expected: a actual: option<b>`

   An arity-0 nominal is NOT a layout family, so it is unaffected — which is
   what keeps the arity-0 fragment above working unchanged. *)
Example a_layout_cell_never_binds_a_variable :
  unifiesb (unify_ty adts UkInput empty_subst (fam_hid f_option 1 [nt]) (TVar 5)) = false
  /\ unifiesb (unify_ty adts UkInput empty_subst (fam_hid f_option 0 [nt]) (TVar 5)) = false
  /\ unifiesb (unify_ty adts UkInput empty_subst optn (TVar 5)) = false
  /\ unifiesb (unify_ty adts UkInput empty_subst opta (TVar 5)) = false
  /\ unifiesb (unify_ty adts UkInput empty_subst (TVar 5) optn) = false
  /\ unifiesb (unify_ty adts UkInput empty_subst serial (TVar 5)) = true.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* A HIDDEN FIELD IS CHECKER-OWNED (`PARAM-HID-OK?`, checker.f:1395-1399).  It
   pairs with the same family at the SAME slot and with nothing else — not
   with another slot, and not with the logical value it came from.  That, and
   nothing else, is what holds a bundle's cells together on the row. *)
Example hidden_fields_pair_only_at_the_same_slot :
  unifiesb (unify_ty adts UkExact empty_subst
              (fam_hid f_option 0 [nt]) (fam_hid f_option 0 [TVar 2])) = true
  /\ unifiesb (unify_ty adts UkExact empty_subst
                (fam_hid f_option 0 [nt]) (fam_hid f_option 1 [nt])) = false
  /\ unifiesb (unify_ty adts UkExact empty_subst
                (fam_hid f_option 0 [nt]) optn) = false
  /\ unifiesb (unify_ty adts UkExact empty_subst
                optn (fam_hid f_option 0 [nt])) = false
  /\ unifiesb (unify_ty adts UkExact empty_subst
                (fam_hid f_option 0 [nt]) (fam_hid f_result 0 [nt; nt])) = false.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* IDENTITY IS THE FAMILY ID PLUS THE ARGUMENTS, PAIRWISE — and an argument is
   an ordinary unification position, so it can bind a variable.  Measured:

     : W ( option<n> -- option<idx> ) ;
       -> exit 70, `at 'W' expected: option<idx> actual: option<n>`
     : W ( option<n> -- result<n,n> ) ;
       -> exit 70, `at 'W' expected: result<n,n> actual: option<n>`
     : W ( result<n,idx> -- result<idx,n> ) ;
       -> exit 70, `at 'W' expected: result<idx,n> actual: result<n,idx>`
     : W ( result<n,idx> -- result<n,idx> ) ;   -> exit 0
     : W ( option<n> -- ) DROP-N ;
       -> exit 70, `at 'DROP-N' expected: n actual: option<n>`

   And the arity is settled before any of this, at signature parse:
     : W ( option -- ) drop ;      -> exit 70, `wrong arity for type family 'option'`
     : W ( option<n,n> -- ) drop ; -> exit 70, same message *)
Example family_identity_is_the_id_and_the_arguments :
  unifiesb (unify_ty adts UkExact empty_subst optn optidx) = false
  /\ unifiesb (unify_ty adts UkExact empty_subst
                optn (fam_app f_result [nt; nt])) = false
  /\ unifiesb (unify_ty adts UkExact empty_subst
                (fam_app f_result [nt; idxt]) (fam_app f_result [idxt; nt])) = false
  /\ unifiesb (unify_ty adts UkExact empty_subst
                (fam_app f_result [nt; idxt]) (fam_app f_result [nt; idxt])) = true
  /\ match unify_ty adts UkExact empty_subst optn opta with
     | Some s => ty_eqb (resolve_ty s (TVar 2)) nt = true
     | None => False
     end.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* WHERE A LOGICAL CELL MEETS A WIDTH EXPANSION (`LOGHID-AT?` /
   `LOGHID-EXPAND`).  A stored effect keeps a layout value as one logical cell
   whenever an argument is open, so a caller holding the expanded run and a
   callee declaring the single cell have rows of DIFFERENT LENGTH.  The row
   rule notices the hidden side is at its tag, expands the logical side in
   place, and the two rows re-pair cell for cell — proving the logical side's
   arguments equal to the hidden side's on the way.

   Measured, and this is the direct witness that expansion happened:

     : ID-OPT ( option<a> -- option<a> ) ;
     : W ( option<n> -- option<n> ) ID-OPT ;   -> exit 0

   and the mirror, a one-cell caller meeting a two-cell callee:

     : ID-N ( option<n> -- option<n> ) ;
     : W ( option<a> -- option<a> ) ID-N ;     -> exit 0 *)
Definition opt_expanded : stack := push_logical adts empty_subst optn (SRow 0).
Definition opt_logical : stack := SPush opta (SRow 0).

Example a_logical_cell_meets_a_width_expansion :
  loghid_atb adts empty_subst opt_expanded opt_logical = true
  (* the hidden side must be at its TAG for the rows to be aligned *)
  /\ loghid_atb adts empty_subst
       (SPush (fam_hid f_option 0 [nt]) (SRow 0)) opt_logical = false
  /\ match unify_stack adts UkInput empty_subst opt_expanded opt_logical with
     | Some s => ty_eqb (resolve_ty s (TVar 2)) nt = true
     | None => False
     end
  /\ unifiesb (unify_stack adts UkInput empty_subst opt_logical opt_expanded) = true
  (* a DIFFERENT family does not expand into it: the rows stay mismatched *)
  /\ unifiesb (unify_stack adts UkInput empty_subst
                 opt_expanded (SPush (fam_app f_result [nt; nt]) (SRow 0))) = false.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* A NOMINAL SCALAR (`NOM-SCALAR?`, checker.f:1310-1316) is the OTHER thing an
   arity-0 family can be: a one-cell value whose whole meaning is its family
   identity.  It is an ordinary value everywhere except inside a POINTEE,
   where a type variable may not absorb it — otherwise a raw pointer from
   `variable` or `create` would acquire the identity by plain unification,
   bypassing the one introduction form.  This is the pointee mirror of the RAW
   value rule already modelled above. *)
Example a_pointee_may_not_absorb_a_nominal_scalar :
  nom_scalarb adts empty_subst serial = true
  /\ nom_scalarb adts empty_subst optn = false
  /\ nom_scalarb adts empty_subst (fam_hid f_option 0 [nt]) = false
  (* value position: an ordinary bind *)
  /\ unifiesb (unify_ty adts UkInput empty_subst serial (TVar 5)) = true
  (* pointee position: refused, in both directions *)
  /\ unifiesb (unify_ty adts UkInput empty_subst
                (TPtr serial) (TPtr (TVar 5))) = false
  /\ unifiesb (unify_ty adts UkInput empty_subst
                (TPtr (TVar 5)) (TPtr serial)) = false.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* THE TWO OCCURS CHECKS DISAGREE, and the disagreement is in the code, not
   here.  `TY-OCC?` (checker.f:1278-1285) descends into family ARGUMENTS;
   `ROW-OCC?` (checker.f:963-977) walks the spine and the pointer / quotation
   chain and never enters a `T-PARAM`.  So a row variable reachable only
   through an argument is invisible to the row occurs check.  No surface
   fixture was found that reaches it — the parameter-kind rules restrict what
   a layout family's argument may be — but the two walks are written this way
   and the model follows them. *)
Example the_two_occurs_checks_disagree_about_arguments :
  (* a quotation CELL on the row hides nothing from the row occurs check *)
  unify_stack adts UkExact empty_subst (SRow 0)
    (SPush (quot (Eff (SRow 0) (SRow 1) (SRow 2) (SRow 2))) (SRow 3)) = None
  (* the same quotation as an ARGUMENT is invisible to it *)
  /\ unifiesb (unify_stack adts UkExact empty_subst (SRow 0)
       (SPush (fam_app f_serial [quot (Eff (SRow 0) (SRow 1) (SRow 2) (SRow 2))])
              (SRow 3))) = true
  (* while the TYPE occurs check does see into an argument *)
  /\ ty_occurs empty_subst 9 (fam_app f_option [TVar 9]) = true
  /\ ty_occurs empty_subst 9 (fam_app f_option [nt]) = false.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* --- 8. Failure is a value ------------------------------------------ *)

(* Everything above is decided by total functions.  Nothing here can raise,
   diverge, or be partial: unification returns `option subst` and the boundary
   returns `bool`. *)

Example failure_is_a_value :
  unify_stack [] UkExact empty_subst (stack_of 0 [i64]) (stack_of 0 [strt]) = None
  /\ check_body [] declared_empty [trusted_img] = false.
Proof. repeat split; vm_compute; reflexivity. Qed.

(* A signature type is a term, not a string: nothing here can be built by
   spelling. *)
Fail Definition rows_are_not_types : ty := SRow 0.
