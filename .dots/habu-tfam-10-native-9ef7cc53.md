---
title: "TFAM 10: native+Gforth MATCH/constructor lowering + bad-tag proof"
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-03T23:36:48.948255+02:00\""
---

PLAN.md item 10. Keyword data/labels/EMIT-KWDATA + lowering for MATCH/OF/ENDOF/ENDMATCH, token consumption, tag pushes, compare/branch chains, invalid-tag die (no normal continuation) in habu2.f AND bootstrap/cg/forth.fs; object/AOT test-entry support for preseeded bad-tag runs (entry identity = package/WID/record id + seeded cells + mode in every cache key/schema/index); bad-tag dies at runtime on native AND Gforth-recovered candidates; one-payload/wide/zero-payload + arbitrary third family; byte-identical fixpoint. Gate 17k. Depends: TFAM 9, 12.

## Audit (fable-tfam12, 2026-07-09) — what exists, design forks, slice plan

### What exists (item 9 = checker only; item 10 = compiler lowering, NONE yet)
- CHECKER (src/core/checker.f) fully checks construct/MATCH. Token machine
  `MM` (0/1/2/3), frame arena `MF-REC` (MF.FAM/TERM/BASE/OUT/SEEN/VCNT/TIX...),
  seen-bitset pool `MSEEN`, reject latch `MREJ`. Dispatch: `MATCH-TOK`,
  `MATCH-FAM-TOK`, `MATCH-VARIANT-TOK`, `MATCH-OF-TOK`, `MATCH-ENDOF`,
  `MATCH-SEMI`; `CF-ENDOF-DISPATCH` routes `endof` to CASE (frame kind 7/8) vs
  MATCH (kind 9/10). Construct: `CONSTRUCT-FAM-XT` (active-package only) +
  `CONSTRUCT-STEP-XT`. Friend XTs (installed by type-family.f): `MATCH-FAM-XT`
  (signature scope), `MATCH-VCOUNT-XT`, `MATCH-VAR-XT`, `MATCH-VTAG-XT`,
  `MATCH-PAY-XT`. Reject codes `E-MATCH-*`/`E-CONSTRUCT-*` (docs §24).
- METADATA (src/core/type-family.f): `TFAM-SLOTS@`=M (max payload cells),
  `TFAM-WIDTH@`=M+1 for sum/enum, `TFAM-SUM?`/`TFAM-ENUM?`/`TFAM-KIND@`;
  `SUMV-TAG@`=k, `SUMV-PAYCELLS@`=p. Generated-ctor body shape (item 8,
  sumtype.f TDGEN-BODY): `(M-p) × "0 " + "<k>" + " ;"` i.e. push M-p zero pads,
  push tag k. This IS the construct runtime rule (docs §12/§16).
- COMPILER (src/habu/habu2.f): NO construct/MATCH lowering. `construct`/`MATCH`/
  `;match` tokens are NOT keywords, hit the undefined-word path → `E-UNDEFINED`
  at compile (fail-closed). OF/ENDOF ARE keywords (CASE). Model to copy: CASE
  lowering `J-CASE`(1046)/`J-OF`(1049: pop x16, cmp, cbz, drop)/`J-ENDOF`(=J-ELSE)
  /`J-ENDCASE`(1061), control-flow stack `LCFPUSH`/`LCFPOP`/`LPAT`, keyword
  strings in `EMIT-KWDATA`(988), dispatch in `EM-COMPILE-CONTROL-KEYWORDS`(3913)
  via `CFN-ENTRY`/`CF-ENTRY`, label vars init at ~4301. Literal push `C-LIT`.
  Bundle-atomic VS handling landed in item 12 (jit.fs VS tags); MATCH must
  `LVSPILL` the bundle before its branch (control flow).
- GFORTH MIRROR (bootstrap/cg/forth.fs): FULL mirror of the CASE lowering —
  `LCFPUSH/LCFPOP/LPAT`(205), `LKWCASE/OF/ENDOF/ENDCASE`(211), `EMIT-KWDATA`
  (1770), `J-CASE`(1828)/`J-OF`(1831), `EMIT-COMPILE-CONTROL-KEYWORDS`(3286).
  NOTE forth.fs:62 "gforth stage0 has NO package/DEF-* system". It compiles the
  FIRST native engine; the engine's OWN source uses no MATCH/construct, so
  forth.fs does not need to LOWER user MATCH — but it DOES bake the reserved
  keyword table + engine codegen, so any new `EMIT-KWDATA` rows and any J-*
  codegen words the engine source itself references must mirror. jit.fs mirrors
  the VS shuffle path.
- Gforth PARITY meaning (confirmed): a "Gforth-recovered candidate" is a native
  engine BUILT via `HABU_ALLOW_BOOTSTRAP=1 tools/bootstrap.sh` (Gforth stage0 →
  native fixpoint). Post-fixpoint it IS the full native engine, so it runs the
  SAME MATCH/construct/bad-tag lowering. Parity = build via bootstrap.sh, run the
  identical fixtures, get identical results. This is NOT the vacuous-boundary
  precedent of item 4: item 10's title requires it and the recovery corpus gains
  these forms once fixtures land.
- PIN (test/gate-engine-lib.f `GE-CONSTRUCT-PENDING` 539): asserts certified
  construct body compile → rc 70 `E-UNDEFINED: construct`, and interpret construct
  → rc 70. Item 10 FLIPS this to execution fixtures. `GE-TYPE-MATCH-SUITE`(399)
  runs the CHECK-only match suite on candidate + bin/hb.

### Design forks (recommendation)
- FORK 1 — how the compiler gets (tag, pad, slots) at a construct/match token,
  without dict lookup:
  (A) compiler enters a construct/match MODE, captures the operand tokens, and
      calls the EXISTING checker friend XTs (CONSTRUCT-FAM-XT/-STEP, MATCH-FAM-XT
      /-VAR/-VTAG/-VCOUNT + TFAM-SLOTS@/SUMV-PAYCELLS@) via the C-CALL-X11-SAVED
      friend-call bridge, emitting from the returned numbers. Resolution is
      deterministic and re-uses the checker's exact scope rules (owner-only for
      construct, signature-scope for match). The compiler runs the body BEFORE
      the `;`-time hook, but the registry is already populated (families declared
      earlier), so resolution is stable.
  (B) checker writes a per-token lowering table during CHECK; compiler replays it
      (pass-2 style, like TFAM 12 width facts).
  RECOMMEND (A): matches the established friend-XT bridge (C-CALL-CHECKER-*),
  no new cross-pass table, no ordering hazard (compiler resolves at its own
  token position). (B) only wins if compile-time resolution can diverge from
  check-time — it cannot here (same registry, same token, same scope XTs). Add
  thin compiler-facing wrappers that return raw cells (tag/pad/slots/vcount) so
  the friend surface stays checker-owned.
- FORK 2 — compiler match/construct MODE state: mirror the checker `MM` machine
  with an engine DATA cell (e.g. CMM-CELL: 0 off /1 want-family /2 want-variant-
  or-;match /3 want-of) tested in LMAIN BEFORE local/keyword/literal/call/undef
  dispatch, so operand tokens never reach LFIND. Construct is a 2-token capture
  (family, variant) with no frame. RECOMMEND: one shared mode cell + a small
  capture buffer for the family/variant token spans; reset on `;MATCH`/def end;
  fail-closed on def-end mid-capture (mirror MD-TRUNC).
- FORK 3 — bad-tag die: emit `drop tag; drop all M slots; <die>` with NO fall-
  through. Model on C-DIE-* (write msg to fd2 + NR-EXIT-GROUP) with a named code
  (add `E-BAD-TAG`), message `"hb: bad <family> tag"`. The family name string
  must be materialized at emit (from TFAM-NAME$). RECOMMEND a single shared
  `C-DIE-BAD-TAG` engine helper taking the family-name span, so both native and
  the runtime path share one exit; assert the no-continuation instruction
  sequence as ADDITIONAL evidence, NOT a substitute for the runtime bad-tag run.

### Slice plan (one commit each; every commit: fixpoint proof + FULL gate +
### six type suites + maki/test.f + prop census for any new prim +
### dot-dep-lint + typed-local-diff-lint + TRUSTED.md pins as needed)
1. **Keyword-data + compiler-facing metadata surface.** Add LKWCONSTRUCT/
   LKWMATCH/LKWENDMATCH label vars + `EMIT-KWDATA` rows (native + forth.fs
   mirror) + label init; add compiler-facing friend wrappers over the checker/
   registry resolution (FORK 1A) returning raw (tag/pad/slots/vcount/tag-of-var).
   No dispatch yet → GE-CONSTRUCT-PENDING still green (tokens still undefined).
   Proof: fixpoint + gate unchanged-green.

   **LANDED (fable-tfam12, "TFAM 10 slice 1: keyword data + lowering metadata
   surface").** What shipped, exactly:
   - Keyword data: `LKWCONSTRUCT`/`LKWMATCH`/`LKWSEMIMATCH` label vars,
     `EMIT-KWDATA` rows ("construct", "match", ";match") and
     `EMIT-LABEL-CONTROL` inits in BOTH habu2.f and the forth.fs stage0 mirror.
     `LKWCMP` folds A-Z on the token side, so uppercase `MATCH`/`;MATCH`
     source will hit the lowercase rows when slices 2-3 wire dispatch. Nothing
     compares against the new rows yet: `construct` still resolves nothing →
     `E-UNDEFINED` exit 70 on compile AND interpret (probed directly on the new
     engine; GE-CONSTRUCT-PENDING green in the gate).
   - FORK 2 mode cell: `CMM-CELL` at **$27A8** (layout.f + forth.fs mirror
     constant). NOT $260 as the stale layout free-hole comment suggested — the
     first build's own guard caught that live: `VVAL-OFF` ($250) + `VSMAX`
     cells spans $250..$350 (the JIT virtual-stack value table), so $260 =
     VVAL[2] and any 3-deep VS write clobbered it (DEF-TKA/DEF-TKL survive in
     that span only via name-token-time liveness when the VS is empty). $27A8
     is the last old CRSIG slot between P2LOC0-CELL ($27A0) and DOESB-CELL
     ($27B0), rg-verified free; layout.f free-hole comments corrected (the
     other old holes were reclaimed by the item-12 P2-* cells).
   - FORK 2 guard: `EM-COMPILE-ADT-MODE` (habu2.f, TRUST row + TRUSTED.md pin
     + classes-block habu2 count 109→110) emitted at the LCOMPILE head after
     EM-P2-COUNT and BEFORE semi/local/keyword/literal/call/undefined — the
     capture position slices 2-3 take over. Armed-with-no-handler dies
     fail-closed: "hb: adt lowering pending: " + token + NL, exit 70
     (ADTMSG-LEN 26). Cleared at `:`/`TRUSTED:` entry and in
     EM-RESET-COMPILE-STATE (all mirrored in forth.fs: EMIT-COMPILE-ADT-MODE,
     C-COLON-PENDING-DREC clear, EMIT-RESET-COMPILE-STATE clear).
   - FORK 1A wrappers (src/core/type-family.f, checked, after TFAM-MATCH-PAY):
     `TFL-CON? ( fa fu va vu -- tag pads ok )` construct one-shot (owner-only
     scope via TFAM-ACTIVE-PKG$/TFAM-FIND-IN + sum/enum kind gate),
     `TFL-CON-FAM?`, `TFL-MATCH-FAM? ( a u -- fam ok )` (signature scope via
     TFAM-SIG-RESOLVE incl. qualified + ambiguity→pure-false), `TFL-VAR?
     ( a u fam -- vid ok )`, `TFL-VPADS ( fam vid -- M-p )`, helpers
     `TFL-SUMKIND?`/`TFL-FOLD$`. All fold raw engine tokens through the
     checker's TOKFOLD (single TKF buffer — fold→resolve strictly sequential),
     latch NO diagnostics, apply NO checker-row effect. Emitters will also call
     the existing named accessors directly: SUMV-TAG@, SUMV-PAYCELLS@,
     TFAM-SLOTS@, TFAM-VAR-COUNT@, TFAM-NAME$.
   - Tests: test/type-family-suite.f TFL-SURFACE block — tag/pads per variant
     (incl. zero-payload pads=M), case-fold, pure misses (unknown fam/var,
     cell-kind `span`), owner-only vs signature scope (pkgx `solo`
     constructs-false but matches-true from top level), qualified `pkgx:amb`,
     ambiguous `amb` false, TFL-VAR?/TFL-VPADS.
   - Emitter-asm note for slice 2/3: local `{: x:label :}` values are used
     BARE with ADR/CBZ (`LABEL@` is only for label VARIABLES — the checker
     rejects the mix loudly), and x11 must stay clear of scratch around the
     C-FIND-GLOBAL/C-CALL-X11-SAVED bridge.
   - Proofs: install --force twice = "bin/hb refresh OK: compiler fixpoint"
     both runs (byte-identical); full gate rc=0 "PASS: native test suite
     (fixpoint + engine suite + checked hb + repl + hb-build) (10010ms <=
     40000ms budget)"; six type suites ok; maki/test.f ok;
     bootstrap-codegen-test ok; compiler-dispatch-test ok; typed-local-diff-
     lint/host-lint/filemap-lint/dot-dep-lint/trusted-inventory ok. No new
     prims (TFL words are ordinary checked defs) → prop census unchanged.
2. **Construct lowering (native).** Compiler `construct` mode (FORK 2): capture
   family+variant, resolve via slice-1 surface, emit (M-p)×push0 + push tag on
   the VS. Flip GE-CONSTRUCT-PENDING construct legs from E-UNDEFINED pins to
   execution round-trips (build a def with `construct`, run it, inspect the
   physical slot0..tag). Keep interpret-mode construct fail-closed OR lower it too
   per docs (decide + record). One-payload + zero-payload + wide + an arbitrary
   third family (not result/option/color).

   **LANDED (fable-tfam12, "TFAM 10 slice 2: construct lowering").**
   - Dispatch: `J-CONSTRUCT` (CFN-ENTRY, keyword row) arms CMM=1; the operand
     tokens are consumed at the LCOMPILE head (`EM-COMPILE-ADT-MODE` →
     `EM-ADT-CON-FAM`/`EM-ADT-CON-VAR`) before semi/local/keyword/literal/call/
     undefined dispatch — they never reach LFIND. CFN (no spill): construct
     only ADDS VS constants, like the generated-ctor literal tokens.
   - Family step (CMM=1): EAGER resolve via `tfl-con-fam?` bridge; id parked in
     the new `CMFAM-CELL` ($1B0 — no exact user, no covering ranged region; the
     seal suite's deliberate $1A0 poke hole left alone); arm CMM=2. Eager
     resolution means no operand STRING crosses a possible REPL line refill.
     Variant step (CMM=2): `tfl-cvar?` (new checked wrapper; TFL-CON?
     refactored through it) returns ( tag pads ok ); emission = pads×LVPUSHC(0)
     + LVPUSHC(tag) — the SAME VS-constant path item-8 ctor body literals
     compile through, so runtime cells are identical by construction.
   - DECISION (die messages): resolution failure dies fail-closed at ITS token
     with named messages `hb: construct: unknown family: <tok>` /
     `hb: construct: unknown variant: <tok>`, exit 70. The slice-1 generic
     "adt lowering pending" message is retired (construct states fully
     handled; slice 3 adds MATCH states). Foreign-package and wrong-kind
     collapse into "unknown family" — owner scope makes them literally
     unknown; the checker's richer E-CONSTRUCT-* diagnostics remain the
     check-path surface, which the engine leg never reaches. `construct fam ;`
     dies as "unknown variant: ;" — MD-CON-TRUNC's engine mirror.
   - TWO ENGINE FACTS LEARNED (both fixed, both in LESSONS):
     (a) mid-body the code REGION is RW; the TFL bridge targets are checker
     words compiled INTO that region → BLR from the compile loop SIGBUSed
     (W^X). Every leg opens an RX window (LPROT 5) around find+call and
     returns to RW before emission resumes (EM-P2-CARVE-W's "caller holds the
     RX window" was the documented precedent). EM-ADT-CON-PUSHES frames
     x12/x13 on SP, flips RW, then loops (LVPUSHC may spill = emit).
     (b) the central body capture is the LBCAP at EM-COMPILE-KEYWORDS' head —
     downstream of ADT-MODE — so consumed operand tokens never reached the
     checker's body (MD-CON-TRUNC at `;`). Each leg now LBCAPs its operand
     first; `;` stays uncaptured (EM-COMPILE-SEMI runs before the keyword
     LBCAP), matching the checker's body contract.
   - Mirror: forth.fs gains C-FIND-GLOBAL (plain LFIND — stage0 has no package
     cells, exact global-scope equivalent), J-CONSTRUCT + registration, both
     operand legs with the identical LPROT/LBCAP discipline, CMFAM-CELL.
   - GE flip: GE-CONSTRUCT-PENDING → `GE-CONSTRUCT-EXEC` = GE-CONSTRUCT-ROUND
     (round-trip vs generated word, cell-for-cell via a fixture-local TRUSTED
     unpack: one-payload 0/0/7, wide 1/4/3, zero-payload 2/0/0, generated
     GECN:ONE identical 0/0/7 — exact-stdout pinned) + GE-CONSTRUCT-BAD-VARIANT
     (rc 70 + named msg) + GE-CONSTRUCT-FOREIGN (public family in a package,
     construct from top level → rc 70 "unknown family: gefr") + the interpret
     pin unchanged (rc 70 E-UNDEFINED — construct is a compile-only keyword;
     the DNAME-WIDE gate owns the interpret surface). Direct probes also
     proved case folding (`construct G4 CC` executes) and quotation interiors
     compile through the same loop.
   - TRUSTED.md: +3 rows (em-adt-con-fam/-pushes/-var), habu2 classes count
     110→113. No new prims (census unchanged). type-family-suite gains
     TFL-CVAR? fixtures.
   - Proofs: fixpoint ×2 ("compiler fixpoint" both); FULL gate rc=0 twice
     (final tree: "PASS: native test suite ... (10432ms <= 42000ms budget)");
     engine runtime slice direct run prints "PASS: construct lowers natively;
     interpret + foreign scope stay fail-closed"; six type suites ok; maki ok;
     bootstrap-codegen-test ok; compiler-dispatch-test ok; typed-local-diff-
     lint/host/filemap/dot-dep/trusted-inventory ok.
3. **MATCH/OF/ENDOF/;MATCH lowering (native).** MATCH mode + frame; peek tag,
   compare/branch chain per variant, branch prologue (drop tag+pads, expose
   payload), ENDOF jump-to-join, ;MATCH join + invalid-tag die (FORK 3). Reuse
   the checker's CF-ENDOF-DISPATCH discipline: OF/ENDOF codegen must route by the
   compiler frame kind. Runtime round-trips execute every branch; bad-tag DIES
   with the named diagnostic (child-process fixture — a die exits the engine).

   **SLICE-3 DESIGN (refined by fable-tfam12 scout, 2026-07-09; NOT yet
   implemented — stopped before starting per the full-proof-battery budget rule;
   this note lives in its own empty change opnumukx on top of slice 2, so the
   slice-2 commit stays pristine).** Deep read of habu2.f CASE/construct lowering
   + checker MATCH machine yields a materially simpler compiler shape than the
   anchor above, plus one hazard fix:
   - **endof needs NO compiler routing.** The checker routes endof (MATCH-ENDOF vs
     CF-ENDOF) because they do different *type-frame* work. The COMPILER's endof is
     pure branch-placeholder bookkeeping: `J-ENDOF` (= `J-ELSE`, habu2.f:1048) pops
     the of-CBZ frame, emits a B-to-join, pushes that B frame, patches the CBZ — and
     never touches the data stack. This is **byte-identical** for CASE and MATCH
     branches (both push exactly one CF frame at `of`, via C-PUSHCP which snapshots
     LOCN/LOCF so branch locals scope correctly). So **reuse J-ENDOF unchanged**; do
     NOT add a compiler CF-frame kind. Only `of` needs routing.
   - **`of` routes via the CMM machine, not a keyword.** `MATCH` (new keyword
     `J-MATCH`, register in EM-COMPILE-CONTROL-KEYWORDS `s" match" … LKWMATCH 5
     ['] J-MATCH CF-ENTRY`) arms a match CMM state; the family/variant/`of`/`;match`
     operand tokens are then consumed by EM-COMPILE-ADT-MODE (runs BEFORE the
     keyword dispatch), so `of`-in-match never reaches `J-OF`. `J-OF` stays
     CASE-only; `endof` still reaches the keyword dispatch (CMM=0 in the branch body)
     and hits the shared `J-ENDOF`. `;match` needs no keyword row — it is consumed
     at CMM=4; a stray `;match` at CMM=0 falls to EM-COMPILE-UNDEF (E-UNDEFINED,
     fail-closed; the checker already hard-rejects it via CF-TOK? MD-STRAY).
   - **CMM states extend construct's 1/2:** 3=match-want-family, 4=match-want-
     variant-or-`;match`, 5=match-want-`of`. Pending variant (tag,pads) = 2 cells,
     NON-nesting (no token ever falls between a variant and its `of`). Extend
     EM-COMPILE-ADT-MODE's dispatch (habu2.f:4356) with the 3/4/5 legs.
   - **Compiler match-frame stack NESTS** (checker fixture M7): (fam,M) per level +
     a depth cell. A `create CMATCH-… allot` arena in habu2.f + a depth variable is
     sufficient and SIDESTEPS the fixed-layout-cell hunt: it is pure compile-time
     compiler state, never baked into user code, so its address need only be
     self-consistent within each build — habu2's and forth.fs's create addresses
     need NOT match (unlike CMM-CELL/CMFAM-CELL, which slice 2 pinned as shared
     layout constants). M = TFAM-SLOTS@ captured at the family token.
   - **J-MATCH** (CF-ENTRY → LVSPILL spills the VS bundle to the physical data
     stack): push a CF sentinel frame x9=0 (J-CASE shape, habu2.f:1056) so `;match`
     can patch the B-chain with the J-ENDCASE loop; push a compiler match-frame; arm
     CMM=3. No emission (bundle already spilled).
   - **EM-ADT-MATCH-FAM** (CMM=3): LBCAP the token; RX-window the bridge call to
     `tfl-match-fam?` (signature scope) → fam; store fam + `fam TFAM-SLOTS@`=M in the
     top match-frame; arm CMM=4. Fail → die `hb: match: unknown family: <tok>` rc.
   - **EM-ADT-MATCH-VAR** (CMM=4): if token folds to `;match` → EM-MATCH-SEMI. Else
     LBCAP; bridge `tfl-var?` (fam) → vid; tag=`vid SUMV-TAG@`, pads=`fam vid
     TFL-VPADS`(=M−p); stash (tag,pads); arm CMM=5. Fail → die
     `hb: match: unknown variant: <tok>` rc.
   - **EM-ADT-MATCH-OF** (CMM=5): require token folds to `of` (else die
     `hb: match: expected of: <tok>`). Emit compare+prologue: peek tag
     `ldur x9,[x19,#-8]` ($F85F8269), `movz x16,#tag`, `cmp x9,x16`, C-PUSHCP + CBZ
     placeholder (skip-if-not-this-variant), then the branch prologue drop of the
     top (1+pads) cells (`sub x19,x19,#(8*(1+pads))`) leaving the p payload cells
     exposed. Arm CMM=0 (branch body compiles normally; VS empty, payload on
     physical stack — the CASE-arm convention).
   - **EM-MATCH-SEMI** (`;match`): CP is already at the bad-tag point (the last of's
     CBZ was patched here by its endof). Emit the bad-tag fall-through: drop the
     full bundle (`sub x19,x19,#(8*(M+1))`) + the runtime die; then run the
     J-ENDCASE B-chain-patch loop (habu2.f:1078) to point every accumulated endof-B
     at the join (CP after the die), stopping at the sentinel; pop the match-frame;
     CMM=0.
   - **BAD-TAG DIE — HAZARD + FIX (supersedes FORK 3's pointer plan).** FORK 3 said
     "materialize the family name pointer at emit (from TFAM-NAME$)". That is
     UNSAFE: TFAM-NAME$ resolves into the `TF-STR` pool (type-family.f:60-83), which
     is GROWABLE / mmap-relocatable (LESSONS "growable registries rebase on
     relocation"), so a LIT64-baked name pointer dangles after a later grow/relocate
     or snapshot. Correct: **emit the family NAME BYTES INLINE into the user code**
     (C-SDQ-style copy at emit — the bytes travel with the word, no live pool
     pointer), and a shared engine helper `C-DIE-BAD-TAG` that at RUNTIME writes
     `hb: bad ` + inline-name + ` tag\n` to fd 2 and `NR-EXIT-GROUP` with the new
     E-BAD-TAG code. (Inline bytes are also forward-compatible with slice-5
     AOT/object persistence, where a family id/pointer would need the registry
     restored.) Model the write/exit tail on C-DIE-TOKEN-NL (habu2.f:1608) but as
     code EMITTED INTO the user word (C-EMITW sequence + inline-bytes span), not the
     engine's own compile-time die. Scout `C-SDQ` / the `s"`-into-user-code path
     first — this inline-string + runtime-syscall emission is the one un-derisked
     novelty (slice 2's dies were all compile-time engine dies).
   - **E-BAD-TAG code:** construct's dies use raw exit `70`. FORK 3 wants a distinct
     named code. Locate the E-code convention home (no `src/config.fs` in this tree;
     E-* names live across render.f/repl.f/habu2.f) and pick a distinct exit code +
     a `docs/type-families.md §24` diagnostic row.
   - **Test battery** (extend GE-CONSTRUCT-ROUND, test/gate-engine-lib.f:554):
     construct+MATCH round-trip per variant (zero/one/multi payload — payload cells
     arrive correctly, exact-stdout pin); a NESTED match; the forged-tag die via a
     `TRUSTED: … ( -- fam<…> ) <payload> <bad-tag> ;` (GE-WMK pattern, :472) matched
     in a CHILD process asserting the E-BAD-TAG rc + `hb: bad <fam> tag` text; CASE
     fixtures stay green; interpret-mode MATCH stays fail-closed (E-UNDEFINED — MATCH
     is a compile-only keyword, like construct; DNAME-WIDE owns the interpret
     surface).
   - **forth.fs mirror:** EMIT-KWDATA rows already present (slice 1). Mirror J-MATCH
     + the three EM-ADT-MATCH-* legs + EM-MATCH-SEMI + C-DIE-BAD-TAG + the CMATCH
     arena, and register the `match` keyword, all per slice 2's parity discipline
     (C-FIND-GLOBAL = plain LFIND in stage0).
   - **Four engine facts to honor** (predecessor): (1) mid-body code region is RW —
     wrap each bridge find+call in an RX window (LPROT 5 → 3); (2) LBCAP each
     consumed operand (the central LBCAP sits downstream of ADT-MODE dispatch);
     (3) keep x11 clear of scratch around C-CALL-X11-SAVED; (4) bare `{: x:label :}`
     used directly with ADR/CBZ, never LABEL@.

   **LANDED — COMMIT A (fable-tfam12, "TFAM 10 slice 3a: native MATCH lowering +
   bad-tag die").** Native `habu2.f` only; `forth.fs` stays MATCH-undefined
   (commit B). What shipped, and the refinements made vs the design above:
   - **State (DATA cells, layout.f, reclaimed $B0..$1B0 band):** `CMBK-CELL`($B0)
     branch-kind bitstack, `CMTAG`/`CMPADS`($B8/$C0) pending variant, `CMFRD`($C8)
     match depth, `CMFR-OFF`($D0, CMFR-MAX=26 levels) fam nesting stack. All
     cleared with CMM-CELL at colon/TRUSTED: entry + EM-RESET-COMPILE-STATE.
   - **`endof` routing — implemented, contra the scout's "no routing" claim.** The
     scout was right that J-ENDOF's *codegen* is byte-identical CASE/MATCH, but the
     token machine MUST re-arm to CMM=4 after a match branch (mirrors the checker's
     MATCH-ENDOF → MM=2). A single CMM cell + a **CMBK branch-kind bitstack** does
     it correctly for ALL nesting (match-in-match, case-in-match, if-in-match):
     J-OF pushes bit 0, EM-ADT-MATCH-OF pushes bit 1, J-ENDOF pops+checks (bit 1 →
     CMM=4). This is the compiler analogue of CF-ENDOF-DISPATCH with no per-CF-frame
     kind. J-OF/J-ENDOF gained only compile-time bookkeeping; emitted CASE bytes
     unchanged (CASE fixtures + bootstrap/dispatch tests green).
   - **Reused slice-1 TFL wrappers — no type-family.f change.** `tfl-match-fam?`
     (signature scope), `tfl-cvar?` (→ tag pads ok), `tfam-name$` (die name) are all
     existing checked words; slice 3a only registers `tfl-match-fam?`/`tfam-name$`
     as C-FIND-GLOBAL bridges (LTFLMATCHFAM/LTFLNAME) + the "hb: bad "/" tag\n"
     die spans (LBADTAGPFX/LBADTAGSFX). No `TFL-MFAM?` needed.
   - **Invalid-tag path = write+exit ONLY (no pre-die drops).** `exit_group`
     terminates, so docs §16's "drop tag / drop all payload slots" is unobservable
     before the die and is elided — the compiled word stays minimal, the match-frame
     stores only fam (26 levels of headroom vs the checker's ~15-deep CF cap). Docs
     §16/§24 updated to record this.
   - **C-DIE-BAD-TAG (the one un-derisked novelty) works first try.** C-SDQ-style
     inline byte copy of "hb: bad <fam> tag\n" into the user word + a self-contained
     write(2)+exit_group(E-BAD-TAG=85) built from per-OS `SYS-EMIT-WRITE/EXIT/SVC`
     stencils baked in src/os/{macos,linux}/sys.f (the syscall reg/svc-imm are
     platform ABI, so they live there). Family NAME BYTES copied inline (no TF-STR
     pool pointer). E-BAD-TAG=85 defined in layout.f by the runtime-exit convention.
   - **Battery (test/gate-engine-lib.f GE-MATCH-EXEC, wired into GE-RUNTIME-CHECKS):**
     GE-MATCH-ROUND (construct+MATCH round-trip, zero/one/multi payload, exact
     stdout, payload cells arrive in order), GE-MATCH-NESTED (match-in-match),
     GE-MATCH-BAD-TAG (forged-tag TRUSTED ctor → child dies rc 85 + "hb: bad gemt
     tag"), GE-MATCH-BAD-VARIANT + GE-MATCH-EXPECTED-OF (named rc-70 dies), interpret
     MATCH → E-UNDEFINED. Family `gemt` = arbitrary third sum. CASE fixtures stay
     green; construct interpret pin unchanged.
   - **TRUSTED.md:** +5 manifest rows (c-die-bad-tag, em-match-semi, em-adt-match-
     fam/-var/-of); habu2.f builder-emit classification count 113→118. Size ratchet
     `test/gate-build-size.f` macOS 132343→148855 (page-granular, same delta as
     slice 3b). No new prims (census unchanged).
   - **Proofs:** byte-identical fixpoint ×2 (`install --force`,
     sha256 70b6790f… both, size 148855). FULL gate rc=0 "PASS: native test suite
     (fixpoint + engine suite + checked hb + repl + hb-build) (10376ms <= 40000ms
     budget)". Six type suites ok; maki/test.f `test: ok`; typed-local-diff-lint on
     the diff clean. Deferred to slice 3b: the `forth.fs` mirror.

   **LANDED — COMMIT B (fable-tfam12, "TFAM 10 slice 3b: Gforth mirror for
   MATCH lowering").** The stage0 mirror, per slice 2's parity discipline:
   - **bootstrap/cg/sys.fs:** `SYS-EMIT-WRITE`/`SYS-EMIT-EXIT`/`SYS-EMIT-SVC`
     stencils in BOTH target blocks (Linux x8/svc#0, macOS x16/svc#0x80),
     mirroring src/os/{linux,macos}/sys.f.
   - **bootstrap/cg/forth.fs:** E-BAD-TAG=85; CMBK/CMTAG/CMPADS/CMFRD/CMFR-OFF/
     CMFR-MAX cell constants (mirror layout.f); LTFLMATCHFAM/LTFLNAME/
     LBADTAGPFX/LBADTAGSFX label vars + kwdata rows + label inits +
     BADTAG-SFX-KW; J-MATCH (CF-ENTRY registration, `lmain LKWMATCH 5`);
     J-OF/J-ENDOF CMBK bookkeeping; C-DIE-BAD-TAG + EM-MATCH-SEMI +
     EM-ADT-MATCH-FAM/VAR/OF + the 5-state EMIT-COMPILE-ADT-MODE (all in the
     stage0 dialect: `@ BL,`/`@ ADR,` label vars, bare locals with the
     typed-local-lint allowance rows, literal msg-length MOVZ immediates,
     stage0 C-FIND-GLOBAL = plain LFIND); colon-entry + EMIT-RESET-COMPILE-STATE
     clear CMFRD/CMBK with CMM.
   - **Proof — the strongest available:** full gforth bootstrap
     (`HABU_ALLOW_BOOTSTRAP=1 GFORTH=~/.local/bin/gforth tools/bootstrap.sh`)
     rc=0 "bootstrap OK: bin/hb"; the Gforth-recovered engine is
     **BYTE-IDENTICAL to the native slice-3a fixpoint** (cmp: 0 differing
     bytes, sha256 70b6790f… both, 148855) — the stage0 mirror compiles the
     engine to the same fixpoint, which also discharges the item-title
     "Gforth-recovered candidates" parity for match/bad-tag (identical bytes ⊃
     identical behavior; round-trip + nested + forged-tag rc-85 probes re-run
     on the recovered binary directly, identical output). NOTE gforth-fast
     fails the bootstrap locals probe; use the ~/.local/bin/gforth install.
   - bootstrap-codegen-test ok; compiler-dispatch-test ok; FULL gate on the 3b
     tree rc=0 "PASS: native test suite (fixpoint + engine suite + checked hb +
     repl + hb-build) (9978ms <= 40000ms budget)"; install --force ×2
     byte-identical (70b6790f… both runs); six type suites ok; maki/test.f ok.
     Remaining for this item: slices 4 (recovery-corpus fixtures if any beyond
     the byte-identity proof), 5 (AOT/object preseeded bad-tag entry), 6 (docs
     census + pin retirement).
4. **Gforth-recovered parity.** Build via bootstrap.sh; run the slice-2/3
   fixtures on the recovered candidate; assert identical construct/match/bad-tag
   behavior. Confirm forth.fs EMIT-KWDATA + any engine-referenced J-* mirror
   compiles the engine to the byte-identical fixpoint.

   **VERDICT: DISCHARGED by the slice-3b proofs (fable-tfam12, 2026-07-09) — no
   new fixtures required.** Each acceptance clause, with evidence:
   - *Build via bootstrap.sh:* `HABU_ALLOW_BOOTSTRAP=1
     GFORTH=~/.local/bin/gforth tools/bootstrap.sh` rc=0
     "bootstrap OK: bin/hb" (run twice: once on the 3b tree, once on the final
     lint-touched tree). gforth-fast fails the locals probe — use the
     ~/.local/bin/gforth install.
   - *Byte-identical fixpoint:* recovered engine vs the native slice-3a
     fixpoint: `cmp -l` = 0 differing bytes, sha256
     70b6790feb6787f08bfee7737bd6750d3ed1db6ae3ee84042d3342b1249b4d00 both,
     size 148855 — EMIT-KWDATA + every engine-referenced J-*/EM-* mirror
     compiles the engine to the same fixpoint.
   - *Slice-2/3 fixtures on the recovered candidate:* after bootstrap.sh
     replaced bin/hb with the recovered binary, the FULL gate ran against it —
     rc=0, including GE-CONSTRUCT-EXEC and GE-MATCH-EXEC (round-trips, nested
     match, forged-tag child die, foreign-scope + interpret pins). Direct
     probes on the recovered binary: match round-trip `7 7 999`, nested
     `7 7 0`, forged tag rc=85 "hb: bad gecn tag" — identical to native.
     Byte-identity makes candidate-vs-native divergence structurally
     impossible; the runs above also prove it observationally.
   - *Optional hardening (NOT required):* a standing recovery-lane gate hook
     running GE-MATCH-EXEC on a freshly bootstrap.sh-recovered binary would
     only re-guard what GE-BUILD-FIXPOINT's fixpoint-divergence tripwire
     already fails on; it adds wall-clock cost, no new coverage. Revisit only
     if a dedicated recovery CI lane lands.
5. **AOT/object persistence of matched defs + bad-tag object entry.** PLAN item
   10 paths: lib/object*.f, aot-*.f, tools/object-image.f, driver-io.f. Add the
   test-only checked object/AOT entry that seeds raw payload+invalid-tag cells
   and calls a generated MATCH helper (NOT MAIN); thread entry identity
   (package/WID/record id + helper-root id + seeded cells + layout/test mode +
   ABI/source digest) through artifact-cache key, object schema/index/cache, and
   restore keys so a stale MAIN object can't satisfy a preseeded bad-tag run. No
   new ADT TRUST/TRUSTED:/set-check/manifest rows (may reuse image-writer trust).

   **LANDED (fable-tfam12, "TFAM 10 slice 5: AOT/object persistence of matched
   definitions"). Driver/library/tooling work only — NO engine source touched, so
   the fixpoint stays byte-identical (70b6790f…, 148855) with no gate-build-size
   ratchet and no TRUSTED.md/classes bump.** What shipped, exactly:
   - PREREQUISITE (net-new capability the plan implied): the AOT maker could not
     compile ANY `SUMTYPE` source — `BF-APPEND-COMMON` omits `src/core/include.f`,
     so the constructor eval hook `TDECL-EVAL-XT` (installed there via
     `' INCLUDE-EVALUATE TDECL-EVAL-XT !`) was 0 and every `SUMTYPE` declaration
     died rc 76 "sumtype: constructor eval hook not installed" before any matched
     def could lower. Fix: `src/habu/aot.f` installs the hook directly
     (`: AOT-CTOR-EVAL ( ptr u8 n -- ) evaluate ; ' AOT-CTOR-EVAL TDECL-EVAL-XT !`)
     at maker boot — `INCLUDE-EVALUATE` is itself just `evaluate`, and xref.f
     (in common) already installs the prot-wid hook. This makes AOT-built matched
     definitions possible at all.
   - THREE-KEY LOCKSTEP (all in ONE commit, `HBB-PRESEED-CK+` tools/hb-build-lib.f):
     folds the `preseed-entry-v1` + entry-name + seed-hex + mode axis into (1) the
     artifact key (`HBB-ARTIFACT-KEY!`), (2) the source-index key + object `source`
     header (via `HBB-CLOSURE-HEX!` → `HBB-SRC-CLOSURE-HEX`), and (3) the object
     bytes (a new `entry` schema row, below). A NO-OP for a normal MAIN build, so
     non-preseed keys/objects/executables stay byte-identical (existing caches and
     the gate's own AOT builds unaffected). Proven: a normal-MAIN build and a
     preseed build of the SAME source are distinct artifacts + distinct objects
     with bidirectional no-cross-restore (re-request normal → rc 0, re-request
     preseed → rc 85), and an object-cache relink (artifact key flipped via
     `--json`, source-index/object key identical) still serves the seeded die.
   - OBJECT SCHEMA (`lib/object.f`): new `entry` row `entry\t<name>\t<mode>\t<seedhex>`
     (PARSE-LINE arm reusing the def/reloc 3-tab + PARSE-RELOC-OFF shape; `OBJ:ENTRY+`
     emit word beside EXPORT+/DEF+, reusing LINE3N). object-link ADD-ROW/APPEND-ROW
     ignore it (unknown-tag fall-through). `HBB-BUILD-OBJECT-RECORD` now emits the
     entry-name export/def (`HBB-ENTRY-NAME$`, default "MAIN") + the entry row when
     preseeded. std.manifest + lib/object-test.f ENTRY-ROW + docs coverage added.
   - SEEDED ENTRY (`src/habu/aot-lib.f` `EMIT-SEED`/`SEED+`): after EMIT-DATA-COPY,
     pushes each seed cell onto the value stack (`movz/movk x9; str x9,[x19]; add
     x19,x19,#8`) before `bl <root>`. NOT the slice-3 emitter crash class: this
     emits into the maker's batch ASM buffer, not a live RX region. `FINDMAIN` is
     parameterized by a settable entry name (`ENTRY-NAME!`/`$`, default MAIN;
     aot-closure.f) so the stripped image starts at the selected helper (closure
     word 0 → MLBL → helper), MAIN stripped. `aot.f` reads argv[3]=entry,
     argv[4]=seed-hex (big-endian u64/cell); hb-build passes them via the maker argv.
   - CLI: `tools/hb-build.f --preseed-entry NAME --preseed-seed HEX [--preseed-mode N]`
     + in-process setters (`HBB-PRESEED-ENTRY!`/`-SEED!`/`-MODE!`) for the gate test.
   - DESIGN DECISION (macho.f/elf.f/driver-io.f image-entry): NOT changed, and this
     is correct, not a shortcut. The seeded entry is an executable STUB that the
     maker's `EMIT-ENTRY` emits FIRST (offset 0 = `CODE-OFF`) into the AOT `__text`;
     `AOT-WRITE-OBJ` writes that full code (stub + closure) as the object `text`
     section, and the object-relink path (`OBJIMG:WRITE` → `DRV-EMIT-IMAGE` →
     `BUILD-IMAGE`) copies it verbatim with the image entry already at `CODE-OFF` =
     the stub. So the seeded non-MAIN entry persists through the object cache with
     entry identity intact WITHOUT a non-CODE-OFF image entry. Seeding requires a
     stub (you cannot push value-stack cells with a static entry offset), and the
     stub is at offset 0 by construction; a non-`CODE-OFF` image entry is only
     needed for a stubless "start directly at a word" design, which cannot seed and
     is therefore not the correct design for this capability. The census's
     macho/elf requirement was predicated on that stubless framing (§ discrepancies);
     the entry-offset hardwire is not on the seeding path. Same-name hazard: the
     test helper name is unique, so name→record resolution IS record selection; the
     dict record carries no package/WID field (census § Cat 6), so name is the only
     resolvable identity and it is resolved once at closure root.
   - TEST (`test/gate-aot-positive-lib.f` GAP-PRESEED, wired into GAP-RUN, phase
     gate-aot-pos): in-process `HBB-BUILD` of a `gemt` family + MATCH helper `HLP`
     + trivial MAIN; normal build → rc 0; preseed (entry HLP, seed 0 0 5) → run
     dies rc 85 + "hb: bad gemt tag"; re-request restore → rc 85; object-cache
     relink (`--json`, asserts `HBB-OBJECT-HIT`) → rc 85; normal re-request → rc 0
     (no clobber). lib/object-test.f ENTRY-ROW proves the entry row emits + reparses
     with a stable content key.
   - Proofs: byte-identical fixpoint ×2 (install --force, 70b6790feb6787f0…,
     148855 both); FULL gate rc=0 "PASS: native test suite ... (17674ms <= 40000ms
     budget)"; six type suites ok; maki/test.f `test: ok`; typed-local-diff-lint on
     the diff clean; host-lint 0 / filemap-lint 0 / dot-dep-lint 0 findings;
     trusted-inventory baseline (gate) green + `-- strict` inventory-neutral for the
     touched files (its lone failure is the PRE-EXISTING missing dot
     `habu-tfam-12-layout-057181a9` at TRUSTED.md:797, not in this diff);
     object-image-test / object-resolve-test / object-link-test / hb-build-test ok.
6. **Docs + census + pin cleanup.** docs/type-families.md §16 lowering marked
   landed; docs/census-tfam-10.md; compiler-dispatch-test.f + bootstrap-codegen-
   test.f cover the new keywords and prove CASE shape unchanged; retire the
   fail-closed pin comments.

### Invariants to preserve (v1, do not weaken)
width-expanded-bundle scrutinee only; construct is owner-only; MATCH family is
signature-scope; no default branch; every physical slot + tag handled in every
branch and in the invalid-tag fallback.

Slices 2/3 are the "very large" core and are TIGHTLY COUPLED to the pin flip
(cannot reserve tokens without lowering or GE-CONSTRUCT-PENDING goes red), so
they are not further splittable. STOPPED CLEANLY after this audit per the
single-phase rule; next agent starts slice 1.
