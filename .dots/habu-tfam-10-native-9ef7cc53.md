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
2. **Construct lowering (native).** Compiler `construct` mode (FORK 2): capture
   family+variant, resolve via slice-1 surface, emit (M-p)×push0 + push tag on
   the VS. Flip GE-CONSTRUCT-PENDING construct legs from E-UNDEFINED pins to
   execution round-trips (build a def with `construct`, run it, inspect the
   physical slot0..tag). Keep interpret-mode construct fail-closed OR lower it too
   per docs (decide + record). One-payload + zero-payload + wide + an arbitrary
   third family (not result/option/color).
3. **MATCH/OF/ENDOF/;MATCH lowering (native).** MATCH mode + frame; peek tag,
   compare/branch chain per variant, branch prologue (drop tag+pads, expose
   payload), ENDOF jump-to-join, ;MATCH join + invalid-tag die (FORK 3). Reuse
   the checker's CF-ENDOF-DISPATCH discipline: OF/ENDOF codegen must route by the
   compiler frame kind. Runtime round-trips execute every branch; bad-tag DIES
   with the named diagnostic (child-process fixture — a die exits the engine).
4. **Gforth-recovered parity.** Build via bootstrap.sh; run the slice-2/3
   fixtures on the recovered candidate; assert identical construct/match/bad-tag
   behavior. Confirm forth.fs EMIT-KWDATA + any engine-referenced J-* mirror
   compiles the engine to the byte-identical fixpoint.
5. **AOT/object persistence of matched defs + bad-tag object entry.** PLAN item
   10 paths: lib/object*.f, aot-*.f, tools/object-image.f, driver-io.f. Add the
   test-only checked object/AOT entry that seeds raw payload+invalid-tag cells
   and calls a generated MATCH helper (NOT MAIN); thread entry identity
   (package/WID/record id + helper-root id + seeded cells + layout/test mode +
   ABI/source digest) through artifact-cache key, object schema/index/cache, and
   restore keys so a stale MAIN object can't satisfy a preseeded bad-tag run. No
   new ADT TRUST/TRUSTED:/set-check/manifest rows (may reuse image-writer trust).
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
