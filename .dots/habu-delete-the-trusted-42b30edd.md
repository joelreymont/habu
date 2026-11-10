---
title: "Delete the TRUSTED: definer and its flag"
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T15:49:57.940329+03:00"
---

Order: retain this project and its partial work, but resume it only after
incremental builds and test-suite work. Joel confirmed that sequence during
the unfinished-work interview. No build-speed benefit is claimed.

Problem: after the sweeps, the TRUSTED: definer, PE-TRUSTED-ONLY / PRIM-TRUSTED-ONLY! / PE-TRUSTED-ONLY? in src/core/checker.f, the E-CAP-TRUSTED code, the Gforth seed mirror of the definer if any, and the three docs/forth.md passages that list TRUSTED: among ordinary definers (lines ~90, ~98, ~297) still exist, so the idiom can come back. The inventory file TRUSTED.md and tools/trusted-inventory.f were already deleted on 2026-08-05 (b9f3edff). Acceptance: `TRUSTED:` is an undefined word in the engine and the seed (a regression asserts E-UNDEFINED); the flag words and error code are gone or repurposed to the owner-bound capability with one name; docs/forth.md names PRIM axioms inside the owning package as the only foreign boundary and never TRUSTED:; docs/debugging.md gains nothing but a dated note; `rg -i "trusted:" --glob "*.f" --glob "*.fs" --glob "*.md" .` returns only the archive and DECISIONS history; engine byte fixpoint; stage0 chain bootstrap check OK; full gate green. Files: src/core/checker.f, the definer site (engine or checker; locate), bootstrap/cg/forth.fs, src/lib/errors.f, docs/forth.md. Verify: rg; regression; tools/native-build.f fixpoint; tools/bootstrap.sh; bin/hb --load test/run.f. Depends: the three sweep dots, aspen FFI sweeps, and habu-honour-owner-private-0a19f45d (the seal honours owner-private rows, so the transitional global trusted-only rows can go). Mechanism decision (hazel, 2026-09-16, for Joel to confirm or veto): retire PE-TRUSTED-ONLY altogether instead of replacing it with an owner-bound gate (habu-pkg-owned-prim-e08e345f, an agent proposal); every primitive is a typed axiom and every caller is checked, as docs/forth.md already requires; the six code-injection primitives become PRIVATE to the packages that own the publisher (package privacy is the existing location-bound mechanism) and ffi-call-bounded becomes private to package FFI behind the typed FFI declarer aspen builds. Rationale: no new checker mechanism, one rule, and the protection the bit gave (only an audited body may inject code) is kept by privacy rather than by an unchecked-body definer. Ownership: checker and seed. Claim: unassigned. Parent: habu-trusted-dies-prim-4fd12d60.

## Deletion list (master d273e641; B12, last)

Each site is named by symbol with its line at d273e641; after drift, `rg -n` the symbol.

Engine, src/habu/habu2.f. TRUSTED-CELL is accessed in:
- C-CLEAR-TRUSTED-STATE (3949; store at 3955)
- DEF-OPEN (4411)
- C-TRUSTED (4878; 4906)
- EM-STARTUP-RUNTIME-STATE (8939)
- C-CALL-COMPILE-IMMEDIATE (9044)
- EM-COMPILE-PUBLISH-TRUSTED (11125; 11136)
- EM-COMPILE-PUBLISH-HOOKED (11206)
- C-COMPILE-CALL-GUARD (11448; 11452)
- EM-RESET-COMPILE-STATE (11596)
C-TRUSTED-TICK? (5713) asks the checker, through the owner's TRUSTED-TICK-OFF, whether a ticked word is trusted-only.

src/habu/definers.f:
- TRUSTED-CELL in DEF-HEAD (243) and DEF-PREFLIGHT (298).
- The TRUSTED: writer boundaries: DEF-OPEN, DEF-APPEND, DEF-TRUST-SIG, DEF-CREATED-SIG and DEF-CLOSE (37-41), DEF-PREFLIGHT (293), DEF-RUN (303) and DEF-COMPILE (376).

Seed:
- bootstrap/src/defining.fs `: TRUSTED:` (38).
- bootstrap/cg/forth.fs: the TRUSTED-CELL constant (286); LKWTRUSTED (690, 4111, 8539); C-CLEAR-TRUSTED-STATE (6549); C-TRUSTED (7624); EMIT-COMPILE-PUBLISH-TRUSTED (7712); the C-COMPILE-CALL-GUARD mirror in EMIT-COMPILE-CALL (7973-7989).

Checker, src/core/checker.f:
- PE-TRUSTED-ONLY (9629), PE-TRUSTED-ONLY? (9685) and PRIM-TRUSTED-ONLY! (9703).
- The use of PRIM-TRUSTED-ONLY! in PE-SPEC-ROW (10038).
- Its rows (10120-10147): CHECKER-VERIFY-PKG-START, CHECKER-VERIFY-PKG-DONE, CHECKER-RESET-SOURCE, CHECKER-TRUSTED-TICK?, FIELD-PROJ!, FIELD-PROJ-A, FIELD-PROJ-U, FIELD-PROJ-FID and FIELD-PROJ-OFF. Also CHECK-DOES! (10269), LOWER-CERT:BYTES (10455) and LOWER-CERT:FOR-HASH (10457).
- The comment at 11731.

src/habu/prims.f: FL-TRUSTED-ONLY (108), ETRUSTED-ONLY! (256), TRUSTED-ONLY? (307) and the 44 ETRUSTED-ONLY! row marks (346-928).

src/core/render.f: the trusted_boundary_required texts in REPAIR-CLASS (910-911).

Docs: `rg -n -i trusted` finds 46 lines in docs/forth.md and 4 in docs/forth-card.md (the section 9 row among them).

Counts at d273e641:
- TRUSTED: definitions in .f files under src, lib, tools and test: 687 (src 138, lib 155, tools 66, test 328). tools/bootstrap.sh holds 3 more.
- Trusted-only rows: 56 (prims.f 44, checker.f 12).

### Also part of the definer, outside the list above

A census of TRUSTED-named words and `trusted:` keyword handling over src, bootstrap and src/lib/errors.f at d273e641 finds more sites that read the cell or the keyword. The deletion must account for these too.
- TRUSTED-CELL's own claim: src/habu/layout.f:636 ($27B8), src/habu/data-claims.f claim rows 137 and 297, and bootstrap/cg/data-claims.fs:112.
- Tier-1 readers: src/compiler/native/compiler.f TRUSTED? (179); dict.f VISIBLE-RECORD? (48) and INT-CALL? (277); elaborate.f TRUSTED-FRAME-RESHAPE (1025) and DO-EVAL (3396).
- x86-64: src/habu/kernel-x64.f DEF-OPEN-BODY (3522) and DEF-CLOSE-BODY (3585).
- The keyword:
  - habu1.f LKWTRUSTED (255);
  - habu2.f KWTRUSTED$ (2305), with LKWTRUSTED at 2420, 4890, 10086 and 12918;
  - definers.f DEF-NAME (103) and DEFINE? (407);
  - outer.f TRUSTED-TICK? (741);
  - verify-source.f TRUSTED-REACH (908), TRUSTED-CLAUSE (913), TICK-NATIVE-DEFINER? (1452), TRUSTED-DOES (1591), TRUSTED-CALL (1598), SCAN-TRUSTED-BODY (1624), TRUSTED-DEFINITION (1646) and RECORD-DEFINER? (2391).
- A generator: src/core/sumtype.f TDINIT-NAME-START (2237) emits `TRUSTED: ` definitions at run time.
- The trusted tick:
  - checker.f PRIM-TRUSTED-SYM? (9750) and CHECKER-TRUSTED-TICK? (12375);
  - checker-owner-abi.f TRUSTED-TICK-OFF ($2C8, 105);
  - layout.f DECL-TRUSTED-TICK-OFF (1199).
- Diagnostics: render.f E-CAP-TRUSTED texts in DCODE (886) and DIAG-PROSE (1057).

The 2026-10-01 census also counted 5 trust-decl and 4 `[`/`]` test sites that exercise TRUST-DECL itself; they go here.

The mechanism decision recorded above stands: PE-TRUSTED-ONLY is retired, privacy bounds the code-injection primitives.

## Scope: seal the checker-owner writers

When TRUSTED-CELL goes, B12 must seal, as one mechanism, every product-reachable word that writes checker state or cuts its rows. Today such a word adds nothing a TRUSTED: body cannot already do. Without TRUSTED: it is a hole.

A product engine resolves each of these from a file at d273e641 (`' NAME drop` run as `bin/hb probe.f` with the d31d4395 engine: rc 0):
- The public CHECKER-OWNER writers in src/compiler/native/checker-owner.f: REPORT (161), TAPE-INSTALL (166), TAPE-ARM (171), TAPE-DISARM (176), TAPE-ADVANCE (181), DOES-FINISH (192), DOES-BEGIN (197), DOES-COMMIT (202), ROWS-END (248), DECLARED-EFFECT (345), DECLARED-ROW (352) and WIDE-PUBLISH (480).
- RETRACT-ROWS (253), the successor of RETRACT-OWN-ROW. It takes a raw store offset, so a top-level caller cuts at any offset.
- ENGINE-INTERNAL:SEAL-PACKAGES (src/core/internal-mark.f:238), which tools/native-build-core.f:312 calls at capture.
- CHECKER-USIGS-TRUNCATE-FROM-RAW (src/core/checker.f:12541), a prefix word with a PRIM: row that truncates any row from top level.
- More checker.f writers with product rows: CHECKER-USIGS-TRUNCATE-FROM (PRIM: row 10123), CHECKER-SCOPE-START, CHECKER-SCOPE-START-NEUTRAL, CHECKER-SCOPE-DONE and CHECKER-SCOPE-FINALIZE (PRIM: rows from 10077), REC-WIDE-PUBLISH (8840) and REC-MIN-IN@ (8854).
- CHECKER-OWNER:WRITE-WINDOW (checker-owner.f:261) and CHECKER-OWNER:BIND-REGIME! (284): a checked callback can shut the write window or change the binding regime.
- The declaration-transaction publics: every public word of DECL-EVENT (src/core/decl-event.f:793-837, OPEN through IDENTITY), TYPE-FIELD-OWNER:OPEN, COMMIT, ADD, FINALIZE and ROLLBACK (src/core/type-family.f), and GENERATED-DECL-CTOR:ARM (src/core/generated-declaration.f). They write no checker store directly; their store writes reach the guarded appenders. Seal them with the rest or show each write path is guarded.

USIG-TRUNCATE, which the original ruling named, no longer exists (E-UNDEFINED). As controls, CHECKER-RETRACT-ROWS and CHECKER-DECLARED-ROW! already answer "hb: internal engine word", rc 70.

The owner record is raw-readable too. Any package can read a CHECKER-OWNER field and execute it through a cast. tools/native-unit-build-core.f does this in UNIT-CHECKER-MARK (41-43), UNIT-CHECKER$ and UNIT-CHECKER-IMPORT, through RESET-XT (tools/native-build-core.f:93), UNIT-EXPORT-XT and UNIT-IMPORT-XT (native-unit-build-core.f:44-45). These casts are TRUSTED: today; without TRUSTED: they become private CAST:s, so the record itself must be sealed.

REG-PROTECT cannot seal these words. IMK-PASS (internal-mark.f:211) ends the registrations with REG-PROT-RETIRE when internal-mark.f loads. src/habu/native-runtime.f loads compiler.f after that (lines 111 and 114), so a REG-PROTECT in checker-owner.f is never applied.

Done when the same probe on the product answers rc 70 for each word above.

Acceptance case from the stripped scan (habu-stop-the-stripped-e1bf9fca, 2026-11-07): a stripped build value-checks only declared cells and reads no undeclared window as cells (DECISIONS "typed data"), so an address laid in undeclared data reaches the image unchecked. `TRUSTED:` lays one where the same checked body is refused (`' dup ,` and `here ,` are E-MISMATCH; `' dup B 3 + !` is E-RAW-CELL-PTR): with `create B 16 allot`, a `TRUSTED:` word storing `['] dup` or a MEM-ALLOC-BYTES pointer at `B 3 +`, or `['] dup` at `B 8 +`, and `0 c, ['] dup ,` inside a `TRUSTED:` word load today and, since the scan change, build (kevin's census, /tmp/kevin-aotscan-census.md). Once `TRUSTED:` is gone none of them loads in any form. The other routes close elsewhere: `ptr-field` over a parameter in habu-refuse-ptr-field-331a9731, a `0 set-check` window in habu-remove-the-checker-4425bcef.

Acceptance case (C2c2 fold-3 review, 2026-11-07): a withheld TRUSTED: colon word keeps an unmarked declared row, so its caller stops a multi-error load instead of being reported undefined. `MULTI-ERR-BEGIN` / `: TBAD2 ( n -- n ) drop ;` / `TRUSTED: TF ( n -- n ) TBAD2 ;` / `: TFU ( n -- n ) TF ;` / `MULTI-ERR-END` ends with `ncomp: cannot compile TFU at TF` and `E-UNDEFINED: TF`, rc 70 (tier 1 rc 67), on the installed hb (master) and every C2c2 candidate. Root: checker.f TRUST-DECL? registers the row without the recovery mark a certified head's publication writes (CHECKER-PUBLISH-PARSED → RECOVERY-RECORD). Once TRUSTED: is gone, the same program written with a checked colon word reads on: count 2, TF and TFU unpublished.

Acceptance case added 2026-11-10 (bob): after habu-a-locals-group-0c34ca08, a TRUSTED: body with `|` or `--` in a locals group is refused as `E-UNDEFINED: |` with no reason (the base bound `|` silently). TRUSTED: is machinery being replaced, so it is not patched: once the definer is gone the same body under `:` must report E-BAD-LOCAL-SHAPE at the group's token.

Acceptance case added 2026-11-10 (bob; was habu-a-trusted-definer-7d6ee093): a TRUSTED: definer head with a bad stored signature is refused on the clause's line. `TRUSTED: MK1 ( n -- nosuch:fam )` / `create ,` / `does> ( -- n )` / `@ ;` prints `ncomp: cannot compile MK1 at nosuch:fam in …/v1.f:3` (cases /Users/joel/Work/reviews/habu-10x-2026-11-10/cases8/ncomp/a v1.f, v3.f); the same program under `:` names line 1. Cause: definition.f DECLARE-ROW, which only a TRUSTED: definition runs, comes after CHECK-DOES-SPLIT, so the refusal is pinned in the clause's text. Not patched: once the definer is gone the colon form is the only one, and it must keep naming the head's line.

Acceptance case added 2026-11-10 (bob; was habu-check-f-json-ea3b54a5, hunt report /Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/checkf-hunt.md): `bin/hb --load tools/check.f -- --json-errors` on a TRUSTED: definer whose clause is refused places the record at 1:18 (t9), and on a TRUSTED: body with an open `if` writes no record, only prose on stdout (t7); the same programs under `:` give 4:3 and 2:10 (c9, c7; files in .jj-ws.noindex/bob-checkf/.out/checkf at the time). Cause: the pre-verifier's SCAN-TRUSTED-BODY (src/habu/verify-source.f) judges no body token and publishes the clause unchecked, so the refusal comes from the run stage, which has no source map; SETTLE-UNIT's MALFORMED? branch (definition.f) writes prose only. A fix would add an owner-ABI entry for a clause checked from source: not added. Once the definer is gone both programs exist only under `:` and must keep the controls' records.

Acceptance case 2026-11-10 (review of habu-an-uncheckable-token-fc4ec7cb, /Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/unseen-review.md claim 4): /Users/joel/Work/reviews/habu-10x-2026-11-10/cases8/unseen-review/cases/t1b-trusted-clause-alone.f, which only TRUSTED: reaches, gets rc 0 under --verify-only while its load refuses with E-MISMATCH at IF; src/core/checker.f DO-TOK1 (:24124-24125) puts UNCK back after an unjudged token but not UNSEEN. With TRUSTED: deleted the path is gone: check that no checked form reaches it.

Acceptance cases 2026-11-10 (TRUSTED: is machinery being replaced, so these are not patched): (1) the check tool places the tokens of a TRUSTED: definer's clause inside the engine's copied text: 'TRUSTED: TK ( n -- ) drop does> ( -- ) cr ;' puts cr at 1/10, not 1/39 (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/emptyclause-hunt.md, emptyclause-fix.md). (2) A TRUSTED: body the unjudged scan cannot settle is refused as a bare 'uncaught throw code -8519' with no token (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/transport-review.md finding 1). (3) After fix one of habu-a-stack-word-5dd345f9 a TRUSTED: body whose stack word reaches under its declared inputs through a multi-cell value compiles at its raw effect or is refused: 'TRUSTED: TW ( tp -- tp ) 2>r 2r> ;' printed 4 3 1 0 on master and is refused after it (the lead's runs; /Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/rstack-review.md).

Acceptance case, read not run (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/multi-error-hunt.md, 'Also noted'): in a multi-error load a REFUSE-DEF throw inside a TRUSTED: body becomes verdict 0 in CHECK-DEF-THREW, so the body is published, where a plain load abandons it. Deleting TRUSTED: removes the path; until then no fix is made in it.
