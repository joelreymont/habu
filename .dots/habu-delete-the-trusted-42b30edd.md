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

Problem: after the sweeps, the TRUSTED: definer, PE-TRUSTED-ONLY / PRIM-TRUSTED-ONLY! / PE-TRUSTED-ONLY? in src/core/checker.f, the E-CAP-TRUSTED code, and the docs/forth.md passages that list TRUSTED: among ordinary definers (lines ~76, ~325, ~2358, ~2436) still exist, so the idiom can come back. Acceptance: `TRUSTED:` is an undefined word in the engine (a regression asserts E-UNDEFINED); the flag words and error code are gone or repurposed to the owner-bound capability with one name; docs/forth.md names PRIM axioms inside the owning package as the only foreign boundary and never TRUSTED:; docs/debugging.md gains nothing but a dated note; `rg -i "trusted:" --glob "*.f" --glob "*.md" .` returns nothing; engine byte fixpoint; full gate green. Files: src/core/checker.f, the definer site (engine or checker; locate), docs/forth.md. Verify: rg; regression; the engine byte fixpoint; bin/hb --load test/run.f. Depends: the three sweep dots, aspen FFI sweeps, and habu-honour-owner-private-0a19f45d (the seal honours owner-private rows, so the transitional global trusted-only rows can go). Mechanism decision (hazel, 2026-09-16, for Joel to confirm or veto): retire PE-TRUSTED-ONLY altogether instead of replacing it with an owner-bound gate (habu-pkg-owned-prim-e08e345f, an agent proposal); every primitive is a typed axiom and every caller is checked, as docs/forth.md already requires; the six code-injection primitives become PRIVATE to the packages that own the publisher (package privacy is the existing location-bound mechanism) and ffi-call-bounded becomes private to package FFI behind the typed FFI declarer aspen builds. Rationale: no new checker mechanism, one rule, and the protection the bit gave (only an audited body may inject code) is kept by privacy rather than by an unchecked-body definer. Ownership: checker. Claim: unassigned. Parent: habu-trusted-dies-prim-4fd12d60.

## Deletion list (B12, last)

Each site is named by symbol with its line at fc8fa94a; after drift, `rg -n` the symbol.

Engine, src/habu/habu2.f. TRUSTED-CELL is accessed in:
- C-CLEAR-TRUSTED-STATE (1012; store at 1019)
- DEF-OPEN (1462; store at 1489)
- EM-ABORT-COMPILE-STATE (2352; store at 2366)

src/habu/interpreter.f:
- TRUSTED-CELL in DEF-HEAD (1660; store at 1670) and DEF-PREFLIGHT (1782).
- The TRUSTED: writer boundaries: DEF-TRUST-SIG (1289), DEF-POSTPONED-CLEAR, DEF-POSTPONED-CUT and DEF-POSTPONED-ADD (1292-1294), DEF-PREFIX-BIND (1898) and DEF-XT! (2379).

Checker, src/core/checker.f:
- PE-TRUSTED-ONLY (7216), PE-TRUSTED-ONLY? (11888) and PRIM-TRUSTED-ONLY! (11947).
- The use of PRIM-TRUSTED-ONLY! in PRIM-CLOSE (26588).
- The verifier-window comment (13346).

src/habu/prims.f: FL-TRUSTED-ONLY (88), ETRUSTED-ONLY! (237), TRUSTED-ONLY? (317) and the 45 ETRUSTED-ONLY! row marks (366-1026).

src/core/render.f: the trusted_boundary_required text in REPAIR-CLASS (1569; 1578).

Docs: `rg -n -i trusted` finds 27 lines in docs/forth.md and 8 in docs/forth-card.md.

Counts at fc8fa94a:
- TRUSTED: definitions in .f files under src, tools and test: 64 (src 37, of them src/lib 13; tools 0; test 27).
- Trusted-only rows: 45, all in prims.f.

### Also part of the definer, outside the list above

A census of TRUSTED-named words and `trusted:` keyword handling over src finds more sites that read the cell or the keyword. The deletion must account for these too.
- TRUSTED-CELL's own claim: src/habu/layout.f:316 ($27B8) and the src/habu/data-claims.f claim row 114.
- Native-compiler readers: src/compiler/native/definition.f TRUSTED? (121); dict.f VISIBLE-RECORD? (43) and INT-CALL? (321).
- x86-64: src/habu/kernel-x64.f DEF-OPEN-BODY (4789), DEF-CLOSE-BODY (5097) and DEF-ABORT-BODY (5119).
- The keyword:
  - interpreter.f DEF-NAME (1411) and DEFINE? (2451; `trusted:` at 2454);
  - verify-source.f TRUSTED-REACH (1381), TRUSTED-CLAUSE (1386), TICK-NATIVE-DEFINER? (1949), TRUSTED-DOES (2191), TRUSTED-CALL (2195), SCAN-TRUSTED-BODY (2211), TRUSTED-DEFINITION (2238; `trusted:` at 3770) and RECORD-DEFINER? (3696).
- A generator: src/core/sumtype.f TDINIT-NAME-START (2022) emits `TRUSTED: ` definitions at run time.
- The trusted tick:
  - checker.f PRIM-TRUSTED-SYM? (12001) and CHECKER-TRUSTED-TICK? (13843);
  - checker-owner-abi.f TRUSTED-TICK-OFF ($258, 86);
  - layout.f DECL-TRUSTED-TICK-OFF (682), read by habu1.f C-TICK-QUERY (3647) and interpreter.f TRUSTED-TICK? (695).
- Diagnostics: render.f E-CAP-TRUSTED texts in DCODE (1543) and DIAG-PROSE (1751).

The 2026-10-01 census also counted 5 trust-decl and 4 `[`/`]` test sites that exercise TRUST-DECL itself; they go here.

The mechanism decision recorded above stands: PE-TRUSTED-ONLY is retired, privacy bounds the code-injection primitives.

## Scope: seal the checker-owner writers

When TRUSTED-CELL goes, B12 must seal, as one mechanism, every product-reachable word that writes checker state or cuts its rows. Today such a word adds nothing a TRUSTED: body cannot already do. Without TRUSTED: it is a hole.

The installed hb resolves each of these from a file (`' NAME drop` run as `hb --load probe.f`: rc 0):
- The public CHECKER-OWNER writers in src/compiler/native/checker-owner.f: TAPE-INSTALL (190), TAPE-ARM (195), TAPE-DISARM (200), TAPE-ADVANCE (205), DOES-FINISH (237), DOES-BEGIN (242), DOES-COMMIT (247), ROWS-END (317), DECLARED-EFFECT (432) and WIDE-PUBLISH (564).
- RETRACT-ROWS (322), the successor of RETRACT-OWN-ROW. It takes a raw store offset, so a top-level caller cuts at any offset.
- CHECKER-USIGS-TRUNCATE-FROM-RAW (src/core/checker.f:13591), a prefix word that truncates any row from top level.
- More checker.f writers: CHECKER-USIGS-TRUNCATE-FROM (14009), CHECKER-SCOPE-START (24659), CHECKER-SCOPE-START-NEUTRAL (24666), CHECKER-SCOPE-FINALIZE (24673), CHECKER-SCOPE-DONE (24676), REC-WIDE-PUBLISH (10920) and REC-MIN-IN@ (10931).
- CHECKER-OWNER:WRITE-WINDOW (checker-owner.f:330) and CHECKER-OWNER:BIND-REGIME! (353): a checked callback can shut the write window or change the binding regime.
- The declaration-transaction publics: every public word of DECL-EVENT (src/core/decl-event.f:553-591, OPEN through REQUIRE-POINTEES), TYPE-FIELD-OWNER:OPEN, COMMIT, ADD, FINALIZE and ROLLBACK (src/core/type-family.f), and GENERATED-DECL-CTOR:ARM (src/core/generated-declaration.f:434). They write no checker store directly; their store writes reach the guarded appenders. Seal them with the rest or show each write path is guarded.

USIG-TRUNCATE, which the original ruling named, and CHECKER-DECLARED-ROW! no longer exist (E-UNDEFINED). CHECKER-RETRACT-ROWS ticks at rc 0.

The owner record is raw-readable too. Any package can read a CHECKER-OWNER field and execute it through a cast, so the record itself must be sealed.

Done when the same probe on the product answers rc 70 for each word above.

Acceptance case from the stripped scan (habu-stop-the-stripped-e1bf9fca, 2026-11-07): a stripped build value-checks only declared cells and reads no undeclared window as cells, so an address laid in undeclared data reaches the image unchecked. `TRUSTED:` lays one where the same checked body is refused (`' dup ,` and `here ,` are E-MISMATCH; `' dup B 3 + !` is E-RAW-CELL-PTR): with `create B 16 allot`, a `TRUSTED:` word storing `['] dup` or a MEM-ALLOC-BYTES pointer at `B 3 +`, or `['] dup` at `B 8 +`, and `0 c, ['] dup ,` inside a `TRUSTED:` word load (rc 0 on the installed hb; `: C4 ( -- ) 0 c, ['] dup , ;` is E-MISMATCH at `,`) and, since the scan change, build. Once `TRUSTED:` is gone none of them loads in any form. The other routes close elsewhere: `ptr-field` over a parameter in habu-refuse-ptr-field-331a9731, a `0 set-check` window in habu-remove-the-checker-4425bcef.

Acceptance case (C2c2 fold-3 review, 2026-11-07): a withheld TRUSTED: colon word keeps an unmarked declared row, so its caller stops a multi-error load instead of being reported undefined. `MULTI-ERR-BEGIN` / `: TBAD2 ( n -- n ) drop ;` / `TRUSTED: TF ( n -- n ) TBAD2 ;` / `: TFU ( n -- n ) TF ;` / `MULTI-ERR-END` ends with `ncomp: cannot compile TFU at TF` and `E-UNDEFINED: TF`, rc 70, on the installed hb. Root: checker.f TRUST-DECL? registers the row without the recovery mark a certified head's publication writes (CHECKER-PUBLISH-PARSED → RECOVERY-RECORD). Once TRUSTED: is gone, the same program reads on as it does today with a checked colon word TF and `MULTI-ERR-END .`: count 1, rc 0.

Acceptance case added 2026-11-10 (bob): a TRUSTED: body with `|` or `--` in a locals group loads with no message: `TRUSTED: TL ( n -- n ) {: a | b :} a ;` is rc 0 on the installed hb, and `5 TL` then stops with E-UNDERFLOW (interpret stack underdepth: TL). TRUSTED: is machinery being replaced, so it is not patched: once the definer is gone the same body under `:` must report E-BAD-LOCAL-SHAPE at the group's token, as `:` does today.

Acceptance case added 2026-11-10 (bob; was habu-a-trusted-definer-7d6ee093): a TRUSTED: definer head with a bad stored signature is refused on the clause's line. `TRUSTED: MK1 ( n -- nosuch:fam )` / `create ,` / `does> ( -- n )` / `@ ;` prints `ncomp: cannot compile MK1 at nosuch:fam in …/v1.f:3`; the same program under `:` names line 1. Cause: src/compiler/native/definition.f DECLARE-ROW (:210), which only a TRUSTED: definition runs, so the refusal is pinned in the clause's text. Not patched: once the definer is gone the colon form is the only one, and it must keep naming the head's line.

Acceptance case added 2026-11-10 (bob; was habu-check-f-json-ea3b54a5): `bin/hb --load tools/check.f -- --json-errors` on a TRUSTED: definer whose clause is refused places the record at 1:18, and on a TRUSTED: body with an open `if` writes no record, only prose on stdout; the same programs under `:` give 4:3 and 2:10. Cause: the pre-verifier's SCAN-TRUSTED-BODY (src/habu/verify-source.f) judges no body token and publishes the clause unchecked, so the refusal comes from the run stage, which has no source map; SETTLE-UNIT's MALFORMED? branch (definition.f) writes prose only. A fix would add an owner-ABI entry for a clause checked from source: not added. Once the definer is gone both programs exist only under `:` and must keep the controls' records.

Acceptance case 2026-11-10 (review of habu-an-uncheckable-token-fc4ec7cb): a file holding a TRUSTED: definer's clause alone, which only TRUSTED: reaches, gets rc 0 under --verify-only while its load refuses with E-MISMATCH at IF; src/core/checker.f DO-TOK1 (:22182, through DO-TOK-TAIL :22156) puts UNCK back after an unjudged token but not UNSEEN. With TRUSTED: deleted the path is gone: check that no checked form reaches it.

Acceptance cases 2026-11-10 (TRUSTED: is machinery being replaced, so these are not patched): (1) the check tool places the tokens of a TRUSTED: definer's clause inside the engine's copied text: 'TRUSTED: TK ( n -- ) drop does> ( -- ) cr ;' puts cr at 1/10, not 1/39. (2) A TRUSTED: body the unjudged scan cannot settle is refused as a bare 'uncaught throw code -8519' with no token. (3) After fix one of habu-a-stack-word-5dd345f9 a TRUSTED: body whose stack word reaches under its declared inputs through a multi-cell value compiles at its raw effect or is refused: 'TRUSTED: TW ( tp -- tp ) 2>r 2r> ;' printed 4 3 1 0 on master and is refused after it (the lead's runs).

Acceptance case, read not run: in a multi-error load a REFUSE-DEF throw inside a TRUSTED: body becomes verdict 0 in CHECK-DEF-THREW, so the body is published, where a plain load abandons it. Deleting TRUSTED: removes the path; until then no fix is made in it.

That case now runs (2026-11-10): inside a multi-error load a TRUSTED: body refused by REFUSE-DEF (a `using` shadow of `over`) prints its packet, counts 1 and is published; a plain load of the same body stops with rc 70, 'ncomp: cannot compile TSH', uncaught 7141. Both engines answer the same.

Acceptance case 2026-11-10 (bob; merged from habu-keep-the-declaration-16209954 "Keep the declaration frame API out of user source", one cause with this dot): TYPE-FIELD-OWNER:OPEN and DECL-EVENT:OPEN run from a top-level user file (rc 0 on the installed hb). Their only production caller is DEV-OPEN inside a checker frame; a user-driven field rollback outside a family frame leaves field names in TF-STR (pftx commit, type-family.f frame comment). Make the frame API private to the core (whitebox tests keep it), per the privacy rule: private sections plus the build strip.

Acceptance case 2026-11-11 (carl-hcdd fix zkmnmrls f455cb19, dot 6dd65911): the pre-pass (src/habu/verify-source.f VERIFY-DOES) still records a TRUSTED: create-less does> word as a definer, where the load runs it as a patch of the last created word; deleting TRUSTED: removes the path, so no fix is made in it.

Acceptance cases (2026-11-12, from habu-a-foreign-pkg-a9c9e60d's probes): a foreign package's public `TRUSTED: FAKE ( n -- Q:tok ) ;` forges a linear token and a TRUSTED: word erases or retypes one with no memory and no cast; the tree does it at src/lib/xml/c2.f:118 and :121 (package XML-C2 mints and erases XML:reader, declared in src/lib/xml/state.f:11-13), and tender src/answers.f:54-56 is the mint, state and consume trio docs/forth.md says never to write. Each stops loading when TRUSTED: goes.

Acceptance case 2026-11-12 (jerry; from habu-print-the-one-ea184772's census): SEAL-CAPTURE (src/habu/habu1.f BSEALCAP :3743) exits 73 writing 'hb: undrained pre-trust defer: <name>' per entry, off the shape `<class> habu: <text>[ <16 hex>] lr <16 hex>`. Pre-trust defers belong to TRUSTED:; accepted when they go with it.
