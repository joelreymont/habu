---
title: "Visibility discharge: 548 shims are not a typing problem"
status: open
priority: 2
issue-type: task
created-at: "2026-08-19T10:05:19.371299+02:00"
---

Phase 1 of habu-trusted-dies-prim-4fd12d60, the highest-leverage cut: 548 of 1,373 TRUSTED: sites (census 2026-08-19) are single-call shims onto words that are ALREADY checked : definitions - the checker knows their effects; internal-mark.f seals the name or the pre-hook prefix never recorded a usig at the call site (internal-mark.f:26 states it). Includes 79 of the 107 sites in the decl machinery (enum-decl 41/44, structure-decl 38/40, generated-declaration 11/23, structure-make 6/8) and ~395 of test/'s 826. Build ONE capability: publish an already-known signature to a named consumer package (friend-import / signature re-export). PROBE FIRST what internal-mark/prot-wid/friend arenas already provide - the capability may be an extension, not a new mechanism. Then sweep the 548 mechanically. Blocks the final TRUSTED: deletion.

Claim: unassigned (RELEASED 2026-08-22: workspace gone, no live lane - the 2026-08-21 gc keyed on dot-id workspaces and missed the shared habu-thecut/habu-trusted names)

PREMISE FALSIFIED, RE-SCOPED (2026-08-19, trusted-1's measured probe through
install --force): of 595 single-call shim sites tree-wide, only 110 target a
name with a checker-recorded effect (50 targets, all PRIM-axiom'd); 485 target
names the checker holds NOTHING for, because the type foundation loads before
the check hook (habu2.f:861-890) - a RECORDING gap, not a seal gap.
internal-mark.f seals bare interpret/tick only; it never blocked a compiled
reference. Evidence: ED-PROBE-A (call SCHEMA-CON from enum-decl.f) dies rc 70
E-UNDEFINED at prefix time, before any seal exists; TRUSTED: at the owner does
not register either; EXPORT correctly refuses a no-effect source (7115).
LANDED: the 10 axiom-target forwarders (enum-decl 44->39, structure-decl
40->35), call sites bound to the checker's own rows - a wrong caller now dies
rc 70 (measured). REMAINING: ~100 mechanical sites (axiom-carrying targets,
MINUS deliberate retypes - a shim over @/drop/patch32/ffi-call restates a type
on purpose and must not be swept); 485 sites BLOCKED on route 3 (the
post-hook move - its own dot). RULING: route 1 (owner-side declared-signature
recorder) REJECTED - one-owner trust is still trust and route 3 deletes it;
route 2 (mass axioms) REJECTED - contradicts the epic. The instrument for any
future sweep: EFFECT-QUERY (checker.f:7015) + EFFECT-DIN-N/DOUT-N - ask the
checker, never classify by source shape.

## Leaves (2026-10-01 plan; census in the parent)

Measured: the rows of FRESH, CHECK-CANDIDATE-START, SCHEMA-REG:SCHEMA-CON, LOWER-CERT:ARENAS-STALE, USIGS, XREF-REC exist (EFFECT-QUERY true on the product) but carry no external authority: this is an authority gap, not a recording gap. Whitebox `: X1 ( -- n ) FRESH ;` is refused E-CAP-TRUSTED; the product answers E-UNDEFINED (sealed DNAME-INT).
test, 654 non-evaluate sites: single-call shims 315, raw-memory 96, multi-token 73, empty body 49, stack-op 26, literal 13, catch 7, trust-decl 3, execute 3. By class: (d) 63, trust-decl/`[ ]` 5 (go with 42b30edd), (b) ~90, (c) ~300, multi-token whitebox readers ~195 (decided per site by the probe). 84 registered files probed (~/.cache/tmp/heron-arm64/trusted/), 51 unregistered fixtures not probed.

B8 (Fable design, then worker-max; lands after habu-share-reopen-name-92885254 and the seal c550102f): the whitebox image binds the real recorded row of an internal word, so a whitebox body is checked against the row instead of asserted. Candidate from the plan: IMK-PASS's IMAGE-WHITEBOX arm sets a CHECKER-EFFECT-AUTHORITY cell that ENFORCED? reads; product unchanged. To verify in design: the window fixtures re-include checker.f from source, so their pre-hook rows may be absent rather than non-external. Designed together with 41e973ce's B7 (src shims onto pre-hook registry words). Fixture: test/internal-word-gate.f keeps the product refusal; a whitebox fixture pins `: X ( -- n ) FRESH ;` certifying.
B9a-d (worker each, after B8): per site `TRUSTED:` -> `:`; a product SUITE file that then fails E-UNDEFINED moves to WHITEBOX-SUITE; E-MISMATCH sites are casts and wait for B10; (d) sites wait for B3/B5. 9a type-family/type-decl/type-ctor/type-family-rollback/type-export suites and checker-scan-index-lib.f with its suites (~230); 9b engine-suite.f, engine-writers.f, effect-intern-suite.f, effect-read-api-test.f, checker-effect-authority.f, checker-rollback-sig-pool.f, catch-stale-suite.f, checker-verify-order.f, decl-event-suite.f, decl-replay-verify-source.f (~170); 9c structure-decl/structure-make/enum-decl/structure-certify suites, layout-*.f, registry-persist.f, rigid-region-suite.f (~90); 9d the 13 window fixtures and the aot-payload/registry children (~80). Acceptance per file: its row green on its engine, T-REPORT counts unchanged, rg count equals the residual (d)+(b).

## Internal-word authority design (2026-10-01, Fable plan; B8 here, B7 in habu-sweep-trusted-out-41e973ce)

Implementation steps 1-4 wait for stage 2 of habu-share-reopen-name-92885254 and the seal habu-seal-every-captured-c550102f; SEAL stays in IMK-PASS's sealed arm; the seal (c550102f) marks packages at the driver's PREPARE-TARGET and reads the same IMK-CLASS verdict, so the two share the verdict, not one call.

Probes: /private/tmp/claude-501/-Users-joel-Work-habu/84130b4f-736a-404a-bf0f-b2271b2ea704/scratchpad (p1-p5.f, w1.f, *-census.out).

### Measured
1. Product `: X1 ( -- n ) FRESH ;` → E-UNDEFINED rc 70; whitebox → E-CAP-TRUSTED. Same for SCHEMA-REG:SCHEMA-CON. EFFECT-QUERY -1 on both, CHECKER-RESOLVES? 0: rows exist without EFFECT-EXTERNAL ($8, checker.f:10590); refusal is DO-TOK-BODY's authority test (checker.f:12050-12058).
2. All 66 distinct targets of the single-call shims in the five decl files have rows on the whitebox (12 global, 8 SCHEMA-REG, 39 TFAM, 7 TYPE-DECL); 122/124 sampled test/tool shim targets have rows (2 misses are evaluate bodies). No src/compiler, src/habu, src/os shim targets a pre-hook file.
3. Window fixtures: tier 0 (`<wb> --load test/native-window-owner-child.f -- w1.f`) has a RECORDING gap (EFFECT-QUERY 0 for SCHEMA-CON and FRESH → E-UNDEFINED); tier 1 (aot-mode.f first) has an AUTHORITY gap (E-CAP-TRUSTED). Same recording gap on every from-source prefix boot (hb-stage/hb-stdin lineage, cold hosts; docs/bootstrap.md:22,38-40).
4. Cause: at tier 0 only TRUSTED: publishes text with no hook (habu2.f:9199-9202; EM-COMPILE-PUBLISH-HOOKED nohook branch 9225-9231 records nothing). Tier 1 scans unjudged (compiler.f:234-239 CHECK-PARENT → CHECKER-OWNER:CHECK-UNJUDGED); success leaves a non-external row (checker.f:17273 CHECK-UNJUDGED!); a failed/uncheckable scan leaves none.
5. test/checker-effect-authority.f (WHITEBOX-SUITE, gate-stdlib-cases.f:465) asserts ENFORCED? TTRUE four times; flipping ENFORCED? would break it and flip CERTIFIED? (checker.f:848, 10428, 10577). So the dot's candidate (ENFORCED? reads a whitebox cell) is rejected.

### Decision: one mechanism, two halves; registry files stay pre-hook
B. The authority gate is a seal state. `package CHECKER-EFFECT-AUTHORITY` (checker.f:824) gains `variable SEALED` (0 open), public `SEALED? ( -- bool )`, `SEAL ( -- )` with `PPRIM: CHECKER-EFFECT-AUTHORITY SEAL PPRIM;` beside it (precedent `PPRIM: CHECKER-BOUND REWIND PPRIM;`, checker.f:8853-8860). DO-TOK-BODY: after the RECOVERY-ROW? branch (12054-12057), before the PRIM-FIRST-IDX 0= CAPREQ branch:
```
   CHECKER-EFFECT-AUTHORITY:SEALED? 0= IF FEP @ EFF-APPLY EXIT THEN
```
PRIM-TRUSTED-SYM? branch (12048) stays first (class d stays refused). ENFORCED?, CERTIFIED?, CHECKER-RESOLVES? (11270), EFFECT-EXTERNAL-MIN-IN (11344), EXPORT (11653) unchanged. IMK-PASS (internal-mark.f:202-208) sealed arm calls SEAL; whitebox arm returns before it (gate stays open, baked like IMK-CLASS). Every population-2 file loads before internal-mark.f (native-build-core.f:221-243, native-runtime.f:85-108). Product unchanged: sealed before user source; 974304d0 drops internal rows; a surviving non-external row is still E-CAP-TRUSTED. test/internal-word-gate.f keeps pinning that.
A. Tier 0 records what tier 1 records for a hook-less definition.
- A1 (engine): habu2.f nohook arm (9225-9231): when TRUSTED-CELL is 0 and TSIG-U-CELL non-zero, call the owner's CHECK-UNJUDGED-OFF field (CHECKER-OWNER-ABI $1A8; layout.f:1074 DECL-CHECK-UNJUDGED-OFF; target owner if published else source, as checker-owner.f:124-126) with BODYBUF/BODYLEN, through C-CALL-X11-SAVED, drop the verdict. Seed mirror required at bootstrap/cg/forth.fs:3968 (C-CALL-CHECK-DEFINER reads HOOK-CELL).
- A2 (checker): CHECK-UNJUDGED! (checker.f:17273) keeps the declared signature as a non-external row whatever the verdict. Rule: a hook-less definition's row is its declaration, recorded without authority, on both tiers.
Residual: 8 checker.f words defined before its owner publication (17823-17846) get no tier-0 row: ARENA-BYTES-GROW, CON-OF, REG-GROW1, MULTI-ERR?, FIELD-PROJ!, FIELD-PROJ-CLEAR, EFFECT-EXTERNAL-MIN-IN, CTOR-PEND-CLEAR (~12 src sites). Ladder: move below the owner publication if checker.f never calls it (EFFECT-EXTERNAL-MIN-IN); else an existing post-owner public with the same meaning; else a PRIM:/PPRIM: axiom beside the word (docs/bootstrap.md:38-40 remedy).
Registry files post-hook: rejected (they are the checker's foundation; checker.f's own words stay pre-hook anyway).
Shims: delete, not `:`, when the caller can name the target (enum-decl.f: FAM-DECL x8 → TFAM-DECL; CON-CODE → CON-OF; ED-SCH-CON → SCHEMA-CON; PKG-PUBLIC → CHECKER-VIS-PUBLIC). Raw-cell shims (`PEND-A @ PEND-U @`, `SUMV-N @`, `REG-PROT-N @`) are class (e): the owner declares an accessor. Row differs from target → cast (B10). A test shim inside a reopened engine package that reaches a private stays as `:` (bridge).

### Steps (one commit each; all after stage 2 (92885254) and the seal c550102f land)
1. Gate (B): checker.f SEALED/SEALED?/SEAL+PPRIM, DO-TOK-BODY branch; internal-mark.f sealed arm calls SEAL. Fixtures: checker-effect-authority.f WHITEBOX case `: X ( -- n ) FRESH ;` certifies, FRESH CHECKER-RESOLVES? still false, ENFORCED? still true, rejected `: Y ( -- ) FRESH ;` E-MISMATCH; internal-word-gate.f unchanged. Proof: native build fixpoint; whitebox-engine-suite, checker-effect-authority, internal-word-gate, native-window-owner rows.
2. Tier-0 recording (A2 then A1, seed mirror). Fixture: window fixture under test/native-window-owner.f at both tiers (ARGS! and TIER1-ARGS!, :41-60) asserting SCHEMA-CON EFFECT-QUERY true and `: W ( n -- n ) SCHEMA-REG:SCHEMA-CON ;` verdict 0, plus a wrong-signature E-MISMATCH arm. Proof: fixpoint, test/run.f, the 12 `0 set-check` suites (engine-suite.f, seal.f, tier.f, build-rewind-test.f, …), cold-host fixtures, tools/bootstrap.sh check-only; measure cold prefix boot time before/after.
3. Residual axioms/moves for the 8 words; extend the step-2 fixture with CON-OF.
4. Docs: forth.md "Checker & type model" rule (open gate = unsealed image binds the real recorded row; closed = product); bootstrap.md near :38-40 tier-0 recording rule; gate.md WHITEBOX-SUITE paragraph; card section 9 row at B12.
B9a-c after step 1; B9d after step 2; B7 after step 3. Per site: delete the shim, load on its engine, read the refusal: E-UNDEFINED → fix at owner; E-MISMATCH → B10; E-CAP-TRUSTED → B3/B5. Product SUITE turning E-UNDEFINED moves to WHITEBOX-SUITE.

### Interactions
- Stage 2 (wwyswovn): checker.f hunks at 12083+/12107+ near DO-TOK-BODY; re-run step-1 fixtures on the rebased tree. habu2.f and forth.fs hunks do not touch the publish tail.
- Seal c550102f: its mark runs at the driver's PREPARE-TARGET and stands down on the whitebox verdict IMK-PASS writes; SEAL stays in IMK-PASS's sealed arm (the seal's plan shows IMK-PASS runs before the compiler and REPL packages exist, and cold hosts never capture).
Risks: tier-0 scans change `0 set-check` behaviour (run those suites); from-source engines answer E-CAP-TRUSTED instead of E-UNDEFINED for sealed pre-hook names with rows (bootstrap check-only; grep check suites for E-UNDEFINED pins); declared rows bind only in unsealed images; boot-time cost (measure).
Not run: a full native build (window inferred from the tier-1 child probe).

## Qualified B8 step 1

The integrated open-image authority gate, failed-row export/copy/transfer refusals and checked-tick correction are qualified on `cf7d6dc3`: full native 600/600, rc 0; official generations 2–5 have identical engines and names. Independent Fable and Astra feature reviews and the final first-failure Astra follow-up pass. Evidence is under `~/.cache/tmp/b8af/` and `dave-tick-abi-review-result.txt`. The product remains sealed; unsealed images use the actual recorded row, and failed declaration rows are confined to their own run. This completes step 1; tier-0 recording and subsequent sweeps remain open.

## Outcome (master d273e641)

Landed; every id below is an ancestor of master.
- B8 step 1, the open-image authority gate: 0455469b "Bind internal rows in unsealed images". The P2 fix 08131803 "Keep recovery rows out of copies", with its review follow-ups 5b4dd582 "Correct the capture rule and EXPORT refusals" and 6aed7ae6 "Refuse recovery rows at owner transfer". Qualified together as release b7f63f25. TRUSTED: 1216 -> 1216 and trusted-only rows under src 53 -> 53 (08131803, 6aed7ae6; 0455469b records no count).
- B8 steps 2-4 (tier-0 recording of hook-less rows, the pre-claim words' rows with CON-OF ( ptr u8 n -- n ), the docs) and B9d (the window and AOT fixtures' shims): 7bcc8270, a source carry of the lane stack b6103a52, a6746673, e8dec73c, d550fc78, da069d05, 0c7b339d and 4b754333. None of those lane commits is an ancestor of master. The carry renumbered the owner fields to $390..$3A8 (checker-owner-abi.f DECLARED-ROW-OFF..WRITE-WINDOW-OFF). 7bcc8270's description is empty. The lane descriptions record TRUSTED: 868 -> 795 (B9d removes 73) and trusted-only 53 -> 53; 7bcc8270's diff removes 75 TRUSTED: lines and adds 2, all under test/. On master: checker.f CHECKER-DECLARED-ROW!, CHECKER-ROWS-END, CHECKER-RETRACT-ROWS, CK-DECLARED-LOG-DRAIN and CON-OF; the step-4 text in docs/forth.md, docs/gate.md and docs/bootstrap.md.
- B9 stack, the suites' shims checked on the unsealed engine: 9285d418 (B9a1, type-family suites), 26debbea (B9a2, type-decl and scan-index), 10baeb79 (B9b first half, effect and decl-event suites), bea8a267 (B9c, structure and layout) and 3f5e2054 (the eval follow-up, evaluate-closed), carried as 9684573b. The descriptions record 355 -> 36 TRUSTED: in the stack's suite files: B9a1 97 -> 0, B9a2 138 -> 43, B9b first half 61 -> 7 and B9c 59 -> 4, then 22 -> 4 at the evaluate sites.
- Evaluate sweep: 63c90bd1 "Evaluate the remaining test texts closed", TRUSTED: 46 -> 27 in its 15 files, with the review's VALUE finding folded in.

Remaining with this dot:
- The eight sites whose targets step 3 gave rows are still TRUSTED:. In test/type-ctor-suite.f: CON-CODE, PEND-CLEAR, CAND-START and CAND-DONE (907-913). In test/aot-payload-graph-child.f: BYTES (30) and SAVE (111). In test/aot-registry-identity-child.f: RI-COPY-A (95) and RI-MIXED-STATE (192). The session permission check refused converting them to `:`, so they wait for Joel.
- test/ holds 328 TRUSTED: definitions at d273e641 (`rg -c '^\s*TRUSTED:' -g '*.f' test`).
Owned elsewhere:
- The kept (d) sites (CERT-SIZE, DICT-MIN, UNCHECKED+/-) and step 4's forth-card.md section 9 row go with B12 (habu-delete-the-trusted-42b30edd).
- TWX-XPG-CHECK (type-ctor-suite.f:702) is a cast for habu-turn-deliberate-cast-ad2e237d.
