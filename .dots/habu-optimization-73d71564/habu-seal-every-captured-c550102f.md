---
title: "Seal every captured package against reopening"
status: closed
priority: 2
issue-type: task
created-at: "2026-09-16T16:49:49.727440+03:00"
closed-at: "2026-10-02T14:50:00.000000+02:00"
close-reason: "Every package the native capture ships is sealed at PREPARE-TARGET (uonquqxy bbcac900, Seal every package the native capture ships): 214 baked namespace rows, public and private wordlists protected, 0 open; reopening or defining into a baked package exits 84 by name on product and app images; whitebox stays open. Engine 2,427,511 B unchanged. Fable review ACCEPT."
---

Problem: only the seven RESTAB names (`habu2.f:2153`, mirrored by `checker.f` CHECKER-SEALED-PKG?) and 173 self-protected wordlists are sealed; user source can reopen any other engine package with `package NAME`, and that reach is the only reason a private word of an engine package needs its checker data.
Acceptance:
- **Seal at capture:** every package the capture window ships gets both wordlists `prot-wid-add`ed before `NATIVE-RUNTIME:CAPTURE-PREPARE` (`native-runtime.f`, run by `PREPARE-TARGET` in `tools/native-build-core.f`) reaches `CHECKER-CAPTURE-PREPARE`, except on the whitebox image (`ACAP-WHITEBOX?`). The 974304d0 sweep reads the protected bit there, so sealing inside `ACAP-PWIN-CAPTURE` is too late. Assert `prot-wid-room` first (the bitmap bounds 8,192 ids; the engine uses 347).
- **Refuse:** the engine guard (`habu2.f C-PACKAGE-PROT-GUARD`) refuses the reopen at load and names the package. The checker adds no package refusal: engine-source certification and `tools/check.f` replay packages under mirror authority (`CHECKER-VERIFY-PKG-DEPTH` 1), and `prefix-src` re-declares all 72 core-prefix packages, so a `CHECKER-PACKAGE` refusal would reject the engine's own source. Check-time refusal by name comes from 974304d0: a private word of a sealed package is E-UNDEFINED.
- **Application packages:** a `--repl` snapshot keeps them reopenable.
- **Docs:** `docs/forth.md` Packages and `docs/forth-card.md` state the rule.
Files: `src/habu/aot-capture.f`, `docs/forth.md`, `docs/forth-card.md`, new `test/package-seal.f` (forked subjects, as in `test/internal-word-gate.f`).
Verify:
- On the product engine: `package XREF` is refused at load (rc 84, the name printed), and `tools/check.f --json-errors` reports a private XREF word as E-UNDEFINED, located.
- SUITE build-fixpoint-source green on the sealed product.
- `package MYAPP` opened twice succeeds.
- After `tools/hb-build.f -- --repl`, the application package reopens and XREF does not.
- On the whitebox engine, `package XREF` succeeds.
- `test/run.f`, generations byte-identical, and Etch's tests on the candidate.
Depends: habu-give-every-baked-9ca94f18; lands after the reopen-name fix (its checker hunk at 9218-9240 is adjacent).
Parent: habu-ship-only-the-d7d38629. Design: the Fable surface design of 2026-09-30 (~/.cache/tmp/heron-arm64/design-surface.md); census: ~/.cache/tmp/heron-arm64/size-census/.

## Plan (2026-10-01, Fable; lead amendments marked LEAD)

The acceptance above is corrected by the last subsection here. Lands after habu-give-every-baked-9ca94f18 and the B10a cast rule (habu-turn-deliberate-cast-ad2e237d), whose private CAST: carries the driver's retype.

Line numbers are master 0e7365b3-era; find words by name.

### Decision: the seal moment
Package mark = the native build driver's `PREPARE-TARGET` (tools/native-build-core.f ~302-304), calling into the target image before `NATIVE-RUNTIME:CAPTURE-PREPARE`. Authority `SEAL` (dot fab55650 design B) stays in `IMK-PASS`'s sealed arm. The two moments share the verdict cell `IMK-CLASS`, not one call.
- IMK-PASS cannot host the mark: native-runtime.f requires internal-mark.f (IMK-PASS runs at its load) before checker-surface.f, the native compiler, repl rows, repl.f, top-row.f and the NATIVE-RUNTIME/CHECKER-REG reopens. The product has 203 namespace rows below its seal floor (114 already self-protected; max wid 343 vs PROT-WID-MAX 8192); those loaded after internal-mark.f exist only at capture.
- Not inside NATIVE-RUNTIME:CAPTURE-PREPARE: APP-IMAGE:SAVE (app-image-core.f ~57-62) calls it for --repl/app snapshots, whose packages must stay reopenable; in the build host it would also seal the host's and driver's packages (CORE-PREFIX:FIRST-RECORD returns the earliest marker).
- The driver supplies the floor: AOT-ARM:R0 (aot-arm.f ~51,130) = ndict@ latched at window open; ACAP-PWIN-CAPTURE (aot-capture.f ~2401-2415) keeps window-wid bits only and dies 74 if WID 0 is marked, so a private wid of 0 is skipped.
- Not SEAL-CAPTURE / CHECKER-REG:SEAL: they run after PREPARE-TARGET, so CHECKER-SWEEP:SWEEP would not see the bits.
- Correction to fab55650's sentence "the seal reuses SEAL's moment": SEAL attaches to IMK-PASS (sealed arm, after `IMAGE-SEALED IMK-CLASS !`); SEAL-PACKAGES reads the same IMK-CLASS at capture. Nothing here moves when B8 lands.

### Design
src/core/internal-mark.f, public section after IMAGE-CLASS:
```forth
\ Seal every package the capture ships (dot habu-seal-every-captured-c550102f):
\ both wordlists of every namespace row at or above `first` take the protected
\ bit, so `package NAME` and a definition into either wordlist exit
\ ENGINE-ERROR:SEAL-PACKAGE and the capture keeps none of their private symbols
\ (src/core/checker-surface.f KEEP?). IMK-PASS cannot do this: it runs when this
\ file loads, before the compiler, the REPL and the manifest's own packages
\ exist, so the driver calls this at its capture with the window's first record
\ (tools/native-build-core.f PREPARE-TARGET), before NATIVE-RUNTIME:CAPTURE-PREPARE
\ sweeps. An application image (APP-IMAGE:SAVE) never calls it. The whitebox
\ image stands down here as IMK-PASS did, on the verdict it wrote.
: SEAL-PACKAGES ( n -- ) {: first:n :}
   IMK-CLASS @ IMAGE-WHITEBOX = IF EXIT THEN
   ndict@ first ?do
      i XREF-REC {: rec:ptr :}
      rec XREF-WORDLIST DICT-WL:NAMESPACE = IF
         rec XREF-PKG-PUBLIC prot-wid-add
         rec XREF-PKG-PRIVATE dup 0= IF drop ELSE prot-wid-add THEN
      THEN
   loop ;
```
(XREF-* xref.f ~44-80; ndict@ checked; prot-wid-add EPRIM PE-N PE-IN, idempotent; DICT-WL:NAMESPACE.)

tools/native-build-core.f PREPARE-TARGET:
```forth
: PREPARE-TARGET ( -- )
   AOT-ARM:R0 @ s" ENGINE-INTERNAL:SEAL-PACKAGES" TARGET-XT SEAL-XT execute
   s" NATIVE-RUNTIME:CAPTURE-PREPARE" TARGET-XT PREPARE-XT execute
   AOT-ARM:HERE-N AOT-ARM:D1 ! ;
```
LEAD: SEAL-XT ( n -- [ n -- ] ) is a private `CAST:` row with a one-line reason (B10a's rule, habu-turn-deliberate-cast-ad2e237d), never a new TRUSTED: (the TRUSTED: retirement is in progress). PREPARE-XT stays as it is; B6 converts it.
TARGET-XT refuses an xt outside the window's code.

Data flow: CHECKER-CAPTURE-PREPARE -> CHECKER-SWEEP:SWEEP -> checker-surface.f KEEP? false for a private whose package public wid is protected (XREF-WID-PROTECTED?) -> symbol retired; aot-capture STRIP; ACAP-PWIN-CAPTURE writes window-relative bits; boot restores (habu2.f LAOTPROT). Runtime refusals are the existing guards (habu2.f C-PACKAGE-PROT-GUARD/C-PACKAGE-SEAL-GUARD, packages.f PKG-SEALED?/PKG-SEAL-GUARD/PKG-OPEN-WID, publish into protected wid). No change to checker.f, habu2.f, outer.f, packages.f.

### Steps (one worker-max commit)
1. Baseline: `bin/hb --load tools/engine-size.f -- bin/hb`; in-process count of namespace rows below SEAL-NDICT@ with public/private protected bits.
2. internal-mark.f: SEAL-PACKAGES; one sentence in the whitebox comment block (~149-180) that the package seal reads the same verdict at capture.
3. native-build-core.f: private CAST: SEAL-XT beside PREPARE-XT; new first line of PREPARE-TARGET.
4. Build and measure; whitebox image (HABU_WHITEBOX_IMAGE=1 / `whitebox` arg as test/whitebox-engine.f does): IMAGE-CLASS prints 1 and `package TOP-ROW ;package` loads there. Multi-generation convergence.
5. New test/package-seal.f, SUITE row `package-seal` in test/gate-stdlib-cases.f after `SUITE checker-surface`. SUBJECT:RUN forks as test/checker-surface.f LOAD does; expected rc ENGINE-ERROR:SEAL-PACKAGE (84). Cases: engine under test is sealed (IMAGE-CLASS = IMAGE-SEALED); `package TOP-ROW ;package` -> 84, stderr names TOP-ROW; same for NATIVE-RUNTIME (both red before); `: TOP-ROW:ZZ ( -- ) ;` -> 84, `cannot publish into protected word` and `TOP-ROW:ZZ`; user package MINE reopened twice -> rc 0, `2`; publics resolve qualified and via `using`; structural: every namespace row below SEAL-NDICT@ has public protected and private zero-or-protected (count printed).
6. test/checker-surface.f SECTION-PRIVATE: "a private of an unsealed package keeps its symbol (CHECKER-REG CHECKED-ROW TTRUE)" flips to TFALSE under "a private of every baked package keeps no symbol"; reword header bullets. SECTION-APP-IMAGE stays the pin that application packages remain reopenable.
7. test/whitebox-engine-suite.f: case "an engine package reopens on this image" (`package TOP-ROW ;package 7 . cr` -> `7`) via the CLASS-OF$ pattern.
8. Delete the owners lane's stand-in test/baked-owner-seal.f, its require/SEAL-BAKED line in test/baked-owner-child.f and the two STAGE-HEAD append lines in test/baked-owner.f; the fixture then proves the product's own seal.
9. Docs: docs/forth.md multi-file reopen paragraph (~167-171): a package the engine bakes is sealed at capture; `package NAME` or a definition into its wordlists exits 84 naming it; use publics qualified or `using`; the whitebox image alone keeps them open; an application image keeps its own packages reopenable. forth.md (~1352-1357) "`private` is a convention until the package seals itself" + "or until the capture does". forth-card.md section 7 multi-file bullet: "an engine package is sealed: reopening one exits 84". docs/engine-size.md: measured section (bytes before/after, sha256, symbol/bit counts).
10. Gates: focused rows first (package-seal, checker-surface, baked-owner, internal-word-gate, friend-arena-seal, whitebox-engine, type-export, memory, aot-registry-identity, aot-payload-graph, aot-payload-constructor, native-window-owner), then the full land.sh gate.

### Dot acceptance corrections (lead amends the dot)
1. The mark is the engine's protected-WID bit on both wordlists (PROT-BITS-OFF), captured window-relative and restored at boot; the checker reads it at capture through checker-surface.f KEEP?. CHECKER-SEALED-PKG? stays (RESTAB seven: no tick, no NAME:tail definition, no EXPORT); generalising it would refuse `' PKG:WORD` and EXPORT for all 203 packages and flip test/type-export-suite.f on the whitebox.
2. Refusal is the engine's: `package NAME` exits 84 with the token; a definition into either wordlist exits 84 "hb: cannot publish into protected word: NAME:X". E-EXPORT-SEALED stays the RESTAB seven's.
3. At the driver's PREPARE-TARGET, before NATIVE-RUNTIME:CAPTURE-PREPARE (SEAL-CAPTURE runs after the sweep).
4. Every package the capture window ships (namespace rows at index >= AOT-ARM:R0); host/driver packages are not marked.
5. checker.f and habu2.f are not touched.

### Downstream
Etch test/hook-count.f:15 reopens IMAGE-LIFECYCLE: use IMAGE-LIFECYCLE:COUNT ( -- n ). Tender, Loom, Maki, Kiba: 0 reopen sites.

### Risks
Marking WID 0 kills the capture (the 0= guard is load-bearing). Order inside PREPARE-TARGET is load-bearing (engine-size is the check). Private records the capture keeps by name lose their checker symbol (113 packages already sealed since 956e7b19e3; full suite bounds it). tools/check.f on a source reopening a baked package gets exit 84, not a diagnostic. Cold/recovery engines never run the mark (no Gforth check needed: no seed edit). x86-64 cross build uses the same driver; bit restore in kernel-x64.f not exercised.
