---
title: Turn deliberate cast sites into declared casts
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T16:57:00.773557+03:00"
---

Problem: 99 TRUSTED: sites under test/ state an effect their body does not have on purpose (a body `;` declared ( n -- ptr u8 ), a body `0` declared ( -- fresh-region-a-a ), a body `n` declared ( -- [ n -- n ] )): they are unchecked casts, not helpers, and a sweep cannot convert them (sweep lane, 2026-09-16). The tree has a declared cast form (CAST: in lib/string.f, docs/forth.md), audited by name. Acceptance: every cast site becomes a CAST: declaration (or the CAST: form is extended to the shapes the tests need, with the checker rule stated), each with a one-line reason; sites that turn out to be lies about a working word become checked definitions; per-suite case counts unchanged; the count of unchecked casts is reported per file. Files: the test files the sweep lane listed, src/core/checker.f if CAST: needs a shape, docs/forth.md. Verify: the affected suites; test/run.f. Depends: none. Ownership: test tree. Claim: unassigned. Parent: habu-trusted-dies-prim-4fd12d60.

## Design facts and leaves (2026-10-01 plan)

CAST: today (checker.f:11645-11657, docs/forth.md:516-523, 709-712, 757): one input and one output term (E-CAST-ARITY), both single retype-eligible machine cells (E-CAST-CLASS refuses a pointer operand), no linear content (E-CAST-LINEAR), family output only in its owner (E-CAST-OWNER). So `( n -- ptr u8 )`, `( n -- [ -- n ] )`, `( -- matrix<...> ) 0`, `( ptr a -- JR:reader )` are not expressible: the extension branch of this dot's acceptance is required for most cast sites (src 58 incl. checker-owner.f AS-* x19 and checker.f ARENA-RC>PTR; tools xt/pointer casts; test ~90 incl. rigid-region and engine-suite phantom makers). Linear tokens (EDIT:editor, JR:reader, XML:source, own, mtok) stay E-CAST-LINEAR by design: those sites are mint/state/consume leaves of their library, redesigned per library, not swept.
B10a (Fable design, then worker-max): CAST: admits a `ptr t` and a typed-quotation destination, with the checker rule in docs/forth.md and fixtures for each admitted and refused shape.
B10b (worker-light, after B10a; workspace trusted-casts): convert the test casts, then the tools/lib pointer casts left by B1/B2; B6 in 41e973ce converts src.

## CAST: extension design (2026-10-01, Fable plan)

Measured on bin/hb of .jj-ws/shrink-owner (master 448ae659 tree; the plan did not see the dot's 2026-10-01 appendix).

### Rule (docs sentence)
A cast term is one machine cell: a con, a width-1 family, a pointer, or a quotation. A pointer or quotation destination is a class mint and is declared only in a package's private section. A family named in an introduction position of the destination (the term itself, an output pointer's pointee, an output quotation's produced rows, recursively) belongs to its declaring package. An unresolved type variable or linear type anywhere in either term is refused.

Admitted: `ptr t` output (t con or owned family), private only (precedent `PPRIM: FFI FFI-CELL>PTR … CLOSE-PRIVATE`, checker.f:8775-8789, docs/forth.md:18-26); `[ in -- out ]` output, private only; projections `ptr t -- n`, `[ … ] -- n`, `ptr fam -- ptr n` unrestricted (docs/forth.md:523); width-1 families and roles unchanged.
Refused: multi-arity (E-CAST-ARITY); `ptr a` / `[ -- a ]` → E-CAST-LINEAR (test/nominal-pointer.f PASS shows `( n -- ptr a )` would forge any nominal; effects.md:426-432); linear in pointee/quotation rows → E-CAST-LINEAR; owned family in an output pointee/quotation row outside its owner → E-CAST-OWNER; class mint outside a private section → new E-CAST-SCOPE 7140 (7132-7139 taken); atoms, W>1 → E-CAST-CLASS. `NULL-PTR BYTE-VIEW +` is a closed forgery (effects.md:426-436), not a typed source.

### Populations
Disappear (typed source exists, measured):
- `ptr u8|n -- n` projections → `NULL-PTR BYTE-VIEW -` (prims.f:354, effects.md:532): byte-edit.f:28, xml/scalar.f:33, native-source-view.f:199, native-build-core.f:102, checker-owner-guard.f:10, native/string.f:71 PTR>N, aot-capture.f:2466, test proc-maps.f:14, does-clause-record.f:41, native-string.f:135. Not for `ptr fam` (test/layout-defer.f:75 DTK-ADDR stays a projection cast).
- `ptr n -- ptr u8` → BYTE-VIEW (hide.f:31, aot-capture.f:40); `ptr u8 -- ptr n` → CELL-VIEW; `ptr n -- ptr ptr n` → `0 ptr-field` (lib/task.f:177).
- `( -- ptr u8 ) 0` (engine-suite.f:933), `NULL$` (env-base.f:69) → NULL-PTR.
- Role fixtures engine-suite.f:926-932 → `>IMG`, `>ASM`, `ASM>N`, `>SNAP` (src/core/roles.f:91-133); T->NODE (1672) → DEFTYPE `>NODE`; T-GROW-PAIR (875) → `2drop`; T-PHASE-ID/T-CODESIG2 identity bodies.
- XT>N/XT0>N (lit-emit-size-test.f:64-65) → `0 search-wl` (prims.f:688); bootstrap-wide-memory-src.f:79 BWM-XT likewise.
- Unmake projections (typed-storage-test.f:54-55, layout-buffer.f:34, bootstrap-wide-memory-src.f:77-78, type-layout-lower-pending.f:59,62) → MATCH-based checked definitions (inferred, not measured).
Become `defer` declarations (abstract fixture rows, never executed): rigid-region-suite.f:61-89 (14), engine-suite.f:722 T-V14, :941-947 T-PTX-*, T-MK-SPAN, T-MK-SPAN=, :1626-1627 T-BIG6-MK/T-SCQ-MK, checker-model-cases.f:92-94. Measured verdicts match today's suites. Keep above the `0 set-check` window (rigid-region-suite.f:44-55). Not measured: defer over TR-TFAM-REG-registered families.
Become private CAST: rows:
- `n -- ptr u8|n`: lib/memory.f:85, aio.f:310, aio-macos.f:72, checker.f:173/2871/5261/5748 (ARENA becomes `ptr u8`), image-bytes.f:27, debug.f:55, hide.f:30,32, xref.f:25,27, address-cells.f:64, aot-capture.f:41, icode.f:115 (`ptr n`), os/*/layout.f (3), tools native-source-view.f:200, native-unit-capture.f:37, lib/task.f:175, tests addrmap-call.f:12, p2-map-rewind.f:27, does-clause-record.f:40, native-string.f:136, x86-64-kernel-prof.f:470, engine-suite.f:2763, xt-cell-band-bad.f:34, stripped-image-subject.f:33.
- `n -- [ … ]`: checker-owner.f:71-89 (19), aot-arm.f:78-81, packages.f:93-94, verify-source.f:305-308,1159, snapshot-format.f:29, address-cells.f:34, compiler.f:259, dict.f:116, aot-capture.f:2450 (move to private), check-core.f:1129, native-build-core.f:90-91,171,294-295,358, native-unit-build-core.f:45-46,118-119, tests native-run-fixture.f:87-92, native-source-view-child.f:55-57, native-window-owner-*.f, address-cell-owner.f:4, native-string.f:267, aot-named-cells-init.f:5. (A FIELD of quotation type is E-TDECL-SYNTAX, forth-card.md:65-67, so per-field casts are the minimum.)
- `ptr n -- ptr [ -- ]`: task.f:182, address-cell-index.f:27, address-cell-tasks-subject.f:31.
- `ptr fam -- ptr n`: layout-valid-*-bad.f (3).
- `[ … ] -- n`: aot-prefix-literal-consumer.f:6-7.
- ACAP-RANGE-XT (aot-capture.f:2451): checked definition over two private casts.
Global-scope sites (checker.f x4, hide.f, xref.f x2, debug.f, image-bytes.f, icode.f, os layout x3, memory.f:85) need a package private section around the cast; callers use `PKG:NAME`.
Out of scope: linear mint/state/consume leaves; aot-payload-native-producer.f:52 ASSERTED.

### Checker change (src/core/checker.f; no engine, seed or native-compiler change)
- :11654 `7140 constant E-CAST-SCOPE`.
- Replace CAST-CELL? (:16567) with CAST-TERM?: con true; var true (linearity refuses it); T-PARAM `T-WIDTH 1 =`; T-PTR recurse pointee; T-QUOT true; else false.
- CAST-MINT? ( n -- bool ): T-PTR or T-QUOT.
- CAST-MAY-LINEAR? (:16589): T-PTR recurse; T-QUOT walk din/dout/rin/rout; T-VAR true.
- CAST-OWNER? (:16579) → CAST-INTRO-OWNED?: T-PARAM existing test; T-PTR recurse; T-QUOT recurse dout/rout; con/var true.
- CAST-CERTIFY (:16607): FAM, ARITY, CLASS, LINEAR (both, TWALK-RESET each), OWNER (output), then mint outside CHECKER-PACKAGE-PRIVATE → E-CAST-SCOPE (mode :1113, constants :855-857).
- Comments: checker.f:2262, :8287-8300, lib/ffi-abi.f:77-78, lib/task.f:389.
- Docs: forth.md:516-523 rule + codes 7130/7137/7135/7140; :709-712 extend; :757-759 drop "refuses a pointer operand"; Rules learned by refusal bullet (~:1250); card §5 line: "An integer becomes an address or an xt only through a private CAST:; NULL-PTR BYTE-VIEW - is the address-to-integer distance."

### Fixtures
Negative (test/cast-negative-suite.f): `( n -- ptr u8 )` global → 7140, under public → 7140; `( n -- ptr a )`, `( ptr a -- n )`, `( n -- [ -- a ] )` → 7137 (replace CNC1/CNC2's 7130); linear in rows → 7137; owned family in pointee/row outside owner → 7135. Positive (test/cast-suite.f): private `( n -- ptr u8 )` + c@ round trip of a BUFFER: address; private `( n -- [ n -- n ] )` from `s" W" 0 search-wl` executed; `( [ n -- n ] -- n )` projection; `( ptr n -- ptr [ -- ] )` + xt!; `( ptr lvfam -- ptr n )` corrupt-then-refuse.

### Steps
B10a (worker-max): checker edits, both suites, docs; proof: both suites, rebuild, test/run.f, convergence.
B10b (worker, trusted-casts workspace): per file delete lies → defer fixtures → private CAST: mints; proof: affected suites, test/run.f, per-file rg counts and case counts.
B6 (worker-max): src quotation casts, then pointer mints with package wrapping, checker.f ARENA last; proof: rebuild, test/run.f, convergence.
Risks: ARENA pointee ripple (last B6 commit, full suite); defer fixture executed dies named; private cast in a test package must stay open while candidates certify; quotation-row linear walk vs AOT-OWNED:capture (a STRUCTURE, passes). Trade-off: private-only (chosen) costs ~13 global src sites a package wrapper; it is the only enforceable owner binding and matches the FFI precedent.

B10a landed 2026-10-02 (kskopkxp 72151038, Admit private pointer and quotation casts): CAST-INTRO? walks the destination's introduction positions; a pointer or quotation mint certifies only in a package's private section, else E-CAST-MINT 7147 (named E-CAST-SCOPE in the commit; the batch merge renamed it because master's borrowing work took E-CAST-SCOPE for scope dependencies, now 7151; E-USING-OUTER keeps 7146).
B10b landed 2026-10-02 (vrqzswrp 9abd0819, Convert test casts to declared casts): test/ TRUSTED: 748 -> 652, the touched files 179 -> 83; per-file counts and the casts left (stage-2 decl/type files, linear leaves) are in the commit description. Fable review accepted.

B10c landed 2026-10-02 (ssvllkkq f180f5e7, Read layout fields in the CAST: rules): CAST-INTRO? and CAST-MAY-LINEAR? walk every product field and variant payload of a layout instance (TFAM-MEMBERS-XT, substituted as MAKE/UNMAKE/MATCH do) with an exact recursion cut (cap 64, dies by name), closing the layout-field forgery and its linear twin (CNF2-4, CNL13-17, CNW1-17, CNM17-24); CAST-CERTIFY refuses any signature that did not parse before the walks: unknown family or wrong arity E-CAST-FAM, any other fault the bad-signature diagnostic and COMPILE-REJECT-RC 70 (CNB1-2, check/cast-badsig-all-errors). No tree cast refused. Fable review ACCEPT, re-review ACCEPT.
