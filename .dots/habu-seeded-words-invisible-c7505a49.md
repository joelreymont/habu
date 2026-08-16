---
title: "Seeded words invisible to the checker: lazy intake"
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-16T21:18:22.839047+02:00\""
---

P1, blocks the product becoming bin/hb (re-minted 2026-08-16 - the first leaf was lost uncommitted in a working-copy jump, the jj-new lesson). DEFECT: an AOT-seeded word is in the runtime dictionary but not the checker's record set; ': T2 ( -- ) BPW-INSTALL ;' dies 'hook: non-certified definition' on TODAY's engine; one TRUST row closes it completely (measured) - the missing thing is a checker symbol + effect record, nothing else. THE BLIT SHAPE IS REFUTED (bake-chain-17 probe): USIGS is one of ~35 persistable checker surfaces (CHECKER-SNAPSHOT-PREPARE enumerates them); signature rows name TYPE-FAMILY IDS (EN.H, identity by id, 70 families created in-window, 3712 role-qualified refs) and SYM string POINTERS - a blitted USIGS names family 111 in an engine holding 49. Route 1 (full blit) = 7.9MB + the tree's first artifact pointer-rebase + ~18ms every boot; Route 2 (eager text replay) = 85ms every boot, priced out. RULED: ROUTE 3 - lazy intake at the checker's existing miss seam (checker.f:8354 DO-TOK, one leg before UNDEFERR consulting a baked name->signature-text pool): zero cost for boots that never name a chain word, 12.5us per distinct referenced word, NO id remap; capture audit = every window dict record has a signature row or the capture refuses by name. SUB-PROBE FIRST: render->re-parse fidelity for sig-less inferred words (REND-SIG refuses unmodeled tags and >26 tyvars) - measure the sig-less share of the 6798 and round-trip before committing. FAMILY REGISTRY travels WITH this fix via the existing REG-EXT-PERSIST machinery (tens of KB) and the acceptance GAINS a family-typed reproducer - a family-typed chain word called from checked code must work or refuse loudly (the lucky-value gap named at checkpoint); if the registry work explodes on contact, checkpoint. Acceptance: both original reproducers + the family-typed one green in both engines; bin/hb --load icode.f green in the product; signature-carry mutation reds by name; then increment 3 (stacked WIP cf4ad8b1) rebases, its battery re-runs ON the product, both land, e98b03d4 closes. Also: fix the leaf's stale 'AOT seed is TTY-ARMED' structural fact (seeds at prefix-stream end in every mode since 2026-08-11).

Claim: agent=bake-chain-18 workspace=.jj-ws/habu-bake-chain

SUB-PROBE GREEN 2026-08-16 (bake-chain-17, ruling 2). Over
`require src/compiler/native/migrate.f` in a booted bin/hb: 2003
of the window's definitions are SIG-LESS (the RECXT arm), the
other ~4795 declared. All 2003 render through REND-SIG and all
2003 RE-PARSE through the production intake (NEW / SGBAD-CLEAR /
PARSE-SIG-RAW): reparse-BAD 0, overflow 0, 8499 bytes of text for
the whole sig-less population. REND-SIG refuses nothing over the
chain either - 0 `effect not recorded` diagnostics on the load.
No population refuses, so the route stands.
THE DECLARED ARM NEEDS NO ROUND TRIP AT ALL: checker.f:11665 is
the single fork - `CHECK-SIG? IF SGA @ SGU @ ... CHECKER-USIG-CERT-ADD
ELSE ... RECXT THEN` - and on the declared side SGA/SGU IS the
text the checker consumed. So the capture hook takes verbatim
text on one arm and REND-SIG's text on the other, and REC-SIG
already holds that text (it renders and drops it today).

FAMILY REGISTRY DID NOT EXPLODE (ruling 3), and it is smaller
than tens of KB in the way that matters: TFAM-N/TF-STR-U/SCHEMA-N
are 49/616/7 at the product's seed point AND at the capture
host's window open - measured by loading the capture tool's real
prelude - so THE BASE DOES NOT DRIFT and window family ids are
identical in both engines by construction. No id remap. Every
registry record is pointer-free by its own assertion
(TF-REC-PTR-MASK / SUMV-REC-PTR-MASK / PF-REC-PTR-MASK /
LAY-REC-PTR-MASK all 0; type-family.f:2291 says so in prose:
"All record fields are integers or interned offsets, so nothing
rebases"), unlike SYM-STR/CT-STR/VREC-STR which do rebase. Window
delta: 70 families (11,200 B), TF-STR 3,778 B, SUMV 317 rows
(25,360 B), LAY 0, TF-PK 0, SCHEMA 43 nodes - ~41 KB, an append
of pointer-free rows at a base the merge asserts rather than
assumes. The REPL window declares NO families (repl.f /
debug-watch.f / stepper.f carry no STRUCTURE, NEWTYPE or ENUM),
so the merge needs no family shift class - it needs the base
equality REFUSAL, the ?BASES precedent.

THE RULED PLACEMENT IS WRONG AND THE CORRECTED ONE IS PROVEN.
"One leg before UNDEFERR" cannot do the intake: USIG-ADD starts
with NEW (checker.f:1930), and NEW does `FRESH MK-ROW dup BROW !
DCUR !` - BROW and DCUR ARE the in-progress body-check state, so
an intake at DO-TOK's miss destroys the very check that asked for
it. The intake has to run where the checker already has a clean
slate: the DEFINITION boundary. Retry shape, proven end to end in
one process on today's engine (probe r1.f): a custom check hook
runs CHECK!, and when the verdict is not certified AND UNDEFERR
is set AND FAILTK/FAILTU names a pool row, it intakes that row
and checks the SAME body again - `: T2 ( -- ) BPW-INSTALL ;`
certifies and RUNS, 1 hook call, 1 intake, no spurious
diagnostic. Two consequences the ruling could not have known:
(1) NO DIAGNOSTIC SUPPRESSION IS NEEDED - the E-UNDEFINED line
comes from check-hook.f REPORT-UNCHECKABLE, which runs AFTER
CHECK! in the hook, not from inside CHECK, so a body that only
needed a seeded name never prints and a genuinely bad one prints
exactly once, from the final verdict. (2) The retry belongs
around CHECK inside CHECK! rather than around CHECK! itself, so
CHECKER-TAPE:DONE and CHECKER-CERT:PRODUCE see the final verdict
once instead of once per pass. Termination is structural, not a
bound: each pass intakes a name that was unresolvable and is now
resolvable, so no name is intaken twice and a pass that intakes
nothing exits.
THE LOOKUP KEY, and why the leg is not a scope walk: the pool
rows carry (package, visibility, name, signature) - the same key
SYM-FIND uses - so the intake re-interns through SYM-INTERN in
the row's own scope. The miss leg does not re-derive the scope
chain; FAILTK already names the token the chain failed on.
SIZE OF THE WHOLE FIX: ~41 KB registry + 8.5 KB sig-less text +
the declared rows and names, order 250 KB, against Route 1's
7.9 MB.
STILL OWED BEFORE THIS LANDS: the AOT-SIG pool cells in
src/habu/layout.f (two, swept clean at $47D0/$47D8 in the
unclaimed run between PROT:RHI/CF and the lowering state - below
$7FF8 for `DATA <off> LDR` and below DATA-START), the capture
hook + its window-record audit, artifact VERSION 4 with the
signature pool and the six registry sections, the seed emit, the
tests with their mutations, and then increment 3's rebase and
battery. Nothing above is speculative: every number is measured
and the retry is proven.
LESSON FOR THE NEXT PROBE (cost me a crash): PARSE-SIG-RAW is
( ptr u8 n -- n n n n ). Dropping two of the four leaks two cells
per call; 2003 calls overflowed the data stack into a SIGSEGV
with a bare register dump. The engine's depth guard catches
underflow, not growth.

THE WINDOW CENSUS, the flagged precondition, MEASURED 2026-08-16
(bake-chain-18). Over `require src/compiler/native/migrate.f` in a
booted bin/hb, every window dictionary record classified through
the checker's OWN tables - wid -> (package, visibility) from the
live namespace records, then SYM-FIND in that record's own scope,
then CHECKER-FIND-USIG-SYM: 6892 window records = 94 package
records + 6798 CHECKED words, with 0 retired, 0 orphan-wid, 0
no-symbol and 0 symbol-without-effect. EVERY non-package window
record is in the checker's record set - no 0 set-check span, no
signature-less generated accessor, nothing TRUSTED-only-and-
unrecorded. The classifier is not vacuous: the same pass over the
PRE-window band answers 3836 no-symbol, 339 symbol-no-effect, 53
retired and 2 orphan-wid, and a synthetic window built to fool it
puts each shape in its own bucket - declared and inferred
definitions and a package's public AND private words come out
CHECKED, a `0 set-check` span comes out no-symbol, TRUSTED: and a
bare TRUST row come out CHECKED (they do carry effect rows), and
a word whose BODY STRING is "SYN-UNCHECKED-A ( n -- n )" creates
no phantom row.
SO THE AUDIT'S CLAUSES ARE: a window record is exempt only if it
is a package record or retired; otherwise it must have a checker
symbol in its own (package, visibility) scope, an active effect
row, AND a pool row, and any of the three missing refuses BY
NAME. A `0 set-check` window word needs no pool row and stays
uncallable from checked code, exactly as it is today.

THE CAPTURE SIDE IS BUILT AND AUDITED (bake-chain-18, commit on
the lane). THE RULED HOOK PLACEMENT WAS INCOMPLETE and the audit
is what proved it: checker.f's fork at 11665 is CHECK's single
fork, but rows also enter USIGS from declarers that never run
CHECK, and capturing at the fork left 17 of the 6798 behind -
eleven TRUSTED:, one `defer NAME ( E )`, three typed-storage
cells, and two EXPORTs (elaborate.f EXPORTs SPLICE-STAGING and
SPLICE-MEANING?). E-ADD-EFFECT is the one creator of a user
record but takes ROWS, and re-rendering them is not faithful -
REND-SIG renders BROW and DCUR only, so a signature with a
return-stack clause ( R [ ... ] ) comes back without it, and
CE-SCOPE has one. So the text is taken at the three words that
REACH E-ADD-EFFECT and hold text: USIG-ADD, render.f REC-SIG, and
CHECKER-EXPORT (which copies the source row's text). That list is
gated, not trusted: the audit refuses by name if a fourth
producer ever appears.
NUMBERS OF RECORD FOR THE POOL: 7556 rows, 120,896 row bytes +
123,979 string bytes = 244,875 total. Longest signature 245
bytes, longest name 23, longest package name 20 - so the u8
length prefix the name pool uses would have been a LUCKY VALUE
(245 of 255) and the pool's strings carry a u16 length instead.
The engine re-records every declared signature at publish time,
so a row whose symbol's newest row already carries the same
interned text is dropped; without that the pool held 13,974 rows
for 6798 words. 758 rows remain as second-or-later rows for their
symbol and every pair inspected is a data record recorded `--` at
definition time and `-- ptr a` when the engine auto-trusts it at
publish - newest-wins makes the second one the live answer, which
is USIGS' own rule.
LAYOUT: AOT-SIG:POOL-CELL/LEN-CELL at $47D0/$47D8 as ruled;
checker.f mirrors them as CK-AOT-SIG-POOL-OFF/LEN-OFF because it
loads BEFORE layout.f in every host that has both (the engine's
own cold prefix and the metabuild host), which is the tree's own
documented reason for CK-SEAL-LATCH-OFF. Ruled extension: the
registered test covers all THREE mirrored pairs, upgrading the
two prose-only mirrors rather than adding a third.
THE MIRROR IS CHECKED, NOT COMMENTED: test/aot-sig-pool-suite.f
covers ALL FOUR mirrored pairs - the pool's two plus the two that
carried only prose before (CK-SEAL-LATCH-OFF, CK-USE-DEPTH-OFF) -
so the ruling upgrades the local standard instead of adding a
third unverified instance. The checker's side is read where it is
DEFINED, through tools/lint/source-lex.f, because the four
CK-*-OFF constants are not reachable from checked code and four
new PRIM: axioms for a test's convenience is not a trade worth
making; the layout side is named live in a booted engine. Seven
adversarial fixtures: the spelling in a line comment, in a paren
body, in a string literal, name and number in the wrong roles, a
non-numeric value, two definitions of one name, and a longer name
that merely starts with the wanted one. Mutation-proven three
ways - drifting the new pool mirror reds assertion 18, drifting
the OLD prose-only CK-SEAL-LATCH-OFF reds 22, and moving
LEN-CELL in layout.f reds both 20 and 26. Registered as SUITE
aot-sig-pool on the tail-engine slice, and the registration is
proven rather than assumed: deleting its slice predicate makes
tools/lint/schedule-lint.f name it by file, line and label.
LESSON PAID FOR TWICE HERE: num-parse answers ( value is-double?
is-number? ), and reading the MIDDLE flag as validity makes every
literal answer "not a number" so a whole structural scan returns
nothing and looks like a scanner bug; and a fixture that leaves
the lexer holding UNTERMINATED-QUOTE changes what the NEXT
fixture counts, so every fixture now asserts the lexer's own
health before its count is believed.
STILL OWED: the audit moved into src/habu/aot-capture.f and its
tool, the artifact's signature + six registry sections at VERSION
4, the seed emit and the CHECK! retry, the remaining tests with
their mutations, increment 3's rebase and battery, and a control
build attributing the certified 4474 -> 4501 delta.
