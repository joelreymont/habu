---
title: "Seeded words invisible to the checker: lazy intake"
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-16T21:18:22.839047+02:00\""
---

P1, blocks the product becoming bin/hb (re-minted 2026-08-16 - the first leaf was lost uncommitted in a working-copy jump, the jj-new lesson). DEFECT: an AOT-seeded word is in the runtime dictionary but not the checker's record set; ': T2 ( -- ) BPW-INSTALL ;' dies 'hook: non-certified definition' on TODAY's engine; one TRUST row closes it completely (measured) - the missing thing is a checker symbol + effect record, nothing else. THE BLIT SHAPE IS REFUTED (bake-chain-17 probe): USIGS is one of ~35 persistable checker surfaces (CHECKER-SNAPSHOT-PREPARE enumerates them); signature rows name TYPE-FAMILY IDS (EN.H, identity by id, 70 families created in-window, 3712 role-qualified refs) and SYM string POINTERS - a blitted USIGS names family 111 in an engine holding 49. Route 1 (full blit) = 7.9MB + the tree's first artifact pointer-rebase + ~18ms every boot; Route 2 (eager text replay) = 85ms every boot, priced out. RULED: ROUTE 3 - lazy intake at the checker's existing miss seam (checker.f:8354 DO-TOK, one leg before UNDEFERR consulting a baked name->signature-text pool): zero cost for boots that never name a chain word, 12.5us per distinct referenced word, NO id remap; capture audit = every window dict record has a signature row or the capture refuses by name. SUB-PROBE FIRST: render->re-parse fidelity for sig-less inferred words (REND-SIG refuses unmodeled tags and >26 tyvars) - measure the sig-less share of the 6798 and round-trip before committing. FAMILY REGISTRY travels WITH this fix via the existing REG-EXT-PERSIST machinery (tens of KB) and the acceptance GAINS a family-typed reproducer - a family-typed chain word called from checked code must work or refuse loudly (the lucky-value gap named at checkpoint); if the registry work explodes on contact, checkpoint. Acceptance: both original reproducers + the family-typed one green in both engines; bin/hb --load icode.f green in the product; signature-carry mutation reds by name; then increment 3 (stacked WIP cf4ad8b1) rebases, its battery re-runs ON the product, both land, e98b03d4 closes. Also: fix the leaf's stale 'AOT seed is TTY-ARMED' structural fact (seeds at prefix-stream end in every mode since 2026-08-11).


Claim: agent=bake-chain-22 workspace=.jj-ws/habu-bake-chain (leaves B
and the region-room hole closed; leaf C diagnosed and dotted;
increment 3 still BLOCKED pending 2f9cc56d)

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
THE AUDIT IS IN THE PRODUCTION CAPTURE PATH (increment B). It runs
inside AOT-CAPTURE:CAPTURE, per RECORD and not as a count
comparison, so the three-producer enumeration is a thing a gate
refuses rather than a thing a comment claims. Measured through
the real tool: recs=6892, sigknown=6798, sigexempt=94,
sigs=7556 - and 6798 + 94 = 6892 is asserted in
test/aot-chain-capture-suite.f as the partition it is. Mutation
table: dropping the CHECKER-EXPORT producer reds naming
SPLICE-STAGING (the exact pair the audit first found), never
arming the store reds naming MAKE, dropping the USIG-ADD producer
reds naming MAKE, and dropping the package-record counter reds
the partition assertion 6892 vs 6798 plus the sigexempt floor.
The two questions live in the checker (CHECKER-ASIG-KNOWN? /
CHECKER-ASIG-MISSING?) because they are about two checker tables;
they take the scope as a package name plus one boolean, never a
visibility number, so SYM-GLOBAL/PRIVATE/PUBLIC stays one
authority - this file's constants are not reachable from checked
code, so a caller spelling them would be a second.
A BOUND THE RULING DID NOT NAME, found by measurement: nothing
disarmed the store at the window's end, so the capture TOOL's own
post-window closure (icode.f, aot-decl.f, aot-capture.f, the
artifact writer) landed in the pool - 8279 rows where the window
has 7556, the extra 723 naming words no target engine has. A
signature for a word that is not there is worse than a missing
one: a definition naming it would certify and then call nothing.
AOT-ARM:SIG-CLOSE now ends the window's DEFINITIONS at CLOSE,
which is a different fact from the window's SPAN ending (the span
deliberately has none).
TWO MORE THINGS REVIEW CAUGHT, both real. (1) The audit ran
BEFORE the call and address scans and masked them:
test/aot-band-data.f's address refusal came back as an uncarried
signature. Soundness refusals come first; the audit is last of
the audits now. (2) Arming the signature store off the window
CELLS' VALUE conflated two axes - `0 0 OPEN` disarms the
INLINER's window on purpose (test/aot-band-lib.f OPEN-UNARMED
needs a copied pre-window body) and says nothing about whether
that window's signatures are wanted. OPEN now always arms;
SIG-CLOSE is the only thing that ends collection.
AND ONE PERFORMANCE DEFECT: the wid -> package lookup walked the
whole dictionary per window record, 6892 times. One memo cell
keyed on the wid (records arrive grouped by package) turns that
into 94 walks - the capture went 2.96s -> 1.86s, which is faster
than it was before the audit existed. The two near-identical
namespace walks are one parameterized walk now.
STILL OWED: the artifact's signature + six registry sections at VERSION
4, the seed emit and the CHECK! retry, the remaining tests with
their mutations, increment 3's rebase and battery, and a control
build attributing the certified 4474 -> 4501 delta.

THE SEED SIDE IS BUILT AND THE ACCEPTANCE IS GREEN 2026-08-17
(bake-chain-19). ARTIFACT VERSION 4 carries three new sections:
signature rows (16 B: name/sig/pkg offsets + visibility), the
signature strings they name, and the type registry delta as ONE
opaque span whose internal table belongs to the registry that owns
those records. Three sections and not ten, because aot-file.f does not
need to know TF-REC to carry bytes - it moves them and, for a merge,
moves a row's three string offsets behind the pool they are appended
to. The seed publishes all three as one span through layout.f's two
AOT-SIG cells, behind a table of its own that the checker validates by
name before it believes a byte.
THE COPY IS THE AUDIT'S WALK, as ruled: one row per checked window
record, taken from that symbol's NEWEST pool row, so the 758
duplicates DROP and the reader never has to implement newest-wins a
second time. The strings travel WHOLE and verbatim: the rows index
that arena by offset, so re-interning the referenced subset would mean
rewriting every offset - a remap where a copy does - and what the
arena holds beyond the surviving rows is the interned text of the
dropped duplicates, kilobytes against 124 KB. Measured: sigrows=6798
(= sigknown), sigstr=123979, reg=46386, payload 279,189 bytes.
THE REGISTRY IS EIGHT STORES, NOT SIX. The handoff's list omitted
product fields, and the schema half is two stores rather than one:
TF/TF-PK/SUMV/PF/LAY/TF-STR/SCH/SCH-ROOT, which is exactly what
REG-EXT-PERSIST already enumerates. Measured deltas over the chain:
+70/+0/+317/+43/+0/+3778 B/+43/+86. CT and VREC do NOT grow over the
chain (measured both ways), so the eight are the whole of it.
IT IS ALL EIGHT OR NONE, and an empty delta asserts nothing: the
stores name each other by id and by interned offset, so a delta in one
is read against every other store's base, and a window that declared
no type of its own (the metabuild REPL's) has no ids to protect.
THE RULING'S FAILTK LOOKUP IS REFUTED, and the replacement is
measured: DO-TOK1 pins EVERY token until FAILSET latches, so a body
whose seeded name is followed by one more token pins that other token
(`: T ( -- ) SEEDED dup ;` pins `dup`). The miss leg queues the NAMES
instead, and the intake takes every pool row for every queued name -
all of them, because the same tail lives in many packages of a chain
and taking only the newest leaves the reference that meant another
unresolved. Two passes for any body, not one per seeded name.
TWO THINGS THE RULING COULD NOT HAVE KNOWN, both found by running it:
(1) THE SOURCE TAPE REFUSES A SECOND SCAN of a unit it is recording
(feed.f ON-SCAN, E-NFEED-SCAN), and a retry re-scans. What the
observer is owed is the token stream, and that is a function of the
TEXT alone - the reader splits on spaces and decides a payload skip
from a token's own spelling - so the first pass gives it the whole
stream, every later pass is silent, and CHECK-RETRY asserts the token
counts matched rather than assuming it.
(2) A DIAGNOSTIC WOULD HAVE PRINTED TWICE for a body with both a
seeded name and a real error. CHECK now latches "another pass is due"
where the verdict is reached - the two facts it needs are both there -
and holds the diagnostic for a pass another pass replaces. Measured:
`: T5 ( n -- n ) BPW-INSTALL drop drop ;` reports once, naming `drop`.
THE WINDOW'S TYPES END AT THE CAPTURE, not at a close: the rows that
travel are chosen by a walk over THIS window's records, so a store
holding more is harmless, but the registry delta is two high-waters
subtracted and the moment it is read is the whole of what it means.
The one caller that must close early is the chain tool, whose own
assembler and artifact writer declare families after its window
(measured: read at capture time the delta counted more than the
window's 70 and the seeded engine refused its own registry).
ACCEPTANCE, all measured: reproducer 1 green on the host
(`: T2 ( -- ) BPW-INSTALL ;` runs, exit 0, where master exits 70);
reproducer 2 green on the product (`using A64ASM : T ( n -- n ) ENC-B ;`);
THE FAMILY-TYPED ONE certifies AND its misuse is refused BY TYPE
("expected: ir-arena:view<> actual: n"), which is what proves the
family arrived as a family and not as an untyped cell; and
`bin/hb --load src/arch/arm64/icode.f` is green on the product, the
phase that named the whole defect. A word no window compiled is still
undefined by name. All four are registered cases (PROBE-SEEDED-SIG in
test/aot-chain-capture-suite.f), mutation-proven: making the pool
unusable reds them with the original E-UNDEFINED, and dropping the
registry base assertion reds PROBE-REG-BASE's two refusal clauses.
STILL OWED, and the lane stops here: tools/build-fixpoint-test.f's
source-boundary case (asserts 224/225) is RED and the red is MINE - a
control build of master's tree in this workspace passes it. What is
measured so far: bin/hb and master's engine accept and refuse the same
sizes through the same `--build` path with the same message and code
(ok at 1 and 2 MiB, `hb: source prefix buffer full` rc 74 at 3 and 4
MiB), so the difference is not the boundary itself; assertion 224 is
one of PROBE's own (an empty label), which makes the timeout branch
and the message branch the two candidates. That has to be root-caused
before this merges. Then increment 3's rebase (change rqmvvossnnsx /
cf4ad8b1) and its battery on the product, the numbers of record, and
the certified-delta control attribution.
GATES ON THE LANE SO FAR: install fixpoint x2 byte-identical
(3907272c...), engine size unchanged at 165367; maki ENGINE RC=0;
the capture suite green including the two new cases; aot-wide-format,
aot-wid-suite, aot-sig-pool, aot-seed-batch, aot-prelude-band green;
both diff lints exit 0 (liveness proved twice - the package lint
refused this lane's own new global and its label ids until they moved
into package AOT-SIG); error-code, section-reach, stdin-closure,
bootstrap-mirror, schedule and dot lints 0 findings. Ratchets bumped
with their attribution: the checker axiom registry 386 -> 390, the
AOT section-label count 28 -> 30, and MACOS-CODE-TEXT 131984 -> 135488
(+3504 = 3452 payload read from the engine's own published cell + 8
for its length word + 44 for AOT-SIG:PUBLISH,'s eleven instructions),
floor distance 5008 -> 8512, same 16 KiB page, file still 165367.

THE RED WAS ROOT-CAUSED AND IT WAS NOT WHAT THE HANDOFF GUESSED
(bake-chain-20, 2026-08-17). Not build timing, not a new stdout line,
not a timeout: asserts 224/225 are POLICY's two, not PROBE's. The
"empty label" clue was a red herring - lib/test/assert.f runs
T-LABEL-CLEAR after EVERY assert, so only a subtest's FIRST assert
carries its label. 224 is `SOURCE-ARENA-CAP need >=` and 225 is
`need NEXT-POW2 SOURCE-ARENA-CAP T=`, which reported got 8388608.
Every PROBE assert passed; the engine accepts and refuses exactly
what master's does.
THE CAUSE IS THE CAPACITY POLICY. A `--build` holds the cold prefix
and the generated stage2 source in ONE arena, so LIVE-SIZE is their
sum. Measured, cold prefix by bisecting `--build` (CAP - OK - 1) and
stage2 through BF-STAGE2-SOURCE: master cb26fc19 1,391,858 +
1,961,629 = 3,353,487, need 4,191,859, fits $400000; increment C
1,422,190 + 1,999,714 = 3,421,904, need 4,277,380, does not. The
master control ran the same engine over a `git archive cb26fc19`
tree, which is sound because increment C changes no PFX-LOAD-ROW -
the cold-prefix FILE LIST is identical and only contents differ.
Delta accounted to the byte: stage2 +38,085 = checker 20,410 +
type-family 9,925 + habu2 4,051 + aot-decl 2,652 + habu1 508 +
image-bytes 539; cold +30,332 = checker + type-family, the only two
changed files the cold prefix carries.
THE ARENA WAS ALREADY 1,956 BYTES FROM THIS RED. The largest
composite $400000 admits is 3,355,443 and master stood at 3,353,487.
checker.f is counted TWICE (cold prefix and stage2 both carry it), so
978 bytes of checker source would have tripped it for whoever edited
next. The bump was owed whatever landed.
LANDED (commit 9219ec56, approved on master 3dd55e61): SOURCE-ARENA-CAP
$400000 -> $800000, the gforth token mirror with it, CODE-CAP-BYTES
$AC0000 -> $EC0000 (ADR-HI + IBUFSZ + AOT-SECTION-CAP), MSIZE $AF0000
-> $F00000 by image-bytes.f's own method, sixth time - MPAGE $EC1000,
tail $4000 + 104 + a $1D96C bound, image $EE29D4, round $EF0000,
margin $F00000. The method was validated by re-deriving the CURRENT
line from scratch first: it reproduces $1596C / $ADA9D4 / $AF0000
exactly. Three mutations red their guards: drifting CODE-CAP-BYTES to
$EB0000 reds test/icode-fixup-test.f, drifting the gforth mirror back
to $400000 reds tools/bootstrap-codegen-test.f's token compare, and
putting MSIZE back to $AF0000 kills a build by name with `macho:
MSIZE below max image`. So $F00000 is required, not merely roomy.
Engine file size unchanged at 165367 (the window is virtual, as
icode.f's own prose says); install fixpoint x2 byte-identical
e2a5c242...; build-fixpoint-test green rc 0; icode-fixup,
bootstrap-codegen, image-bytes, engine-size, hb-build all green.
Two dots minted from this: habu-dedup-the-cold-8010f67c (the cold
prefix and the stage2 source carry ~1.4 MB of ONE checker/core text
twice, which is where the real headroom is, with the Linux DATA-SIZE
rider for maker.f's 8-of-32MiB dictionary allot) and
habu-maker-f-comment-249a69f5 (maker.f:22 says "mmap'd" of an allot).

NOW THE BLOCKER, AND IT IS INCREMENT C'S OWN DESIGN (bake-chain-20).
Increment 3 rebased clean onto the fixed stack (abf880fa) and the
product BUILDS: install 17.6s, bin/hb 3,649,399 bytes, the artifact
fixpoint held ("two processes wrote the same artifact"), and the
host digest e2a5c242... is byte-identical to the engine this tree
installs without the bake, exactly as increment 3 claims. The
ACCEPTANCE passes on the product: `bin/hb --load src/arch/arm64/icode.f`
rc 0 and `using A64ASM : T ( n -- n ) ENC-B ;` runs - the phase that
named the whole defect is green.
BUT test/run.f reds ~60 phases, all with ONE message:
`tfam: the seeded families registry opens at 53 where the capture
marked 49` / `tfam: a seeded type registry does not start where its
capture did`. THE REFUSAL IS CORRECT AND THE DESIGN IS WHAT IS WRONG.
Falsified with a minimal fixture, three cases on the product: a file
that names a seeded chain word and declares NO family of its own is
green; the SAME file with ONE `NEWTYPE` ahead of it dies; with four,
dies. So the mechanism is exact - the registry install is deferred to
the first lazy intake (checker.f CK-AOT-REG-INSTALL), but the carried
family ids are absolute (EN.H, identity by id) and are only valid at
the BOOT-TIME base. Any user family declared before the first chain
reference shifts the high-water and the base-equality clause refuses.
Declaring a type and then calling a chain word is completely ordinary,
which is why it is 60 phases and not a corner.
WHY INCREMENT C'S ACCEPTANCE COULD NOT SEE IT: the family-typed
reproducer USES a chain family (ir-arena:view<>); it never DECLARES a
new one ahead of the intake. The gap only opens when bin/hb is the
product, so increment 3 is the first thing that could show it.
CK-AOT-REG-INSTALL's own comment already names the three answers -
"already there, appendable, or neither, and the third is a refusal" -
and the bug is that "appendable" is judged by base equality, so it is
only true when nothing has declared a family yet. A lazy install
cannot promise that.
THE FIX THAT LOOKS RIGHT, UNRULED, FOR THE COORDINATOR: install the
REGISTRY eagerly at the seed point (prefix-stream end, before any user
token), and keep only the SIGNATURE POOL lazy. That is where
appendability is true by construction and where no rollback frame is
open, which answers the comment's stated reason for latching nowhere.
It also keeps the lazy intake's real win: the registry is ~46 KB of
pointer-free rows, against the 124 KB of signature text and 6798 rows
whose parse is what the laziness was priced to avoid. The alternative,
remapping ids at intake, is the id remap the design ruled out and
would have to rewrite every family id in every carried row plus the
eight stores' cross-references.
STILL OWED AFTER THAT RULING: the registry-eager change with a
registered regression (declare a family, then name a chain word) and
its mutations, the battery re-run on the product, THE NUMBERS OF
RECORD (baseline bare 258.3 / source-chain 1353.3 / baked 353.2 /
baked-no-chain 261.1 / install 6.74s -> 16.0s; this lane measured
install 6.4s unbaked and 17.6s baked, the rest not yet re-measured),
the certified-delta attribution (4474 -> 4571 on the host at 9219ec56),
and increment 3's description still says "BLOCKED, do not merge".

CLAUSE (0) VERDICT 2026-08-17 (bake-chain-20): FIXTURE ARTIFACT,
and the anomaly was MIS-ATTRIBUTED - by me, in the flag that
became the ruling's first clause. There is no packed value
reading back as 0. ir-id.f:412 declares `13 constant FAMILY#`,
and the failing assert is :556 `TF1 @ TF0 @ - FAMILY# T=` - a
family-count DELTA - which reads 0. The 13 is FAMILY#, not the
13 that ATTR-CASE packs at :157; the two are the same number by
coincidence and I matched the wrong one by grepping `13 T=`,
which :556 does not spell. The run confirms it: assert 1 carries
an EMPTY label, and FAMILY-SURFACE is the one T= in the file
with no T-LABEL above it. The `tfam: bad family id` that follows
is :557's loop indexing TF0+0..12 past the live TFAM-N.
ATTR-LOCAL IS CORRECT ON THE PRODUCT, proven outside the fixture
with the fixture's own sequence (NEW-MODULE, PACK-ATTR 13,
ATTR-LOCAL): local=13, owner-same=yes, count-n=14, local2=5, and
bin/hb and hb-host agree on all four. No seed defect here, and
no data word lost its value. WHY 0 IS LEGITIMATE UNDER SEEDING:
BEFORE/AFTER latch TFAM-N around the fixture's require of the
chain; seeded, every closure file is already `provided`, the
require is a no-op, no family is declared between the latches,
so the delta is 0 by construction. Subject = source-loading the
chain -> bin/hb-host under clause (2).

BUT CLAUSE (3) IS NOT ONE CLASS, AND THE REST OF IT IS A REAL
DEFECT. The ~15 phases do not share ir-id.f's shape: ir-op.f,
ir-build.f and native-tape.f carry no TFAM-N bracket at all and
die `checker: a seeded signature does not parse in the engine it
was baked into` - the same message the no-install mutation
produced. The stored text for IR-SOURCE:SPAN is
` IR-ID:ir-source-id n n -- span`; the source at
src/compiler/ir/source.f:371 declares
`( IR-ARENA:arena IR-ID:ir-source-id n n -- IR-SOURCE:span )`.
So a seeded word's OWN signature does not round-trip: the
leading IR-ARENA:arena is gone to a bare space and IR-SOURCE:span
lost its qualifier, while IR-ID:ir-source-id kept one. Naming
IR-SOURCE:SPAN by hand certifies on both engines, so the registry
travelled - what fails is the STORE'S OWN RENDER of a signature
whose types are package-qualified families.
THE ACCEPTANCE GAP, same shape as increment C's: every
family-typed case so far put the family in the TEST'S signature
(SIGFT's IR-ARENA:view) and never exercised a SEEDED word whose
own signature names one. Nothing covered the round trip.
DOT: habu-seeded-sig-qualifier-round-trip. This BLOCKS the close.
No fixture in this class is to be adapted around it.

RENDER-DEFECT PROBE VERDICT 2026-08-17 (bake-chain-20): it is
NONE OF THE THREE CANDIDATES, and the ruling's acceptance as
written would pass while the bug remained. Evidence is the
artifact's own bytes (chain-b.aot, strings -a):
  line 7421  IR-ARENA:arena IR-ID:ir-source-id n n -- IR-SOURCE:span
  line 7347  IR-ID:ir-source-id n n -- span
  line 7350  span -- IR-ID:ir-source-id n n
The FULL, CORRECT declared text for IR-SOURCE:SPAN is stored,
intact and qualified. Nothing was truncated and no leading term
was dropped. 7347/7350 are DIFFERENT ROWS - the names that
follow them in the string section are `make` and `unmake`, the
STRUCTURE-generated constructor and destructor for `span`, and
their shapes match its three fields exactly (src
IR-ID:ir-source-id, start n, len n). I had compared two
unrelated rows.
SO REND-SIG IS INNOCENT (the declared text never went near it),
THE DECLARED-ARM TAKER IS INNOCENT (SGA/SGU stored the text
verbatim - text-equality with the declaration HOLDS), and THE
POOL WRITER IS INNOCENT (the leading space is the signature-text
convention, present on every row).
THE ACTUAL DEFECT: stored signature text is SCOPE-DEPENDENT and
the intake re-parses it in the wrong scope. Naming a family bare
from inside its own package is legal Habu - `make`/`unmake` in
package ir-source say `span`, not `IR-SOURCE:span`, and they
certified at definition. 153 of the 1087 stored signatures carry
a bare non-primitive term (digest, arch, abi, view, arena, ctx,
builder ...). Every one is unparseable outside its package.
THE FIX IS THE PARSE SCOPE, NOT THE TEXT. The row already
carries pkg-off beside name-off, sig-off and vis, and the intake
already uses the package to re-intern the word where it lives;
the signature parse must use that same package as its resolution
scope. Qualifying the STRUCTURE generator's output instead would
be a patch: it repairs generated words and leaves every
hand-written bare-family signature broken.
ACCEPTANCE AS RULED IS UNSOUND ON THIS AXIS: "the round-tripped
text is EQUAL to the declaration" already holds - the text is
byte-identical to what was declared. A case asserting it passes
today and constrains nothing. The axis that does work: a seeded
word whose own signature names its OWN package's family BARE
must certify a caller in a FOREIGN scope; mutation = parse the
stored text in the intake's scope instead of the row's, which
reds it by name.

SCOPE-PARSE IMPLEMENTATION + THE THIRD BYTE-DUMP VERDICT
2026-08-17 (bake-chain-20): the ruling's fix landed in three
parts, and running it home surfaced one more defect class,
dotted habu-seeded-variant-ctor-c98479f0 and CHECKPOINTED.
(1) sumtype.f TDGEN-OUT-TYPE now spells the generated
declaration's own family QUALIFIED, the way TDGEN-FAM-REF
already spelled every payload family. The bare tail was the one
exception, and it is exactly the 153 bare rows: every one is
generator output (make/unmake/eq/tag/ordinal); the tree's
hand-written signatures qualify even in-package (measured:
IR-ARENA uses IR-ARENA:view inside its own file). The pinned
generated text in test/type-ctor-suite.f:1197 grew by its
qualifier (pv:zpl) and now guards the spelling by mutation.
(2) checker.f SIG-SCOPE$/SIGSCOPE!: the AOT intake parses a
stored VERBATIM signature in the ROW'S package (armed only in
CK-AOT-TAKE). For hand-written rows the row package IS the
declaring scope. NOTE the ruling's premise fails for generated
rows: their row package is the CTOR package (ir--source-span),
not the scope the text renders in - which is why (1) is needed.
(3) checker.f CHECK-CANDIDATE! now answers through CHECK-RETRY:
a bare CHECK cannot see a seeded word never taken in-process,
so ir-context.f's seal battery read verdict 1 (undefined) where
the source engine reads 0 (refused). Snapshot/restore of the
retry state makes nesting safe. All five seal candidates now
answer identically on product and host, loud diagnostics equal.
RESULTS: ir-op/ir-build/native-tape/ir-attr/ir-context/
ir-source/native-feed green on the product; battery 29 -> 17
RED (6 wrappers); zero seeded-signature parse failures remain.
THE THIRD VERDICT (dump-the-record, again): scan-index's red is
SCX-DIFF-SUMV (55), not USIG - the SVX ctor index never hears
the seeded delta's bulk append. The honest one-line reset
exposed habu-seeded-variant-ctor-c98479f0 (see the dot: carried
SV.CTOR-SYM ids are capture-absolute into the UNALIGNED sym
store; a colliding fresh sym inherits a stale variant;
count-dependent; wedged the install until reverted). The reset
is REVERTED and must land with the ctor-sym repair, never alone.
scan-index stays red as that defect's honest symptom.

CTOR-SYM REPAIR LANDED 2026-08-17 (bake-chain-20, per the ruling
at 48651132): mechanism (b), zero-on-load, chosen by consumer
shape - SVX-LINK and SUMV-FROM-CTOR-SYM already refuse to key on
0, so absence is structural, and every seeded suite proves a
seeded ctor's calls certify through its intaken signature row.
REG-AOT-SCRUB zeroes SV.CTOR-SYM for exactly the rows the load
appends (store 2, [base,base+cnt)); the SVX reset lands in the
same change, safe now because the rows it exposes are scrubbed.
MEASURED AFTER: the install fixpoint survives its own engine
(the exact 11-file load that wedged), checker-scan-index is
GREEN (the ds=55 differential agrees - first green since the
capture began carrying the registry), ir-op/ir-context stay
green, the count-collision reproducer stays clean, and a walk
finds zero SUMV rows with ctor-sym >= SYM-N. Mutations, both
run in this lane: reset-without-scrub is the wedge (install
itself reds, exit 70); scrub-without-reset is the mask
(scan-index's registered differential reds at ds=55). Each half
is guarded by a registered gate that fails without the other.

CTOR-SYM LANDED + PARTITION LANDED + HANDOFF 2026-08-17
(bake-chain-20, context handoff; stack is 8 commits on 9e9263e7,
nothing pushed, all in .jj-ws/habu-bake-chain):
  3210c5a5 test: the bin-surface pins admit the kept capture host
  9e33e19f test: partition fixtures by subject between product/host
  7142d09c build: the install keeps the capture host beside the product
  136ca999 tfam: no absolute foreign id survives the registry load
  08f715cd checker: seeded rows parse in their own scope, candidates retry
  c4e108d8 sumtype: spell the generated declaration's own family qualified
  281da640 aot: install the seeded type registry at the seed point
  331a549a + 8bdc7b85-line dots commits (verdicts, defect records)
BATTERY: 29 -> 17 -> 10 -> 6 RED (3 wrappers + 3 leaves).
PARTITION TABLE (subject -> engine, stated in each header):
  test/aot-chain-capture-suite.f  capture+bake via HB$ -> bin/hb-host
    (baked programs stay on the engines it bakes; producer-key
    probe hashes HB$); GREEN on the schedule.
  test/snapshot-writer.f  child build -> bin/hb-host default,
    HABU_UNDER_TEST overrides; GREEN.
  test/compiler/ir-id.f, ir-id-manifest.f, ir-id-proof.f  ->
    thin -host drivers via test/host-run-lib.f (child verdict is
    the verdict, output relayed); schedules updated in BOTH
    test/gate-stdlib-cases.f and the tail-pure fork list; GREEN.
  bin-surface pins (hb-baseline-contracts, bundle-lib-test) now
  name bin/hb-host and pin the count at two; GREEN.
CTOR-SYM: REG-AOT-SCRUB + SVX reset landed together (136ca999).
scan-index GREEN (first since the registry travelled); the
install survives its own engine; zero foreign ids in a walk.
REMAINING RED, all pre-existing, none diagnosed to root yet:
 (1) native checker diagnostics repair slice - throws -2201
     (E-FS-OPEN) into expected-0 asserts; same code family as
     GROUP stdlib/tool-doc. Suspect: a repair/doc tool opening a
     path that exists only in a source tree arrangement - NOT
     seeded-signature related (predates the fix chain unchanged).
 (2) build-fixpoint-fixtures (tools/build-fixpoint-test.f): 10
     fails on BOTH engines - emitted-source CONTAINS pins
     (F41..F62) plus a snap child dying `hb: AOT call site
     unresolved` exit 82. Red on both engines means NOT a
     partition case; likely the emitted prelude changed shape
     under the increments (fresh diagnosis owed).
 (3) MAKI on the product: rc 77 `hb: dictionary full at:
     DLT-ROOT-U` in maki/examples/nanogpt/data-loader-test.f;
     host green. The seed registers the chain's ~6900 records,
     and maki's largest example overflows the remaining dict
     headroom. Fix shape: grow the dictionary capacity in the
     layout (seed-affecting; install --force + full battery + a
     fresh look at ENGINE-SIZE marks). Judge/lints/NUMBERS OF
     RECORD and the certified walk (4474 -> 4574 + today's
     commits) still owed; increment 3 stays BLOCKED until the
     phases are green.

REGISTRY TIMING RULING 2026-08-17 (bake-chain-20's blocker; the
refusal is CORRECT, the lazy install's promise is not): the
TYPE-FAMILY REGISTRY installs EAGERLY at the seed point -
prefix-stream end, before any user token - where base equality
is true BY CONSTRUCTION and no rollback frame is open (the
install's own comment states why it latched nowhere; the seed
point is the answer to that comment's stated reason). The
SIGNATURE POOL stays lazy - the laziness was priced for 124KB
of parse-heavy text and 6798 parses, not for 46KB of
pointer-free rows. Id remap at intake stays REFUSED (ruled at
the route decision). Regression: the three-case fixture (no
family / one NEWTYPE ahead / four ahead - all green after) plus
a registered case declaring a family THEN naming a chain word;
mutations: eager install dropped reds the fixture; the
base-equality refusal must STAY for a genuinely foreign base
(the forge case keeps it live). Increment C's acceptance gap is
recorded: its family-typed reproducer used a chain family but
never declared one ahead - only the product could see this.

FIXTURE-PARTITION RULING 2026-08-17 (the 29 remaining phases):
(0) FIRST, before any adaptation: probe the ATTR-LOCAL anomaly -
ir-id.f:157 packs 13, :158 reads 0 on the product. A packed
value reading back 0 is potentially a data word whose VALUE did
not travel (a seed defect class nothing yet audits). Nobody
adapts a fixture in that family until this has its own verdict;
if it is a defect, it gets a dot and blocks the close.
(1) THE CAPTURE HOST IS KEPT: install retains bin/hb-host
beside bin/hb (the build already emits it; never shipped is not
never kept). Fixtures obtain a capture host by PATH, no
rebuilding.
(2) ONE PARTITION RULE covers both classes: a fixture whose
SUBJECT is source-loading the chain (captures, load-time deltas,
family-count brackets) runs against bin/hb-host; a fixture whose
subject is chain BEHAVIOR runs against the product. The
partition is per-fixture-subject, stated in each fixture's
header, not per-suite.
(3) The class that changed failure mode under the fix gets
fresh diagnosis per the lane's own flag - the old diagnosis is
void.

SCOPE-PARSE RULING 2026-08-17 (the fork's byte-dump verdict; the
render/taker/writer candidates are ALL innocent - the earlier
"mangled" comparison was two unrelated rows, the STRUCTURE
make/unmake pair whose shapes coincidentally matched): stored
signature text is SCOPE-DEPENDENT - 153 of 1087 rows carry a
bare non-primitive term legal in its own package and unparseable
outside it. RULED: THE PARSE USES THE ROW'S PACKAGE as its
resolution scope - pkg-off is already on the row and the intake
already interns the word there; one authority, no text rewrite.
Qualifying the STRUCTURE generator is REFUSED as the patch shape
(repairs generated words, leaves hand-written bare signatures
broken). THE RULED ACCEPTANCE IS CORRECTED - text-equal
round-trip is VACUOUS (it already holds over a live bug); the
axis that works: a seeded word whose own signature names its OWN
package's family BARE must certify a caller in a FOREIGN scope;
mutation = parse the stored text in the intake's scope instead
of the row's, redding by name. Lane rule adopted into LESSONS at
the close: DUMP THE RECORD BEFORE NAMING THE PRODUCER - twice
this lane a symptom-read produced a wrong mechanism and the
bytes settled it in minutes.

CTOR-SYM RULING + PREMISE RATIFICATION 2026-08-17 (the fork's
checkpoint; new defect class c98479f0 - seeded SUMV variant rows
carry ABSOLUTE capture-engine sym ids, masked today by SVX-SYNC's
deliberate direction gap; the honest reset converts the mask to
active mis-binding and wedged the fixpoint - reverted, recovered,
dotted):
(1) THE INVARIANT RULES, the mechanism is the fork's to derive
per consumer evidence: NO ABSOLUTE FOREIGN ID SURVIVES LOAD -
whatever consumes SV.CTOR-SYM must see a TARGET-MINTED id or a
structurally-absent marker, never a capture-engine number.
Between (a) re-derive at load from the variant's (pkg,name) and
(b) zero-on-load + re-intern at first use: pick by what the
consumer's shape makes structural (if SVX treats 0 as absent
and the first checked use interns through the existing intake,
(b) is the lazy-where-priced shape; if the index must answer
eagerly, (a) at the seed point beside the registry install).
Checkpoint if both fail on contact. THE SVX RESET LANDS WITH
THE REPAIR, never alone - ratified in the fork's words.
scan-index stays red as the honest symptom until then.
(2) PREMISE CORRECTION RATIFIED, reversing part of the scope-
parse ruling on the fork's evidence: the 153 bare rows are ALL
generator output, and the row package for generated words is
the CTOR package, not the writing scope - so row-package parsing
alone could never fix them. Hand-written signatures qualify even
in-package (measured), so the generator was the SOLE producer of
bare text, and TDGEN-OUT-TYPE qualifying the generated
declaration's own family - matching its existing treatment of
payload families - is fixing the only producer, not the patch
the original refusal targeted. Both halves landed (generator
qualification + SIGSCOPE row-package parse armed only in the
intake) with the pv:zpl mutation pin.
(3) The CHECK-CANDIDATE! retry-snapshot fix (seal candidates
answering identically on product and host, caught by ir-context's
seal battery) is noted as increment-C completion, not new scope.

DICT-CAPACITY RULING 2026-08-17 (bake-chain-21's record: seed
delta exactly 6892 records constant across all 193 files; the
monolithic inventory needs 33,302; master was ALREADY under its
own 25% standard unseeded at 26,410x1.25 - the bump was owed
before the seed, the seed made it fail; and the STRUCTURAL
SURPRISE - REGION holds dict AND code, so a dict lift alone
leaves 36KB of code band):
(1) DICT-CAP 65,536 - Q1 answered: NOT 49,152; picking a cap to
dodge an encoder limit is the convenience the review gate
refuses, and the HIDX-SLOTS = 2x identity stays whole. The
786KB snapshot cost is real and noted, not deciding.
(2) REGION $A00000 AND the named CODE-AREA constant - the
coupling becomes a constant a reader sees instead of a
subtraction nobody performs. Approved as the structural half.
(3) The cascade as tabled: nine MOVZ sites to LIT64 (the
HIDX-SLOTS precedent), the four-file mirror set (forth.fs,
Reloc.v region_bytes, the frozen reloc-schema literal) - the
parity gate enforces it in every direction.
(Q2) THE HEADROOM PROBE IS APPROVED - it passes the new-
mechanism test exactly: the failing probe is this very red (the
wall blamed an innocent file, the prot-wid-probe shape), and
the named first consumer already exists (maki/test.f's closing
REQUIRE-ROOM). Dict and code headroom on one line, fail-closed
at a floor stated as a fraction of the bound.

DICT CAPACITY LANDED 2026-08-17 (bake-chain-21, commit b614673b
"layout: size both region bands for the composite they hold" on
de5dcdd2 on the stack tip 3210c5a5). The ruling's three parts and
the probe all landed together.
THE RECORD, measured through the real MAKI-TEST harness over all
193 files of `bin/hb --load maki/test.f` (the merge-gate command,
which loads them into ONE image; the gate's four slices are
smaller and all four passed before and after: 28626 / 20637 /
20315 / 15985 seeded):
  boot ndict        host 7160   product 14052   (seed = +6892)
  inventory ndict   host 26419  product 33131
  inventory code    host 4,047,032  product 5,234,436 bytes
The seed delta is 6892 at EVERY one of the 193 files - it
publishes its window once. End to end it is 178 smaller, and the
step is at exactly one file: maki/onnx/asm-collide-test.f
requires chain sources the seeded engine already provides, so the
unseeded run publishes records the seeded run does not. One step,
not drift.
LANDED: DICT-CAP 32768 -> 65536 (33131 +25% = 41414, smallest
power of two), CFSTK-OFF $300000, DICT-SIZE $301000, HIDX-SLOTS
$20000, HIDX-BYTES $80000, HIDX:LOAD-MAX $18000, REGION $800000
-> $A00000, and CODE-BAND:BYTES = REGION - DICT-SIZE. Six
`DICT-CAP MOVZ,` sites in habu2.f and five in bootstrap/cg/
forth.fs became LIT64 - including the two `DICT-CAP 1 -` sites
that still FIT imm16, because a site that works only while the
cap happens to fit a 16-bit field is a lucky value. LIT64 emits
ONE instruction for a single-chunk value, so the sites cost what
MOVZ cost; the size gate confirms it (engine 165367/3649399
unchanged, gate-size-attribution green with no ratchet bump).
THE CONSTANT IS PACKAGED, not global: the package diff lint
refused `CODE-AREA` at layout.f:140 by name, which is the same
liveness this lane already paid for with AOT-SIG. It is
`package CODE-BAND / public / REGION DICT-SIZE - constant BYTES`,
the shape `package HIDX` already uses two hundred lines below.
THE PROBE (Q2) IS tools/region-room-probe.f + test/region-room-
suite.f, registered as SUITE region-room in gate-stdlib-cases.f
AND selected by SUITE-LINT-TOOLS-LABEL? in gate-stdlib-lib.f. The
second half was NOT optional: with the registration alone,
tools/lint/schedule-lint-test.f named it - "SUITE region-room is
unscheduled: no slice predicate selects its label" - exactly the
dark-suite hole the 2026-08-15 lesson describes. Both floors are
SOURCE-HEADROOM-PCT, so the guard and the sizing rule are the
same rule; REQUIRE-ROOM ends maki/test.f beside the sibling
PROT-WID-PROBE call and prints
`region headroom: dict 33131/65536 records, 32405 left (floor
13108); code 5234436/7335936 bytes, 2101500 left (floor
1467188)`.
FIVE MUTATIONS, all run: FLOOR-OF as cap/4 reds the two boundary
asserts; CODE-USED without DICT-SIZE reds the origin assert;
DICT-CAP back to 32768 reds four; SOURCE-HEADROOM-PCT 25->20 reds
the historical case; REGION back to $800000 reds the code case.
THE FOURTH MUTATION CAUGHT A TAUTOLOGY IN MY OWN TEST and is why
the suite has a CODE-CASE: the first LAYOUT-CASE asserted
CFSTK-OFF + cfstk + CODE-BAND:BYTES = REGION, which RESTATES
CODE-BAND:BYTES' definition - REGION went back to $800000 and the
suite stayed green. What binds REGION is the sizing rule over the
code band's own composite, and that case also keeps the lane's
near miss as a test: the dictionary lift ALONE, inside the old
region, leaves a code band this guard refuses.
GATES ON b614673b: install fixpoint x2 byte-identical (bin/hb
58bd8a1f..., hb-host 8ddfbebd...), artifact converged
65ad95fa..., "two processes wrote the same artifact"; the
merge-gate `bin/hb --load maki/test.f` rc 0 on the PRODUCT (was
rc 77) and rc 0 on the host; reloc-proof, icode-fixup-test,
bootstrap-codegen-test, gate-size-attribution green; both diff
lints exit 0 (package lint liveness proved by its own refusal);
error-code-lint 0 findings, dot-dep-lint 0 findings, schedule
lint 0 findings.
FULL BATTERY test/run.f: ENGINE RC=1, 3:40 wall, SIX RED - the
SAME six as the handoff, with maki now green throughout:
  RED native checker diagnostics repair slice   (leaf C)
  RED build-fixpoint-fixtures                   (leaf B)
  RED GROUP: stdlib/tool-doc                    (leaf C)
  RED native checker diagnostics group          (wrapper)
  RED native engine build slice                 (wrapper)
  RED native stdlib tail slice                  (wrapper)

THE MASTER LINEAGE TRAP, and it cost me a recovery (read this
before any rebase). `master` is a DOTS-ONLY ruling lane -
3a6c58a5 -> 48651132 -> a81b4fa6 -> 826d2187 -> f272ec21 ->
3dd55e61 - and it contains NONE of this lane's implementation
stack: `ancestors(master) & (3210c5a5 | 9219ec56 | 136ca999)` is
EMPTY. `jj rebase -s @ -d master` therefore does not "catch up";
it MOVES the working copy off the stack onto a tree with no
seeded-signature work at all, and the first symptom is quiet -
SOURCE-ARENA-CAP reading $400000 where the lane's own 9219ec56
put $800000. The leaf's rulings and the lane's code are on two
different lines: merge the leaf TEXT (union: this lane's version
plus master's ruling blocks) and rebase code onto 3210c5a5,
never onto master.

RECORD FOR LEAF (B), build-fixpoint-fixtures, captured from the
battery's own pool log (gate-stdlib-tail/hb-pool-*/pool-0-7-21-
out.log) - 10 failures, TWO classes, red on BOTH engines:
  asserts 41 42 46 47 49 62  "expected true got false" - six
    CONTAINS pins over the EMITTED STAGE2 SOURCE that are now
    ABSENT (BFT-TEST-STAGE2-SOURCE and its neighbours). Absent,
    not present-and-wrong: something the pins expect to be
    emitted no longer is, or moved.
  asserts 148 149 155 156    the snap child exits 82 with
    `hb: AOT call site unresolved` where 0 was expected (case
    label "snap trailer"). That message has ONE producer,
    src/habu/habu2.f:4645, beside "AOT call site names a wordlist
    outside the window".
  Between the two, `certify: cert-blocking rejected rc 70` and
  `habu: in bft-cert-bad2: at 'drop' expected: n actual:` are
  EXPECTED negative-fixture output, not failures.
The assert numbers are run ORDINALS, so the first job is to map
them to their pins (count, or label them) - do not guess from the
file. If the snap child's 82 traces to a real seed/snap
interaction, CHECKPOINT with the bytes: that is the snapshot-verb
ruling's ground (snap builds from the host) needing its wiring,
which increment 3 carries.

RECORD FOR LEAF (C), the diagnostics repair slice: the fork
worker throws -2201 (E-FS-OPEN) AFTER the last passing case
`byte-ptr store-target class`, i.e. entering test/gate-
diagnostics-lib.f FILE-ORIGIN, which is the first case in that
run to WRITE A SOURCE FILE (WRITE-SRC -> PATH! -> GT-PATH, rooted
at the GT-START tmp root) and then hand the PATH to check.f
rather than stdin. Every earlier case is stdin-only. WHY-THREW
dumped SB fill=17, CK fill=0, CK-ROW fill=57. That is the whole
of what is measured; the tmp-root-versus-fork question is
UNANSWERED and must be dumped, not reasoned about. The task
contract stands: diagnose to a verdict, and if it is genuinely
pre-existing, prove it with a control build of master's tree in
this workspace (the 2026-08-17 lesson) and DOT it rather than
fixing out of scope.

STILL OWED: leaves (B) and (C); the battery to honest zero (or to
red-equals-dotted-preexisting with master controls); judge
--check; the remaining tool lints; THE NUMBERS OF RECORD on an
idle box (bare / source-chain / baked-chain / baked-no-chain /
install, 7 runs, high-low dropped; record to beat 258.3 / 1352.8
/ ~350 / ~262 / 6.74s->16-17.6s - this lane measured install
16.8s twice with the grown bands); the certified walk (4474 ->
4574 -> current, attributed); lifting increment 3's BLOCKED
marker LAST, only on an honest-green battery; and the closing
package.
ONE THING THE NEXT WORKER SHOULD RE-PROVE RATHER THAN TRUST: I
could not find `region-room: ok` in any surviving pool log after
the lint-tools slice passed, so the suite's presence IN the gate
rests on the schedule lint's answer, not on observed output. The
2026-08-15 lesson's own recipe - break one assertion and watch
the full gate go red - has NOT been run for it.

PUBLIC-SIGNATURES RULING 2026-08-17 (2f9cc56d; the tool walks
the LIVE engine, so a seeded product publishes 386 defs where
the host publishes 13 - misattributed provenance, not a cap
problem; cap growth stays refused): THE LOAD-TIME HIGH-WATER
BRACKET - the tool publishes families created AFTER its scan's
load began, the same bracket discipline the whole campaign uses.
That IS "declared by this load," structurally - the
declared-in-file parse would be new machinery answering the same
question by text. The diag gate's golden follows the contract:
update it WITH attribution (each removed row named as
seed-provenance), never silently.

BAKE-CHAIN-22 CLOSE 2026-08-17. Three commits on 2ca699d4:
ef22bee0 the region-room fork, a413b3a8 the build-fixpoint
fixtures, c1145b21 the code ratchet.

(1) THE REGION-ROOM SUITE WAS DARK and the owed break-and-watch
found it. One inverted assertion, full battery, same six reds, no
pool log naming it. REASON: phase 17 is RESIDENT, so test/run.f
forks test/run-worker-stdlib.f -> GSI-LINT-TOOLS ->
test/gate-stdlib-lint-tools.f, and that body's fork list is the
only thing it reads; a SUITE row plus a label predicate schedules
the STANDALONE `test/gate-stdlib.f -- lint-tools` run and nothing
else. tools/lint/schedule-lint.f SLICE-AT counts a slice live
from PHASE-SLICE-TOKEN without asking TEST:PHASE-RESIDENT?, so it
answered covered. Measured: 233 registrations, 8 label-only under
a resident-only slice (tool, check-cli, lint-tools), 7 of them run
anyway through the GSI body's own call, region-room genuinely
dark. Fixed with a fork in the GSI body; row and predicate stay.
Proven both ways: broken -> `RED: lint-tools/region-room`;
restored -> `PASS: lint-tools/region-room (31ms)` in the full
battery. Lint hole dotted habu-schedule-lint-resident-8b020630.

(2) LEAF (B) IS THREE ROOTS, none the one the handoff named.
Ordinals mapped by printing T-CASES per subtest: 41/42 `stale seed
install`, 46/47/49 `cert inject install`, 62 `chain stamp fold`,
148/149/155/156 `snap trailer`. `stage2 source` is 92-116 and was
green throughout.
 (B1) Both sandbox subtests died `-2102` with an EMPTY tmp:
 BF-STAMP-MATCH? calls CHAIN-RECORD first, ENTRY$ is
 tools/aot-chain-capture.f, and the sandbox copied only
 build-fixpoint.f's require closure. It now copies ENTRY$'s
 closure, asked from the tool.
 (B2) The key writes `capture-src`; FRAGMENT$ searched for
 `chain-src`. The positive failed, the negative passed for the
 wrong reason. Both take BF-STAMP-CAPTURE-TAG$ now.
 (B3) BFT-SNAP0-BUILD built from `hb-stdin`, the seeded product;
 the build uses `hb-host` because a restore skips the AOT seed.
 Exit 82 `hb: AOT call site unresolved` was the seed refusing
 correctly about an engine production never asks for. Both ask
 BF-SNAP-ENGINE$ now.
 MUTATIONS, each alone, each redding only its own asserts: drop
 `ENTRY$ COPY-CLOSURE` -> 41/42/46/47/49; drop the capture digest
 from the key -> 62; BF-SNAP-ENGINE$ back to hb-stdin ->
 148/149/155/156 with the same 82. Fixture rc 0 in 40s.

(3) LEAF (C) IS ONE DEFECT, IT IS OURS, IT IS DOTTED, AND IT
BLOCKS THE CLOSE. Code is -2201 E-STR-CAPACITY (not -2102) and the
case is PUBLIC-SIGNATURES (not FILE-ORIGIN, which is in the
file-unsafe slice and green). PS-EMIT-REGISTRY walks TFAM-N@ over
the LIVE engine and stamps every public family's rows with the
SCANNED FILE: seeded, examples/llm/good.f reports 386 definitions
in 60,491 bytes; the same tree's bin/hb-host reports 13 in 1,982.
GT-OUT-CAP is 32768, so the diagnostics repair slice and GROUP
stdlib/tool-doc (tools/public-signatures-test.f, 24 failures on
the product, GREEN on the host) both overflow. NOT pre-existing.
The fix is a product choice - load-time high-water base vs
declared-in-file filter, cap growth refused - so it is
CHECKPOINTED: habu-public-signatures-publishes-2f9cc56d.

(4) THE CODE RATCHET, attributed by building each commit and
diffing its HABU_ENGINE_SIZE_MAP against its parent's:
135488 -> 281da640 135560 (+72: +52 compile/exit for
AOT-SIG:INSTALL,'s thirteen instructions, +20 aot-seed for the
CK-AOT-REG-INSTALL name) -> 2ca699d4 135648 (+88: interpret/define
+16, primitives/find +16, find-wl +8, find-used +8, hash-index
+24, qualify-def +16 - the six phases that materialise the raised
caps). container/text-pad 7872 -> 7712 absorbs all 160 in the same
16 KiB page, so the file stays 165367. MACOS-CODE-TEXT ->  135648,
MACOS-FLOOR-DIST -> 8672; manifest self-check and the live
SIZE-ATTR coupling both green.

(5) NUMBERS OF RECORD, idle box, 7 runs each, high and low
dropped, `--load` of a one-line program:
  bare (hb-host, empty)            251.4 ms  [was 258.3]
  source-chain (hb-host, require) 1255.2 ms  [was 1353.3]
  baked-chain (bin/hb, require)    261.0 ms  [was 353.2]
  baked-no-chain (bin/hb, empty)   259.8 ms  [was 261.1]
THE BAKE REMOVES 994.2 ms PER CHAIN BOOT (-79.2%); the seed costs
8.4 ms on a boot that never names a chain word. Through a real
consumer (same require plus `using A64ASM : BENCH-T ( n -- n )
ENC-B ;`, which drives the lazy intake): 1234.2 ms host vs 256.3
ms product, -977.9 ms. Install --force 16.80 s then 16.89 s,
byte-identical twice (bin/hb 58bd8a1f..., hb-host 8ddfbebd...),
artifact fixpoint held both times.
CERTIFIED WALK, host census per commit: 9e9263e7 4571 | 331a549a
4571 | 281da640 4574 | c4e108d8 4574 | 08f715cd 4577 | 136ca999
4578 | de5dcdd2 4578 | 2ca699d4 4578 | current 4578. The 4474 leg
is below this stack's base and was NOT re-measured: master's tree
will not build under this lane's engine (bin/hb and bin/hb-host
both exit 82 with empty stdout and stderr), so that control needs
master's own bootstrap engine.

(6) BATTERY 6 RED -> 3 RED. Newly green: build-fixpoint-fixtures,
native stdlib tail slice, native engine build slice. Still red:
the diagnostics repair slice, GROUP stdlib/tool-doc, and the
diagnostics group wrapper - all three are 2f9cc56d.
OTHER GATES: `bin/hb --load maki/test.f` rc 0 on the PRODUCT;
judge --check 46 rows agree; error-code, stdin-closure, namespace,
dot-dep and schedule lints 0 findings; both diff lints exit 0.
INCREMENT 3'S BLOCKED MARKER IS NOT LIFTED - the battery is not
honest-green, which was the condition. It lifts when 2f9cc56d
lands.
