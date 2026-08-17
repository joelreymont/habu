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
