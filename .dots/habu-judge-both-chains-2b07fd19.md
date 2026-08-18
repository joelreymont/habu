---
title: Judge both chains on one shared corpus
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-05T10:36:19.649103+02:00\""
---

Claim: agent=judge-1 workspace=.jj-ws/habu-thecut

WHAT IS LEFT ON THIS LEAF is the post-cut DELETION of the old comparison
harness. The judge is built and covers every corpus, and the deletion plan plus
the four things the old harness still uniquely provides are at the foot of this
leaf.

MEASURED 2026-08-12, master 326b1216 - two claims once on this leaf are OUT OF
DATE and the lane is not to act on them:

- "CALL-FAN-BIG 88-vs-36" is now a DRAW at 40 bytes against 40. The committed
  tables say so (test/compiler/codegen-compare-baseline4.txt:72,
  test/compiler/codegen-chain-baseline4.txt:90) and a live run agrees.
  src/compiler/native/combine.f folded each copied `3 * 5 +` into one
  multiply-add and the trade closed.
- The known-loss name exemption is EMPTY: tools/codegen-compare-report.f
  KNOWN-LOSS$ is `s" "`, so every byte loss on every row is already a counted
  finding. There is nothing for its deletion to surface, and it dies with the
  old harness after the cut rather than being reworked now.

WHAT THE JUDGE IS NOW. tools/judge/src.f reads a corpus source file structurally
and derives each definition's text, arity, callees, storage words and output
kinds. tools/judge/chain.f compiles that text through the chain and records the
routine or the MEASURED refusal code. tools/judge/pass.f holds the measuring
every corpus shares - the row texts, the three columns' call texts and the four
passes - so a corpus file states its rows and nothing else; the same apparatus
used to be copied into each corpus file, and three more copies of it was not an
option. tools/judge.f writes test/compiler/judge-baseline.txt, which the gate
checks byte for byte through tools/judge-test.f.

ALL FIVE CORPORA ARE IN THE TABLE: forty-six rows, which is one row for every
one of the forty-six cases the old harness measures (10 + 7 + 10 + 12 + 7,
counted from the CODEGEN-COMPARE:MEASURE calls in tools/codegen-compare-cases*.f
- the only subject not carried over is CODEGEN-CORPUS:NOOP, which is that
harness's calibration row and not a case). Three rows are refused: two with
E-A64RA-SPILL (-8508, dot habu-spill-from-a-4145325c) and CODEGEN-CORPUS2:SYM-
FOLD-C with E-NFEED-LITERAL (-8405, dot habu-record-the-engine-79c570ed). None
is larger than the engine's and none disagrees on the answer.

SYM-FOLD-C IS A DELIBERATE DISAGREEMENT with the old harness, which reads that
row green, and the artifact's head says so. The corpus writes the checker's own
body with $41, $5A and $20 in it and the chain declines that text; the old
column does not compile it - tools/codegen-compare-migrated2.f:100 respells
those literals in decimal and admits it at its own line 28 - so green there is
measured against a program the corpus does not contain. The corpus is not
respelled to buy the row back, and tools/judge-test.f now fails on any refusal
whose code is neither of the two named above, so a third capability gap cannot
arrive unnamed.

THE COST COLUMN, the MEMORY WITNESS and the PROJECTIONS. Each column's timing
and value body is generated from the row's ONE pinned input text, certified by
the checker and compiled, so a REFUSED subject simply has no body built and the
three-way input duplication is gone. Every generated body is held against the
address of the column it was built for, which is the one way a body can be wrong
that comparing answers cannot see. A subject that leaves a double or a flag is
projected through the one word the comparison already records that kind by
(CODEGEN-COMPARE:REAL-BITS, and FLAG-BITS beside it, which VECTOR-FLAG now
calls), and which projection a row needs is read off its own stack comment. A
subject whose point is a STORE carries a reader for the memory it wrote, and
that reader's answer is a SECOND compared number rather than a fold into the
first - CELL-BUMP answers exactly what its cell holds, so folded together the
two are identically zero and prove nothing.

THE DIFFERENTIAL ORACLE LANDED. tools/judge/fuzz.f generates straight-line
integer programs from a CONSTANT seed, hands ONE text to both code generators
through the same reader, and requires the same cell back on MIN-INT, MAX-INT,
-1, 0, 1 and generated inputs. tools/judge-fuzz-test.f is the scheduled member
(a few seconds, and a prefix of the sweep); `bin/hb --load tools/judge-fuzz.f`
is the hand-run sweep, named in the artifact's head, which measured 256
programs and 9472 answers with no disagreement. The member also proves the
comparison can SEE a difference, by handing the two columns two texts that
differ by one literal. The artifact's head states the oracle's blind spot: both
columns read ONE text, so a reader that derived the wrong program hands the same
wrong program to both compilers and no differential test can notice.

STILL OPEN: THE CUT. After it, delete tools/codegen-compare*.f except the files
the judge itself loads - cabi, macho, clang, cc, core, and the five corpus files
- along with test/compiler/codegen-compare-baseline*.txt and
test/compiler/codegen-chain-baseline*.txt and their suite registrations in
test/gate-stdlib-cases.f and test/gate-stdlib-inline-lib.f. Nothing outside the
harness reads those baselines; the only references to the family from src/, test/
and the other tools are prose comments naming the CORPUS files, which survive.

WHAT THE OLD HARNESS STILL UNIQUELY PROVIDES, measured by reading it rather than
assumed, and each of these has to move or be deliberately dropped IN the cut:

1. DATA-STACK TRAFFIC per row. tools/codegen-compare-test.f:962 DS-TRAFFIC
   counts the stores and loads each column's emitted code makes against the
   caller's data stack, and the member pins it row by row (see its own head:
   that is what stands in place of the cost assertions it deliberately does not
   make). The judge has no traffic column at all.
2. SEVERAL INPUTS PER SUBJECT. The old harness records 23, 28, 47, 45 and 29
   output vectors across the five corpora; the judge pins ONE input per row plus
   the memory witness. CODEGEN-CORPUS2:WS? is checked on five bytes there and on
   one here. The oracle covers GENERATED programs, not corpus subjects on
   several inputs, so this is a real reduction and not a coverage swap.
3. THE CLANG OBJECT ACCOUNTING. tools/codegen-compare-report.f:937 reports the
   reference object's whole __text size and the literal-pool bytes that belong
   to no one twin. The judge reports per-twin bytes only.
4. THE COST COLUMN AS A RATIO to a calibration call, which survives being
   regenerated on a faster host. The judge prints absolute nanoseconds with the
   measurement's own floor subtracted, in its unchecked half, plus the spread
   the run measured. Also tools/codegen-compare-timed-test.f, the hand-run cost
   assertions, which no suite schedules.

CG-27 + CG-28, transition evidence. Old subjects live in tools/codegen-compare-corpus*.f while new subjects are hand-copied strings in tools/codegen-compare-migrated*.f with inputs/results repeated; the gate compares finite recorded vectors and its own tests fabricate comparator rows. Corpus 4 compiles 11/13 rows, and name-based known-loss/unsupported exemptions (compare-report.f:519-575,625-633,675-679) let the gate exit clean despite CALL-FAN-BIG 88-vs-36 and two uncompiled rows. Fix: compile one canonical source artifact through both chains and judge against an independent semantic oracle or property set; committed gaps and size losses are explicit failures or explicit raw measurements, never name exemptions; add adversarial inputs (MIN-INT/MAX-INT overflow boundaries, seeded random bodies, spill-pressure words). After the cut, delete the old-vs-new harness and keep the oracle, a compact production corpus, the new chain's committed baseline, and the optional clang reference.

SCOUTED 2026-08-18 (full map with file:line in the scout's report; corrections
verified against the tree). The deletion is 27 files + the 10 committed
baseline tables + the SUITE codegen-compare block + two GSI-FORK-INCLUDE lines
+ docs/codegen-parity.md. THREE CORRECTIONS to the keep list above:
1. KEEP -text.f — -macho.f:62 requires it; the old list omits it and the judge
   breaks without it.
2. KEEP -migrated{,2,3,4,5}.f — they are NOT compare members: four scheduled
   inventory suites (compiler-codegen-{branch,loop,combine,callsite}-inventory)
   and four hand-run tools walk them as an independent population of
   chain-compiled routines. Rename them out of the codegen-compare- prefix at
   deletion time.
3. tools/codegen-spill-probe.f:106 requires -cases4.f but only ever names
   CODEGEN-CORPUS4: words — repoint that require at -corpus4.f in the same
   commit the case lists die.
Also stale: this leaf's claim that only prose references remain is false —
test/run.f:9, test/run-files.f:77, two fork probes, the spill probe and the
eight inventory files hold live requires (all landing on the corrected keep
list). KNOWN-LOSS$ is already gone from -report.f.

PORT BEFORE DELETE (the capabilities the judge lacks, sharpest first):
a. The Mach-O symbol reader's adversarial tests (-clang-test.f:6-17) — they are
   the ONLY coverage of -macho.f, a file the judge keeps calling
   (judge/pass.f:359). They move to a judge-side test, not to the grave.
b. The chain-baseline direction check (grew = finding, shrank = not); the
   judge's byte-for-byte artifact check conflates the directions.
c. The cost-direction assertion (chain not slower than the engine, per-row
   against a committed table as a calibration ratio) — the judge prints costs
   and checks none; this leaf already owns bringing the cost columns over.
d. The per-row data-stack traffic column (-test.f:962-969).
e. The clang object accounting (whole __text + orphan literal-pool bytes,
   -report.f:939-945) — decide keep-or-drop explicitly, with the several-
   inputs-per-subject reduction (23/28/47/45/29 vectors -> 1 + memory witness)
   named in the report either way.
Deliberate drops, no port: CODEGEN-GAP vocabulary, COVERAGE-CK, --update paths.

SEQUENCING RULED: this does not wait for the cut. Port a-d to the judge, then
delete the 27 in the same series; the corpora keep their files (rename then or
at E). Two code lanes are saturated today; this is next in the queue.

LANDED 2026-08-19 (judge-1, .jj-ws/habu-thecut). a, b, c, d and the first half
of e are in the tree. Each was gated on its own commit: install --force
fixpoint byte-identical twice, the whole of test/run.f rc 0 with no FAIL or
RED, maki, `tools/judge.f -- --check` at 46 rows, schedule-lint 0 unreached,
both checked diff lints, error-code-lint and dot-dep-lint.

a. tools/judge/ref-test.f. The listings moved unweakened and the tokenizer
   cases with them. Mutation-checked three ways against the reader: a substring
   linkage match, a dropped underscore strip and a reader that trusted nm's own
   order each turn it red.
b. tools/judge/base.f reads a committed artifact back as ROWS - structurally, a
   line is a row only with the whole shape of one - and adjudicates each column:
   chain bigger is a regression, smaller is progress, the engine moving either
   way is a finding, a row on one side only is a finding. The byte-for-byte
   check still decides the exit code, so nothing is weakened; what changed is
   that a disagreement now names the rows and their directions. Fixtures in
   tools/judge/base-test.f, attacked both ways plus prose built to fool it.
c. tools/judge-timed.f, listed in no suite - the old flake ruling is kept and
   the file says so. THE BAND IS NOT THE OLD COST-BAND. Eight is calibrated for
   a live measurement against a number recorded on another host on another day;
   against two columns of ONE interleaved pass it would admit a chain eight
   times slower and call it level. The band used is the noise the run measured
   for itself (JUDGE-ROW SAMPLE's worst same-program gap), which widens by
   itself under load because it is measured under that load. Measured: the
   claim reverses to 46 findings when the direction is flipped, so it is not
   vacuous - the tightest row still has about 2.3x of margin.
d. tools/judge/traffic.f, and the two columns are in the CHECKED half of the
   artifact, so every row is pinned rather than the dozen the old member named
   by hand. The counts reproduce the old harness's pinned numbers exactly.
   ONE ROW IS HEAVIER: CODEGEN-CORPUS4:CALL-FAN-BIG, where the engine CALLS its
   callee five times and the callees do the stack work in their own routines
   while the chain copies the bodies in. Named in the artifact and in the
   member, not hidden.
e-FIRST-HALF. The clang object accounting is PORTED, and widened: the artifact
   now prints the whole __text, HOW MUCH OF IT THE ROWS NAME, and the orphan
   literal-pool bytes. The middle number is new and is the honest one - on this
   host 5124 bytes of __text of which the rows name 4324, plus 530 of pool.

e-SECOND-HALF IS DECIDED AND IS NOT DONE: THE SEVERAL INPUTS MUST BE PORTED.
The drop cannot be argued honestly. What the extra inputs buy is the arms the
timed input does not take - BYTE-SUM on an EMPTY span, BYTE-FIND on a MISS,
FACT on 0, COUNT-DOWN on a negative turn count, MAX-F on a NaN and on -0.0,
LADDER on each rung - and that is exactly the class a single input cannot see.
The differential oracle does not cover it: it generates PROGRAMS, not inputs
for corpus subjects, as this leaf already says. So: PORT, and the next lane owns
it before the case lists die, because the vectors live in the files being
deleted.

THE DESIGN, MEASURED AND PART-BUILT (kept out of the tree rather than landed
half-done). A row states extra inputs with a new JUDGE-PASS:ALSO between
tuples:

    s" ADD3" s" hc1_add3" JUDGE-PASS:ROW!
       s" 1 2 3" JUDGE-PASS:IN+
       JUDGE-PASS:ALSO s" -5 5 7" JUDGE-PASS:IN+
       row execute

tools/judge/pass.f holds HABU-IN and REF-IN as IN-MAX tuples with their own
length cells; IN+ and STORE+ append to the tuple ALSO last opened; VALUE records
tuple 0 in the row as today and COMPARES the rest through all three columns
(answers and the memory witness), marking the row on any disagreement; TIME uses
tuple 0 alone, so no cost and no committed byte moves. judge/row.f gains
ALT-BAD (consulted by AGREES?) and INPUTS/TOTAL-INPUTS, and the artifact's tally
gains `pinned inputs compared: N` so an input quietly dropped moves the
committed file. The answers are NOT committed: what these inputs establish is
that three independently compiled programs agree, which nothing has to keep in
step by hand. IN-MAX must be at least 6 - CODEGEN-CORPUS4:LADDER has six rungs.
The old vectors to move are in the VECTOR bodies of
tools/codegen-compare-cases{,2,3,4,5}.f (23/28/47/45/29). Corpus 3's extra
inputs need care: two of them name S-VEC and Z-VEC, which have no C twin in
tools/clang/twins.c, so those rows either gain a twin or state their extra
inputs over the buffers that do have one.

STILL OPEN AFTER THIS LANE: the deletion itself (step 4), unstarted. The
27-file set is confirmed by count against the tree, and the require closure is
CLOSED - after the corpora, cabi, cc, clang, core, macho, text and
migrated{,2..5} are kept, the only live require of a doomed file from outside
the family is tools/codegen-spill-probe.f:106.

ONE MORE CORRECTION TO THE SCOUT'S LIST, measured: correction 3 is incomplete.
Repointing the spill probe at -corpus4.f alone BREAKS it. The probe names
CODEGEN-CORPUS4:C-LONG-N at its lines 284 and 400, and that word is published
by tools/codegen-compare-migrated4.f (line 94), which -cases4.f pulled in
through -new4.f. The probe must require -corpus4.f AND -migrated4.f.

Prose that still names deleted files, to fix in the deletion commit:
test/gate-stdlib-lib.f:372-373, test/gate-stdlib-cases.f:386 and 394-402,
test/compiler/native-chain-fixture.f, test/compiler/native-source-fixture.f,
test/compiler/native-inline.f, tools/codegen-workload-test.f and
tools/codegen-workload-timed-test.f (all name -new.f, -test.f or -timed-test.f
in comments), plus docs/codegen-parity.md, which is the harness manual and is
to be rewritten onto the judge rather than deleted.
