---
title: Optimization
status: open
priority: 1
issue-type: task
created-at: "\"2026-09-24T16:53:24.838504+02:00\""
---

Reduce Habu engine and application size, unnecessary generated instructions,
startup storage, and compiler/build cost. This parent collects existing work;
child IDs, status, dependencies and historical evidence are preserved.
Remeasure historical claims on the current product before implementation.
Record confirmed findings in an existing matching dot or a new bounded dot
before fixing them. Update evidence and acceptance before an implementation
claim; an RCA or candidate saving is not completed implementation.

## Current thin-engine work

Parked at the user's request to return to Tender after committing, pushing and
installing the completed non-compression work. Code and DATA compression require
explicit user approval; the pending DATA and CODE compression experiments are
rejected and will not be landed or installed. The additional size target remains
unfinished. Preserve their source revisions and measurement artifacts as
historical evidence, not accepted savings.

The user explicitly set a continuing goal of **1 MB or more of additional
reduction** from the qualified 2,625,655-byte paired engine. The target is
1,625,655 bytes or smaller. Earlier savings below are historical and do not
count toward this additional target. Smaller accepted wins are progress, not
completion. Preserve checked REPL, JIT, AOT, source reconstruction and
capture/restore behavior. Count complete product cost, including new optimizer
code; do not aggregate hypothetical pattern savings or remove state without
proving its semantic roots. Completed bounded measurements cover real capture
literal placement, remaining ARM64 selection gaps and compact startup tables.
Direct boolean masks are qualified (habu-emit-bool-masks-9ba063d8).
Paired stack writeback is qualified (habu-fuse-paired-stack-c1800302).
Packed startup tables are qualified (habu-pack-baked-seed-66e05afd). The broader repeated-code census and
independent source coverage audit are complete (habu-find-repeated-code-7811ef13).
Generated buffer bounds sharing (habu-share-buf-bounds-c8093ca8), fixed
validation/fatal tails (habu-share-fixed-valid-a57b1b73), and dialect schema
finishing tails (habu-share-dialect-schema-d498c791) are qualified together.
Opcode name tables (habu-table-opcode-names-d7cd2487) are qualified through
native schema and restored-image checks. Packed DATA values
(habu-pack-startup-data-0c5b8d64) and
captured code packing (habu-pack-captured-code-54666c9e) are cancelled by user
direction. Preserved source: DATA
`cbfdab2dff57dafa14ceef8a75639253d19f0d7c`, CODE
`89686491ae43fb71101918b2340251abba59768f`. Neither experiment counts toward
the goal; their native images are not releases.

The current qualified engine is **2,444,023 bytes**. Shared opcode-name tables
remove repeated dispatch code and save **16,512 signed bytes**, including all
new table and relocation costs. Additional saving from the goal baseline is
**181,632 bytes**, leaving 818,368 bytes to target. Engine SHA-256:
`89b78737a1af0c75e6e28c34389559e879fa4f4065638fbca77cbac32a5dd165`.
Five generations and name maps are byte-identical; native HIR/A64IR, selection,
session and twice-restored image checks pass. The full registry ran all 492
suites: 491 passed, and the sole failure was Gforth missing its precompiled
libraries in an empty private cache. That unchanged fixture passes with those
runtime libraries supplied. The duplicate full run was cancelled as redundant;
Etch was not rebuilt. Evidence:
`~/.cache/tmp/habu-opcode-names-completion-20260929-01.md`.

The preceding qualified engine is **2,460,535 bytes**. The three source-sharing
changes save another **16,512 signed bytes** from the compact-table parent.
Additional saving from the 2,625,655-byte goal baseline is now **165,120 bytes**,
leaving 834,880 bytes to target. Engine SHA-256:
`c21bf57f366a4f138d0af805f3db7e744c7bf31247ecab0064ae672182c21a84`.
Generated buffer accessors and their generators save 13,144 code bytes including
new helper costs; fixed native validation/fatal tails save 968; typed schema
finishing tails save 912. Complete payload saves 15,764 bytes, container padding
saves 620 and signatures save 128. The two tail changes individually save zero
file bytes because alignment absorbs their payload reductions. Each feature
passes B1/B2 identity and focused native checks; the final combined source
`dc3bea8d686d06ef87d4ed9639573966894eb56a` passes five identical generations/names,
all 492 suites, restore/recapture paths and strict signatures. Independent reviews
cover each feature. Etch saves 49,248 bytes to 19,978,688; both actual-stdin board
exports remain byte-identical. Evidence and replay:
`~/.cache/tmp/habu-source-sharing-completion-20260928-01.md`.

The preceding qualified engine is **2,477,047 bytes**. Packing the dictionary,
bound-call and DATA-site startup tables saves **132,096 bytes** from the paired
writeback parent. Every row, field, order, name, WID and alias is preserved;
reusable AOT13 and snapshot10 remain unchanged. At that step, additional saving
from the 2,625,655-byte goal baseline was **148,608 bytes**, leaving 851,392 bytes
to target. Engine SHA-256:
`1e16786cb66df805dcbdef714928630b658cfe19b85d322e3a7c461bcce03f22`.
Table sections save 134,672 bytes including new headers/padding; native decoder
code costs 1,984 bytes, captured code is unchanged, container padding costs
1,616 bytes and signatures save 1,024 bytes. Independent reviews, five identical
generations/names, all 492 suites, 25 signed corruptions, empty cold boot and
complete semantic/physical row accounting pass. A transient name-entry bitmap
removed an introduced repeated scan; 40 alternating process pairs measured
6.241500 ms parent versus 6.352250 ms candidate (+0.110750 ms). Etch saves 131,328
bytes to 20,027,936; both stdin board exports remain byte-identical. Qualified
source: `1ff58c3d7e36b4a6c55b325d988bb11df51cffd0`. Evidence:
`~/.cache/tmp/habu-compact-seed-completion-20260928-02.md` and
`habu-compact-seed-review-20260928-03.md` in the same directory.

Eligible paired stack stores now consume their planned pointer adjustment as
STP post-index, preserving the enclosing call. That preceding signed engine is **2,609,143
bytes**, down **16,512 bytes** from the additional-goal baseline. Generated code
falls 16,468 bytes and nonpadding payload falls 16,072 bytes from CSETM. This is
16,512 bytes toward the additional 1 MB target, leaving 983,488 bytes to target.
Engine SHA-256:
`5907200e52b43a55a24e27814dca4d82600a3d234923c3cad0e2dddc5763526c`.
Independent reviews, B2–B5 engine/names identity, all 492 suites, selected-form
order/call/sentinel and guard-page checks, five assembler vectors, ten refused
encodings and strict signatures pass. Etch falls 32,832 bytes to 20,159,264;
both actual stdin board exports remain byte-identical. Qualified source:
`451d5c0f1ee7ec721519dd6a9a6d194a8d161080`. Evidence:
`~/.cache/tmp/habu-pair-writeback-completion-20260928-01.md` and
`habu-pair-writeback-review-20260928-02.md` in the same directory.

The four native comparison forms now emit CSETM directly instead of CSET then
NEG, preserving exact 0/-1 flags and floating unordered behavior. Generated code
falls 9,036 bytes and total nonpadding payload falls 8,900 bytes. The signed
engine remains 2,625,655 bytes because alignment absorbs the saving: this adds
zero file bytes toward the additional 1 MB target. Engine SHA-256:
`830c33d0af20d4202de584b62a94d63953d2b2f1f1af19830b0296a606ff2821`.
Independent review, all 14 encoder vectors, B2–B5 engine/names identity, all 492
suites and strict signatures pass. Etch is 32,832 signed bytes smaller at
20,192,096, with both stdin board exports byte-identical. Qualified source:
`e319fdfe324a31bfca98541a785ea1f4c22c3ed3`. Evidence:
`~/.cache/tmp/habu-native-mask-completion-20260928-01.md` and
`habu-native-mask-review-20260928-01.md` in the same directory.

The repeat census covers 1,388,784 owned code bytes in 13,362 disjoint rows,
exact and normalized 4/6/8/12-instruction windows, and 9,322 short bodies.
The fixed engine prefix and 33,968 blob bytes outside declared body ownership
are excluded. Exact short duplicates occupy 27,988 extra bytes; normalized
templates occupy 154,692. Neither proves interchangeable behavior, identical
absolute callees, independent patching or safe removal. Remaining small local
screens find 108 gross bytes of repeated comparisons and 168 of repeated frame
loads; another optimizer is not justified by these counts.

Source attribution and instruction templates confirm 197 shipped fixed buffer
accessors and 159 dynamic ones. Their repeated bounds and pointer logic occupies
20,268 bytes before replacement wrapper/helper costs. Existing reserve/release
already share their runtime. HIR/A64IR naming ladders occupy 11,188 bytes;
schema-definition families occupy 11,516 before their nonshared portions; 41
derived enum TAG bodies occupy 5,808. These are candidate populations, not
savings. Evidence: `~/.cache/tmp/habu-repeat-code-census-completion-20260928-01.md`,
`habu-generator-census-completion-20260928-01.md` and
`habu-repeat-patterns-review-20260928-01.md` in the same directory.

The initial startup-table census preserved every row and measured a 134,560-byte
representation opportunity for dictionary records, signed bound-call gaps and
signed DATA-site gaps, including framing and alignment. The qualified result
above now includes native decoder, compiler and complete product costs.
Anonymous spans stay unchanged. Initial census evidence:
`~/.cache/tmp/habu-metadata-census-completion-20260928-01.md`.
The ARM64 census finds 4,449 compatible STP-plus-pointer-move sites (17,796
gross instruction bytes), 595 mixed scalar load pairs (2,380), 124 ordinary
unsigned address folds (496), and two floating pairs (8). These independent
screens overlap and lack live IR provenance; they are not additive savings.
Evidence: `~/.cache/tmp/habu-selection-gap-census-completion-20260928-01.md`.

Native stack transfers now use paired GPR64 instructions when both operations
have the same block, full source origin, base and adjacent safe slots. The
qualified engine is **2,625,655 bytes**, down **115,584 bytes** from f805a05f.
Generated AOT code falls **116,100 bytes**, including the new compiler cost.
The 29,887 emitted pairs remove 119,548 instruction bytes directly; this does
not claim every secondary layout difference is attributed. Engine SHA-256:
`da27859f7f4b18137cac8ef4524c34a8137897402e8d11485790cff1d01b6bb0`.
Independent source and fixture reviews, B2–B5 engine/names equality, all 492
suites, guard-page diagnostics and strict signatures pass. Etch falls **295,488
bytes** to **20,224,928**, with **300,340 fewer physical instruction bytes**;
both board exports are byte-identical. Exact qualified source: `430083dcd2ea`.
Evidence: `~/.cache/tmp/habu-native-pairs-completion-20260928-01.md` and
`habu-native-pairs-fix-20260928-01/` in the same directory. Relative to the
2,922,871-byte thin-engine baseline, total file savings are **297,216 bytes
(10.17%)**. This meets the hundreds-of-kilobytes scale, not a 1 MB claim.

Local effect links store record-relative cell spans, preserving every record
and history boundary. That preceding engine was **2,741,239 bytes**, down
**49,536 bytes**: DATA value section −41,964, AOT code +76, with another 8 bytes
of span metadata. Net payload saving before padding/signature is 41,880 bytes.
SHA-256: `6c9d991160f61a0d08a4c82ebfba1e5b80745d45b7b9fb980aa8f78bbf54bcad`.
Independent review, focused E2Es, B1–B5/names identity, all 492 suites and strict
signatures pass. Etch falls 32,832 bytes to 20,520,416; both board exports are
byte-identical. Evidence: `~/.cache/tmp/habu-effect-links-fix-20260928-01/`
and `habu-effect-links-review-20260928-01.md` in the same scratch parent.

Shared DATA literal pools remain a separate candidate. A real source-build
capture census now recovers original dictionary/raw ownership, including retired
records, and the complete graph map. Its instrumented engine and names remain
byte-identical to the baseline. The checked plan selects 14,760 carriers into
three islands with 4,237 cells; 99 carriers without declared executable owners
remain unchanged. All mapped ranges, roots, edges and persisted plan fields
verify. The conditional saving is 128,016 payload bytes, or 132,096 signed-file
bytes with new implementation costs held at zero. No pooled executable or net
implementation saving is claimed. This estimate overlaps the compact startup
tables' DATA-site saving and must not be added to it. Evidence:
`~/.cache/tmp/habu-live-literal-census-completion-20260928-01.md`.
Earlier missing-authority result:
`~/.cache/tmp/habu-literal-placement-completion-20260928-01.md`.
Design and earlier repeatable census:
`~/.cache/tmp/habu-literal-pool-design-20260928-01.md` and
`habu-literal-pool-census-completion-20260928-01.md` in the same directory.

The attempted boolean normalization reduction is rejected: its optimizer costs
1,556 code bytes to remove 1,680 emitted bytes, while other serialized content
grows 200 bytes. Engine payload therefore grows 76 bytes, with file size
unchanged. Etch's 660-byte code saving is exactly offset by DATA/other growth.
The source and patch are preserved in the open boolean dot; none is integrated.
Focused tests, native convergence and Etch smokes passed, but the full registry
was not run after the size rejection. Evidence:
`~/.cache/tmp/habu-native-bool-completion-20260928-01.md`.

Proven nonzero scalar divisors now select a single native division instruction;
unknown and zero divisors retain the guarded throw path. The qualified engine
has **4,968 fewer AOT code bytes** and **344 fewer throw-call entries** (1,376
bytes). Its file remains **2,790,775 bytes** because alignment absorbs the
payload reduction. SHA-256:
`ae41d43ac92b69d6c0a1bc45b92f4f359a77b7766fd991e2fa4930e10b83ca02`.
Etch generated code falls **18,072 bytes** and its executable falls **32,832
bytes**, to **20,553,248 bytes**. Both exported boards are byte-identical.
Independent source review, B2–B5 and names equality, all 492 suites, and strict
engine/application signatures pass. Exact qualified source is `78d21b471ad4`;
evidence: `~/.cache/tmp/habu-native-divisor-completion-20260928-01.md` and
`~/.cache/tmp/habu-native-divisor-fix-20260928-01/`. The divisor dot is closed;
boolean normalization and other open optimization tasks remain unfinished.

The user requested implementation of the remaining engine-size fixes. Tender
is reference material only; its engine, pin, application tests and historical
500 KB target are not dependencies or acceptance criteria for this work.
Preserve Habu's checked REPL, JIT, AOT, source reconstruction and supported
capture/restore behavior. Use the existing implementation dots below rather
than creating duplicate retention tasks. Alder owns integration and closure;
Sol workers own implementation and verification in isolated workspaces.

The preceding product from `6c64049ce625` is 2,922,871 bytes, SHA-256
`19dbc04c14d10add5e9813f563fd115ea3e6e398511ac67e382038bc32f19db0`.
`bin/hb --load tools/engine-size.f -- bin/hb` reports 1,681,480 code bytes,
778,148 DATA/bitmap bytes, 267,536 dictionary/name bytes and 195,707 other
bytes. All dictionary records and anonymous spans are reachable under the
current dictionary-surface roots. Engine-entry roots omit 180,336 code bytes
but do not describe every future compilation dependency; that is not a
deletion list. The baseline passed the unchanged 490-suite registry with two
concurrent children; the default eight-slot run had three load failures that
each passed alone and in the full reduced-concurrency rerun.

The qualified internal-name reduction is **2,856,823 bytes**, down **66,048**
(2.26%), SHA-256
`cf9c706cb70b9898822a5c4f20a13df163ac93a047c645f5fda46f5e657bc5cf`.
It removes 2,172 non-package dictionary records: record bytes fall from
166,640 to 123,200 and name bytes from 90,576 to 64,240. Reachable unnamed
code remains; span metadata grows from 42,840 to 58,792 bytes. The code blob
falls from 1,560,148 to 1,538,952 bytes. These section differences overlap
other build changes and alignment; the total above is the measured file saving.

Guarded declaration-owner callbacks replace real private-name consumers;
the exact dynamic `DEFER-UNSET` root remains. Private inspection uses whitebox
while public tests still use the product. Independent review passed, native
generations 2–5 are byte-identical, and the default eight-slot gate passes all
490 suites. Both Etch routing/geometry and shared-channel smokes pass with
byte-identical accepted boards; engine and Etch signatures verify strictly.
Evidence and artifacts: `~/.cache/tmp/habu-thin-internal-completion-20260928-02.md`.
The internal-name and product/whitebox test dots are closed.

The next capture E2E exposed a native `does>` publication defect: a resident
definer ran correctly but source verification could not recover its created-word
effect. `habu-publish-native-does-10a4b58a` fixes publication through the checker
owner and holds its existing rollback transaction through native publication.
Rejected clauses and later failures restore signatures, control/CREATES, symbols
and extension state; they cannot leave a phantom created word. The public E2E
reproduced both failures before their repairs. Independent review, source-only
replay, native B2–B5 byte equality, all 490 suites and the two byte-identical Etch
board smokes pass. The engine remains **2,856,823 bytes**, now SHA-256
`77270a103f32c6e7422554df65a36fa06680186d01ce50383aeea64c4b76e2b6`.
This correctness prerequisite adds no file-size cost and is the new baseline
for control compaction. Evidence: `~/.cache/tmp/habu-native-creates-fix-20260928-01.md`.

Captured NORET control compaction (`habu-compact-captured-control-4969c5eb`)
preserves primitive, saved-boundary and current states, including clearing rows
and nonzero created-word effects. The qualified engine is **2,807,287 bytes**,
down **49,536 bytes (1.73%)** from the native DOES baseline, SHA-256
`24003f017a601713c84185a7be5b1e0664d9951c942847372d2a0ddb9b0a654b`.
DATA value bytes fall 728,444 to 688,080 and bitmap bytes 51,132 to 47,928;
generated code grows 3,232 bytes. Alignment, metadata and signature changes
also contribute to the measured total. Combined with internal-name stripping,
the engine is 115,584 bytes (3.95%) smaller than `6c64049ce625`.

Independent review passed; native B2–B5 and their names sidecars match byte for
byte. All 491 suites pass. Two retained application images exercise actual
capture/restore, boundary rewind, new rollback and active-scope refusal. Both
Etch boards remain byte-identical and strict signatures pass. Evidence:
`~/.cache/tmp/habu-control-compaction-completion-20260928-02.md`.

These reductions do not prove that remaining DATA is necessary or generated
code is efficient. The current product retains 736,008 DATA/bitmap bytes and
1,543,368 generated code bytes plus 121,100 fixed engine bytes. General effect
history, private symbols, DATA retention and native instruction selection remain
open. Require current physical evidence, native self-host convergence, the full
gate and Etch qualification for each capture change. No estimate or tracker
cleanup counts as an implemented size reduction.

Dots 0.6.4 renders one parent level reliably. Keep these tasks as direct
children. The Tender size campaign remains recorded in
`habu-bring-the-stripped-114f614a`; its associated tasks are
`habu-measure-the-type-c7b88b15`, `habu-remove-bounds-checks-22af10b0`,
`habu-prove-the-closure-5b7d02bb`, and
`habu-validate-frozen-fetch-19e9d5cd`. Closed tasks retain their recorded
outcome, including supersession; closure does not assert implementation
acceptance. The mixed compiler product/correctness campaign remains at
`habu-compile-the-tender-8385810f`, with its optimization leaves grouped here.

Native scalar literal selection now starts MOVZ at its first nonzero half,
preserving MOVN policy and the separate relocation carriers. The qualified
engine remains **2,807,287 bytes**, SHA-256
`7a357464fe741b9359a170b433b54364a431df117f21ee947f2caca25d8044bb`.
Its AOT code blob falls 1,543,368 to 1,543,112 bytes (**256 bytes**); DATA,
metadata and alignment absorb the file saving. Etch generated code falls
1,232 bytes with its total file size unchanged. Independent source review,
B2–B5 and sidecar equality, all 491 suites, strict signatures and both Etch
board smokes pass. The smokes execute through stdin and retain exact accepted
board hashes. Evidence: `~/.cache/tmp/habu-native-scalar-completion-20260928-01.md`.
The scalar dot is closed; the separately measured native shift, divisor-guard
and boolean gaps remain unfinished.

String interning for checker symbol names now preserves distinct identities,
visibility and effects while reusing identical folded name bytes. The qualified
engine is **2,790,775 bytes**, down **16,512 bytes (0.59%)**, SHA-256
`614b033636dab64e95d6195ccb29c8c08d6463765dbd625e603cd9cb2cba1c0c`.
DATA values fall 16,804 bytes and bitmap storage 192 bytes; added compiler code
costs 632 bytes. The live string pool falls 168,780 to 154,056 bytes. This is
132,096 bytes (4.52%) smaller than the preceding `6c64049ce625` baseline.

The runtime index adds 512 KiB per mapping at current capacity. Existing reset
abandons mappings without unmapping them; this change does not repair or measure
that accumulation. One serial uncached build per version took 54.95 seconds
before and 54.85 after, an informational pair rather than a statistical claim.
Independent source review, native B2–B5/sidecar identity, all 492 suites, retained
two-image capture/replay and strict signatures pass. Etch is 16,416 bytes smaller
with both boards byte-identical. Evidence and limits:
`~/.cache/tmp/habu-name-intern-completion-20260928-02.md`.

## Measured baseline

macOS ARM64 product built from master `434fbe1d29bb`; SHA-256:
`d3b3bdfd65c835c4275006bc40274635b7272e22d45591301ddcf62197279f8f`.
Reproduce the physical budget with
`bin/hb --load tools/engine-size.f -- bin/hb`.

| File component | Bytes |
|---|---:|
| Native engine and captured Habu code | 1,656,928 |
| Sparse captured DATA, including alignment | 924,136 |
| Dictionary records and names | 262,008 |
| Relocation, span and framing metadata | 489,276 |
| Mach-O headers, padding, GOT and signature | 52,859 |
| Total | 3,385,207 |

There is no baked source text. Total padding is 25,660 bytes. The captured
DATA span is 8,671,320 bytes; zero cells are already omitted from the file.

## Verified representation reductions

The preceding Mac product is 2,972,407 bytes, down 412,800 (12.2%).
SHA-256: `2daeb34544e8c081209f2437485d76f606c61ca5ac7eafe8cc28ddf438845639`.
Interned checker package names and arena offsets remove 267,024 bytes of
address rows; zero-sentinel scratch removes 51,840 bytes of bitmap/values;
bound primitive instructions reduce the final call table to 38,560 bytes.
The total is measured after changed code, scalar values and Mach-O alignment,
not the sum of gross savings.

Independent review accepted all three changes. Two native generations are
byte-identical, and the integrated native gate passes all 490 suites. The
first attempt timed out during documented host sleep; the unchanged candidate
passed with a process-scoped sleep assertion. Etch's native image falls from
25,675,040 to 25,264,640 bytes; its routed two-filter PCB remains byte-identical,
and the existing negotiated-routing suite passes. External KiCad DRC was not
part of these checks. Commands, signed images, output boards, hashes and logs
are retained at `~/.cache/tmp/habu-opt-names-scratch/RESULTS.md`.

The current verified product is **2,889,847 bytes**, down another 82,560 bytes
and 495,360 bytes (14.6%) from the initial baseline. SHA-256:
`730f69dac961702ea8593685d5f7641df32cf2d8d20725ba996b38f422e63aae`.
Exact immutable effect contents are shared while every binding, history and
authority flag remains; the external 96-byte wire representation is unchanged.
The native emitter also eliminates proven adjacent frame reloads. Independent
review accepted both changes, two native generations are byte-identical, and
the complete 490-suite gate passes.

Etch's native REPL image is **22,687,328 bytes**, down 2,577,312 from the
preceding product. Its raw warm DATA window alone shrinks by 2,495,588 bytes;
no snapshot codec changed. Routing/geometry and negotiation checks pass with
both exported boards byte-identical. Final measurements, failed runs, the
existing TCP/HTTP fixture ordering corrections and repeatable artifacts are
retained at `~/.cache/tmp/habu-opt-round2/combined/RESULTS.md`.

The following storage changes remove the checker boot reservations, give
SPA/TV/SEEN mapped ownership, and omit full-image driver/signers from warm
capture. Etch is now 21,702,368 bytes, another 984,960 bytes smaller; routing
and negotiation pass with byte-identical board exports. The native engine is
2,906,359 bytes, 16,512 bytes larger: sparse DATA already omitted most removed
zero storage, and owner code/alignment increase its physical size. Two native
generations are byte-identical and all 490 native suites pass. The gate's invalid
subject-timeout fixture was corrected without changing its deadline or assertion.
Evidence, including the rejected first run, is retained in
`~/.cache/tmp/habu-opt-round3/integrated/`.

The packed REQUIRE owner is qualified for integration against that storage
product. Five native generations are byte-identical; the new engine remains
2,906,359 bytes while its captured DATA span falls from 6,648,272 to
6,130,400 bytes. Fresh native Etch falls from 21,702,368 to 21,193,472 bytes,
and its raw DATA window falls from 13,252,884 to 12,742,308 bytes. Unchanged
warm recapture adds no DATA bytes; one new provided fact adds 68 live path
bytes and a 16,384-byte raw DATA-window increase on recapture, including
capture overhead and a retained previous pool. The frozen candidate passes all
490 native suites. The packed and prior native Etch images export a
byte-identical routed and geometry-checked two-filter board; the packed image
also passes the retained negotiation suite and exports a byte-identical
shared-channel board. The new engine and Etch image pass strict macOS signature
verification. External KiCad DRC/connectivity, IPC and
map/unmap/DATA-allot fault injection remain untested. The exact source
manifest, executables, logs, sizes and limits are retained in
`~/.cache/tmp/habu-opt-round3/require-pool/RESULTS.md`.

Compression remains held. Snapshot repacking and dropping zero-filled data
are not compression and are not held (Joel, 2026-09-30). General DATA reachability and
former persisted semantic copies remain separate open work.

Measurements before the next reduction: 20,827 effect headers occupy 267,461
encoded bytes while representing 1,766 exact semantic tuples. The effect
allocation costs 321,883 bytes including its bitmap. Code contains 14,758
three-instruction DATA address carriers (177,096 bytes), plus 1,519 adjacent
same-slot/register store-then-load pairs. These are costs and patterns, not
proven removable bytes; source reconstruction, metadata roots and branch-entry
semantics still constrain their removal. The private-symbol and internal-name
dots record newly verified consumers before further stripping.

## Tracked RCA work

The native code linker selects a code/dictionary closure, while persistent
checker capture copies mutable registries without that selection. Startup
copies and links the native payload. Track corrections at those owners:

| Finding | Implementation dot |
|---|---|
| 8,502 private symbols remain without dictionary entries | [Prune checker metadata](habu-drop-private-signatures-974304d0.md) |
| Internal-name reduction qualified above | `habu-strip-the-names-89d6524a` (closed) |
| 9,638 zero-displacement calls use 12-byte target rows | [Bind primitive calls](habu-bind-primitive-calls-a45cdb44.md) |
| Repeated package strings and absolute string pointers | [Intern names and use offsets](habu-store-checker-names-70a89ffb.md) |
| Older effect and control records persist wholesale | [Compact checker histories](habu-compact-checker-histories-3a1ce692.md) |
| 20,825 effect headers represent 1,766 semantic tuples | [Share effect headers](habu-share-effect-headers-d26ffc89.md) |
| Four reconstructible UNBOUND arrays cost 51,840 bytes | [Initialize checker scratch](habu-init-checker-scratch-4c2afab4.md) |

Header sharing preserves every history and has no correctness dependency on
history compaction; the earlier ordering only avoided concurrent representation
edits. Serialize shared checker edits. The other tasks are independently
investigable.
The existing DATA-reachability owner remains
habu-prove-the-closure-5b7d02bb. Implemented reductions are qualified above;
the remaining unimplemented rows describe open work.
The current downstream readiness check is Etch; older task text naming other
applications does not expand this optimization work.

## Priorities supported by current evidence

1. **Represent persistent checker state compactly.** The 16,689 symbol rows
   contain 33,378 string-pointer relocation rows costing 267,024 bytes.
   Every target lies in one 251,306-byte arena. Encoding those exact targets
   as one-based arena-relative offsets would take 97,765 ULEB bytes, a
   169,259-byte representation difference before changed code, bitmap and
   alignment. This is a measured encoding estimate, not a validated saving.
   Package names occupy 87,134 arena bytes despite only 201 distinct strings
   totaling 2,112 bytes: 85,022 duplicated bytes. Packed text itself expands
   under the cell-wise unsigned-varint codec; this arena costs 286,764 image
   bytes including its bitmap. See `src/core/checker.f`:
   `SYM-PKG!`, `SYM-COPY-FOLD`, `SYM-SNAPSHOT-MARK-POINTERS`;
   `src/habu/aot-decl.f`: `CELL-V!` and address-row emission.
   Existing owner: `habu-attr-the-captured-e060c47e`.

2. **Stop serializing reconstructible scratch state.** Four transient
   checker maps contain 5,120 UNBOUND cells: 40,960 raw bytes become
   51,200 value bytes plus 640 bitmap bytes. Restore their required initial
   state at startup or change the sentinel representation; simply zeroing
   them violates reset invariants. See `TV-SNAP-RESET` in
   `src/core/checker.f`. The live effect/signature store separately costs
   about 321,853 image bytes; it is persistent compiler state, not scratch.
   Reuse `habu-attr-the-captured-e060c47e`,
   `habu-persist-registry-arrays-0459b70a` and
   `habu-drop-private-signatures-974304d0` as applicable.

3. **Make DATA reachability part of image closure.** Real stripped builds
   prove word-level code shaking works, but unused initialized DATA survives.
   An unread declared XT cell also roots code. Fix storage ownership and
   reachable DATA together; deleting XT roots alone can remove live code.
   See `BUILD-SPARSE-DATA` in `src/habu/aot-lib.f`,
   `COLLECT-XT-CELLS`/`CLOSURE` in `src/habu/aot-closure.f`, and the
   corresponding whole-window capture in `src/habu/aot-capture.f`.
   Existing owner: `habu-prove-the-closure-5b7d02bb`.

   | Stripped probe | File | Code | DATA values | Bitmap | XT rows |
   |---|---:|---:|---:|---:|---:|
   | Empty MAIN | 33,276 | 2,172 | 427 | 153 | 0 |
   | Plus uncalled arithmetic word | 33,276 | 2,172 | 427 | 153 | 0 |
   | Plus unread 65,536-byte array filled with 65 | 99,324 | 2,172 | 74,155 | 1,179 | 0 |
   | Plus unread typed XT cell | 33,276 | 2,272 | 432 | 153 | 8 |

   All four builds and executions succeed. The array adds 73,728 encoded
   value bytes; page padding accounts for the smaller file delta. The XT
   case's code delta includes relocation support and is hidden by padding
   in its final file size.

4. **Measure code-generation changes against current instructions.**
   Production selects tier 1 before loading dependencies. On the documented
   13-file corpus, 2,328 definitions total 175,544 bytes at tier 0 versus
   173,736 at tier 1 (-1.03%); direct calls fall from 11,659 to 5,444.
   Address materialization and spill traffic offset much of the size gain.
   The actual product contains 14,762 recognized three-instruction DATA
   address carriers, costing 177,144 bytes. Their shared addresses cannot
   simply use the task-local DATA base; inspect `address-carrier.f` and
   `src/compiler/native/emit.f` before proposing a different convention.
   This is a measured cost, not a proven removable total.

   There are 1,517 adjacent same-register/slot store-then-load pairs:
   6,068 bytes of candidate redundant reloads, subject to branch-entry and
   semantic checks. The old 43,153 load-then-store claim is obsolete;
   only three such pairs remain and `MB-IDENTITY-COPY?` already eliminates
   the common case. 248 exact call wrappers offer at most 2,976 bytes
   before tail-call eligibility checks. Of 8,496 framed bodies, only eight
   contain no call: shared does> routine contracts waste 64 frame bytes.
   Existing owners include `habu-elide-same-slot-443de377`,
   `habu-elide-the-three-a87cf770`, `habu-inline-small-colon-2ca2438f`,
   and `habu-cost-a-placement-1f61860d`.

5. **Separate startup memory from file size.** Startup zeroes the entire
   restored DATA span before applying sparse contents. A fresh idle product
   measured 13.3 MiB physical footprint and 8,944 KiB resident in its initial
   DATA mapping. The 1 TiB reservation is virtual address space, not disk
   or resident allocation. Deferring scratch allocation can reduce startup
   writes/residency even where sparse encoding already removes file cost.
   Existing owners: `habu-reserve-the-capture-e9d07c82` and
   `habu-size-the-capture-5f0c0e42`.

## Shaker and measurement boundaries

Native `AOT-CAPTURE:CAPTURE` already walks and compacts its code graph.
All 7,953 shipped dictionary records and 5,348 anonymous spans are reachable
under the current dictionary-surface roots. An engine-entry-only census
omits 180,528 reported code bytes, but future compilation and source loading
can require them: this is not a deletion list. Declare and qualify the
intended surface through `habu-declare-the-surface-89e9aed0`.
Capture also conservatively retains 33,696 bytes outside indexed bodies;
their removability is unproved.

`docs/engine-size.md` and `tools/image-size-lib.f` now distinguish the native
closure walk from the historical Linux samples. Old tier-1-growth examples
must not override current measurements.

The initial audit changed no compiler or runtime behavior. Its baseline
previously passed all 500 then-registered native suites. Focused audit checks passed:
`test/aot-capture-compact.f`, `tools/engine-size-test.f`,
`tools/manifest-lint.f` (28 rows, 20 entries, 88 closure files, no findings),
the four stripped probes, and the fresh two-tier corpus.
For implementation, compare actual emitted sections, startup behavior and
the existing behavior suites; do not add candidate byte savings together
before rebuilding, because representations, reachability and padding overlap.

## ARM64 code-size campaign

Owner heron; design revision 3 of the ARM64 "all fixes" design. Each slice is
a child dot carrying its own contract and measured byte change: census
e13ae0a3; division 1118f223; terminal-only link save f2071a86; mask and shift
immediates f082dbf3; shifted index and msub c57b5f1b; max as select fec184ee;
callee clobber summaries db11d4c1; persisted summaries 890d67ea (optional);
DATA literal pools d65bdc94; (RETURNED) 2ddb20af (census cleared its break-even); (STORE-CELLS) 458c9100
(closed below its census break-even); zero-filled snapshot DATA
089e4588; proven span checks 22af10b0, after colon inlining 2ca2438f; cold-throw
target a6529379.

Joel's decisions (2026-09-30): no no-check build mode; HR1, the private
register ABI, is revisited only after db11d4c1 and only for a measured
stripped-product win. Confirmed drops: inline guarded scalar stores (12 bytes
either way); exact-site division helpers and CallOrigin rows (nothing reads
x30 on a throw); ADRP for DATA (DATA-VA is out of ±4 GiB reach); the fragment
model, typed relocation algebra and wire schema (pools are sealed records);
the SCC fixed point and capsules (summaries never narrow).
