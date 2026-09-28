---
title: Optimization
status: active
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
490 suites. Both Maki routing/geometry and shared-channel smokes pass with
byte-identical accepted boards; engine and Maki signatures verify strictly.
Evidence and artifacts: `~/.cache/tmp/habu-thin-internal-completion-20260928-02.md`.
The internal-name and product/whitebox test dots are closed.

The next capture E2E exposed a native `does>` publication defect: a resident
definer ran correctly but source verification could not recover its created-word
effect. `habu-publish-native-does-10a4b58a` fixes publication through the checker
owner and holds its existing rollback transaction through native publication.
Rejected clauses and later failures restore signatures, control/CREATES, symbols
and extension state; they cannot leave a phantom created word. The public E2E
reproduced both failures before their repairs. Independent review, source-only
replay, native B2–B5 byte equality, all 490 suites and the two byte-identical Maki
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
Maki boards remain byte-identical and strict signatures pass. Evidence:
`~/.cache/tmp/habu-control-compaction-completion-20260928-02.md`.

These reductions do not prove that remaining DATA is necessary or generated
code is efficient. The current product retains 736,008 DATA/bitmap bytes and
1,543,368 generated code bytes plus 121,100 fixed engine bytes. General effect
history, private symbols, DATA retention and native instruction selection remain
open. Require current physical evidence, native self-host convergence, the full
gate and Maki qualification for each capture change. No estimate or tracker
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
passed with a process-scoped sleep assertion. Maki's native image falls from
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

Maki's native REPL image is **22,687,328 bytes**, down 2,577,312 from the
preceding product. Its raw warm DATA window alone shrinks by 2,495,588 bytes;
no snapshot codec changed. Routing/geometry and negotiation checks pass with
both exported boards byte-identical. Final measurements, failed runs, the
existing TCP/HTTP fixture ordering corrections and repeatable artifacts are
retained at `~/.cache/tmp/habu-opt-round2/combined/RESULTS.md`.

The following storage changes remove the checker boot reservations, give
SPA/TV/SEEN mapped ownership, and omit full-image driver/signers from warm
capture. Maki is now 21,702,368 bytes, another 984,960 bytes smaller; routing
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
6,130,400 bytes. Fresh native Maki falls from 21,702,368 to 21,193,472 bytes,
and its raw DATA window falls from 13,252,884 to 12,742,308 bytes. Unchanged
warm recapture adds no DATA bytes; one new provided fact adds 68 live path
bytes and a 16,384-byte raw DATA-window increase on recapture, including
capture overhead and a retained previous pool. The frozen candidate passes all
490 native suites. The packed and prior native Maki images export a
byte-identical routed and geometry-checked two-filter board; the packed image
also passes the retained negotiation suite and exports a byte-identical
shared-channel board. The new engine and Maki image pass strict macOS signature
verification. External KiCad DRC/connectivity, IPC and
map/unmap/DATA-allot fault injection remain untested. The exact source
manifest, executables, logs, sizes and limits are retained in
`~/.cache/tmp/habu-opt-round3/require-pool/RESULTS.md`.

Compression and snapshot repacking remain held. General DATA reachability and
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
The current downstream readiness check is Maki; older task text naming other
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
