---
title: Preserve exact dictionary code extents for native no-return bodies
status: open
priority: 1
issue-type: bug
created-at: "2026-09-14T17:37:00Z"
---

Blocks [native typed-load validation](habu-attribute-native-preseed-tag-38ad40e8.md).
Engine `/tmp/cedar-fetch-string-native` (SHA prefix `a4137573`) publishes a native
nonreturning record with its full emitted length, while AOT `SCAN-REC`,
`ADDRESS-OWNER`, and `RAW-LEN` add four bytes unconditionally. `BAD-DESCRIPTOR`
therefore acquires the first MOVZ of adjacent `BAD-TAG`; the recorded address
chain correctly refuses as truncated. Scalar, empty-sum, and nested preseed
images fail exit 74 before output. No address-map or chain validation is relaxed.

Owner: Cedar. `CODE-SPAN` reserves ordinary length bit31 for an exact full span;
the remaining bits carry the body length. Untagged legacy records retain their
final four-byte slot, including a RET patched to B by DOES. Namespace private
WIDs are raw and never decoded. The existing compact u32 field preserves the
encoding verbatim. Native no-RET publication and deferred bodies set the bit;
live/compact readers and recovery `is`/DOES/inlining decode the same contract.

Transition: the original a413 engine remains a negative control. Its captured
native no-RET records lack the bit and cannot be reconstructed by examining
neighboring bytes. A first rebuilt engine can provide the new publisher while
still containing bodies captured by the old one; a second native generation is
required before qualifying the target's emitted spans. Source presence alone is
not evidence that baked compiler records have migrated. Snapshot/compact format
shape is unchanged, but old readers do not understand flagged lengths.

Passing initial evidence: checked schema test; native-build-core checked load
including flagged compact/expand self-test; bootstrap source lexer test; actual
Gforth stage0 prefix/ptr-cell-mark test; fresh recovery image executes created
DOES and compiled deferred binding and returns 41 then 42 (exit 0). The a413
negative fixture proves both four-byte ownership overruns and exits 74 when
SPIN-B borrows LIT-B's first recorded MOVZ. Logs: `/tmp/cedar-code-span-*.log`.

Pending acceptance: positive adjacent no-RET/RET/address-chain regression;
native legacy returning, deferred metadata, and DOES boundary tests;
two-generation native transition; exact pinned scalar,
empty-sum, nested valid/inactive/invalid preseed controls. Invalid typed loads
must exit 85 with `hb: bad layout tag`, including dropped results. Every image
build pins both `HABU_UNDER_TEST` and `HABU_FIXPOINT_ENGINE` to its qualified host.

Combined-build correction: frozen `b589ce65` refused exit 74 with
`hb: malformed dictionary code length`. `ACAP-SCAN-DEFER-SITES` visits namespace
rows too; `ACAP-DEFER-SITE` decoded their private WID before checking whether
the resulting address was in the code window. A checked reduction on guarded
host `68b6a5d6` selects `CHECKER-FETCH-ABI` with valid private WID 6 and reproduces
the refusal. The caller now skips `DICT-WL:NAMESPACE` before decoding [8].
Ordinary code records still pass through the unchanged `CODE-SPAN:CHECK`.

`test/compiler/code-span-capture.f` captures two actual namespaces, verifies a
private WID is not divisible by four, and preserves the exact exit-74 refusal
for an ordinary synthetic record with length 3. On the same guarded host it
failed before the caller fix and passes afterward; logs
`/tmp/cedar-span-capture-{before,after}.log`. The fixture is registered in the
gate. Combined native generations and the stripped preseed matrix remain
pending; this focused source-load result does not qualify a rebuilt image.

First combined generation: frozen source `93a972eb` builds and launches without
source as `/tmp/cedar-combined-native-r1`, SHA-256
`07d6f0dbaa95b81af102c19a0b23b240bdd56858736d712a9e608a4af9e5dc7a`.
The schema and actual native publisher fixture pass on this image with both
engine environment variables pinned. Native guards exposed a fixture-only
closure mistake: `ADJACENT` scanned callees, discarded that closure, then copied
only its root. It now uses the existing `ENTRY-NAME!`/`CLOSURE` traversal and
checks root identity, preserving the exact extent, adjacency, ownership and
copied-root byte assertions. The original test exits 74 for a PC-relative target
outside that incomplete closure; the corrected test passes. Logs:
`/tmp/cedar-combined-r1-code-span.log` and
`/tmp/cedar-combined-r1-native-code-span{,-after}.log`. Generation two and fresh
stripped preseed/cache evidence are still pending.
