---
title: Shorten shared DATA address carriers
status: active
priority: 2
issue-type: task
created-at: "2026-09-16T16:09:22.600785+03:00"
---

Problem: references to variables and created buffers use four move-wide
instructions at both compiler tiers. Both supported fixed DATA mappings fit in
48 bits, so their fourth halfword is unnecessary. Shared storage must keep its
absolute address: x20 is the current task's private header, not the process-wide
DATA mapping (src/habu/layout.f SIGNAL-ABI and lib/task.f TASK-REGION-INIT).

Acceptance: three-instruction DATA carriers at both tiers, with absolute full
carriers retained outside the fixed mapping; unchanged address-site provenance
and numeric lookalikes; shared variable access from a worker task; capture,
artifact merge, stripped link and snapshot restore agree on the carrier;
malformed/truncated carriers refuse; Gforth tier-0 mirror agrees. Report both
tier censuses, sample disassemblies, relocation-site counts, engine/code sizes
and runtime timings before/after. Check-only bootstrap, byte fixpoint and full
gate pass before integration.

Ownership: code generation. Claim: alder, `.jj-ws/alder-data-address`, based on
c0f75b0d. Hazel released the compiler and the three engine relocation seams.

The original x20-relative proposal was incorrect. The threaded filesystem
fixture and a reduced worker incrementing a shared variable both crashed with
that carrier (rc 134). Three absolute halves in high-to-low order preserve the
same 4-byte saving without a task ABI change or an additional reserved register.
The distinct first instruction makes the short and full forms unambiguous.

The original "no relocation" premise was also incorrect: MAPPED-DATA moves
carried claims and re-interned literals. The existing declared address-site map
must remain the authority, including outside-window refusals. The AOT artifact
version changes for the new carrier grammar; no relocation row or tag is added.

REQUIRE-BOOT-OPEN? measured 56 bytes / 14 instructions on faa44fcc. Five
materialise and load its DATA address; nine perform the two boolean conversions,
stack publication and return. Even a single base-relative load would leave ten
instructions, so the old "under 8" target belongs with f9eadb86's double-negation
fold. The accepted shared-address semantics also preclude x20-relative load
fusion for these variables. Keep those costs separate from the carrier saving.

Evidence and private build/test logs:
`~/.cache/habu/data-address/source-be059e51/` (directory named for the initial
measurement; the final implementation base is c0f75b0d).

Measured result on c0f75b0d: the 2,265-word corpus falls from 169,524 to 168,484
bytes at tier 0 and 194,492 to 186,880 at tier 1; no word grows. Declared DATA
sites stay 260 and 1,903 respectively. The full native engine falls from
3,604,672 to 3,539,136 bytes; combined engine and AOT machine code falls by
67,564 bytes. After the review fixes, gen2 and gen3 both hash to
d4554f4f6df9274b5243694cebe2f46a88d68911117741ceb6a1d982e3c928b2,
still 3,539,136 bytes. The rebuilt code differs from the measured product only
in the two snapshot-version immediates; the code placement is unchanged.

The remaining placement finding is tracked under a6529379. Two CPU-4-pinned
ABBA sequences, with no
Habu build or gate in the before/after inventories, measure search at 2,060 ->
2,292.5 us (+11.3%); the other six benchmark cases are within 1.4%. The hot
LINT-FIND-SUB/LINT-STR= instructions are unchanged. A controlled placement probe
runs 1,000 identical searches: old/default 2.059 s, new/default 2.289 s, new
with 16 bytes of preceding unused code 2.060 s, old with 48 bytes 2.289 s.
Within each engine the placement change leaves retired instructions and branch
misses essentially unchanged; the slower placement costs about one cycle per
searched byte. The exact microarchitectural mechanism is not yet established.
No padding is added to production code or the normal benchmark. This is a real
layout sensitivity, not evidence that the default benchmark stayed flat.

A finer diagnostic holds LINT-STR= at the same address and moves only
LINT-FIND-SUB by 4-byte steps: offsets 0/4/8 take 2.289/2.292/2.288 s; offsets
12/16 take 2.062/2.061 s for the same 1,000 searches. The finder alone is
sufficient to reproduce the placement effect. This does not identify the
hardware mechanism or resolve the normal-placement regression.

Focused compiler, relocation, capture, stripped-build, whitebox and bootstrap
rows pass; the engine-size fixture passes and its generated documentation row
has the expected image drift. The full gate and review remain Hazel's landing
steps. README.md in the evidence directory records exact tests and limitations.

The carrier grammar lives in the pure address-carrier.f module, shared by the
maker and inspection tools; loading it does not allocate capture buffers. Its
extraction is byte-neutral against gen2/gen3. Check-only bootstrap, build-fixpoint,
all three hb-build rows, the complete tail-pure row, both tiers of the codec/owner
row and the relocation proof pass after that extraction. The complete engine-size
row has only its generated documentation assertion red (assert 3); the fixture
itself passes. No full gate was run outside Hazel's chain.

Review corrections: ACAP-SCAN-CSITES refuses a three-word carrier whose value
falls in the CODE band before it can rewrite a fourth word. Live shared DATA
and CODE occupy disjoint mappings, so that shape is corrupt. The regression
first demonstrated the four-byte overwrite, then the named rc 74 refusal.
Reloc.v and docs/proofs.md now state that the model covers only four-word
carriers; DATA-CHAINS in test/compiler/reloc-cases.f checks the shipped
three-word relocation branch.

The outer snapshot format is 10 because its region-code grammar changed;
the address-cell storage ABI stays 1. A diagnostic image with eafd8a99's old
loader and 82c96a9e's format-9 region failed with rc 97, "snapshot address map
mismatch". The same old-loader experiment with the versioned product now
refuses with rc 80, "snapshot format version unsupported"; the matching new
loader restores and prints 42. Maker sources and their running engine must be
paired: loading a new maker into an old engine does not upgrade its baked
loader. The build-fixpoint trailer fixture pins the format-9 refusal as well.

After these review corrections, all three hb-build rows, build-fixpoint,
snapshot-writer, app-image, address-cell-tasks, aot-chain-capture,
native-window-owner, the whitebox snapshot-xt-cell-decl row, all eleven
tail-pure files in one load, the relocation proof and check-only bootstrap
pass. The full gate remains Hazel's integration-chain proof.
