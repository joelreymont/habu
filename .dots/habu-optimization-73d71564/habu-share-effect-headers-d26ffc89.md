---
title: Share effect headers across word bindings
status: closed
priority: 1
issue-type: task
created-at: "\"2026-09-24T17:18:43.196816+02:00\""
closed-at: "2026-09-25T09:28:29.445555+02:00"
close-reason: "Shared exact immutable contents preserve every binding, history and authority flag; explicit 96-byte wire transfer remains unchanged. Independent review and owner/payload/rollback E2Es pass. Isolated effect allocation falls from 321883 to 239570 encoded bytes. Combined native generations are byte-identical at 2889847 bytes; all 490 suites pass. Maki is 22687328 bytes with identical exported boards and no snapshot compression. Evidence: ~/.cache/tmp/habu-opt-round2/combined/RESULTS.md."
---

The captured effect store has 20,825 96-byte headers: 1,999,200 raw bytes and 267,431 encoded value bytes. Shared graph nodes/strings/argument runs cost only 20,566 encoded value bytes; graph interning already works. Exact comparison found 1,766 semantic header tuples. Fields compared: ACTIVE, DIN, DOUT, RIN, ROUT, HASR, TVN, RVN, MINI; excluded NEXT, SYM and SYMPREV.

Separate per-word binding/version identity from shared effect content so semantically equal headers share one representation. Preserve ACTIVE/source authority, primitive overload identity, every direct effect reference, symbol IDs, historical lookup, source reconstruction and rollback. Retain every binding and history; sharing does not require deciding their liveness.

Own checker.f effect record layout, publication/readers and persistence plus affected owner/payload ABI boundaries. Verify existing checker effects, overloads, control flow, type constructors, source replay, rollback and capture/restore paths; produce a smaller actual section while preserving semantics. Run native fixpoint, full test/run.f and Maki smoke before landing.

Approved design: 40-byte identity/history bindings reference exact interned
64-byte immutable content in the same USIGS arena. ACTIVE remains per binding;
both SOURCE-ROW transfer and serialized graphs retain their explicit 96-byte
wire layout. History compaction was sequenced first only to avoid overlapping
representation edits, not for correctness. It can follow this change, accounting
for shared content without combining unproved savings.

Before implementation, identified failure modes: shared authority or identity
changes replay/overload selection; stale index entries after rewind or growth;
content mistaken for graph nodes; zero binding offset mistaken for absence;
foreign wire rows decoded using the local arena; importer offset/overflow errors
or partial publication; transient addresses surviving capture. Existing effect
interning, scan/history, rollback, authority, DOES>/defer, owner transfer,
serialized payload refusal and snapshot E2Es cover these paths. Preserve their
behavior while adapting layout access; no new post-code unit tests.

Baseline 88243cb4 engine SHA-256
`2daeb34544e8c081209f2437485d76f606c61ca5ac7eafe8cc28ddf438845639`:
20,827 headers cost 267,461 encoded bytes; 1,766 exact content tuples. Complete
effect allocation including graphs and bitmap costs 321,883 bytes. These are
retained costs, not achieved savings. Design/evidence directory:
`~/.cache/tmp/habu-opt-round2/`.

Implementation and independent review are complete. The exact-content index
preserves its invalid state through clear followed by arena growth; it rebuilds
from valid bindings instead of reviving stale buckets. Local readers use the
40-byte binding plus 64-byte content; explicit 96-byte projection/import keeps
both owner-transfer boundaries unchanged. Every history and binding remains.

Isolated accounting reconstructs all 20,914 bindings, 1,767 shared contents and
2,237 graph nodes, with every symbol row matching the live checker. The complete
effect allocation falls from 321,883 to 239,570 encoded bytes. Independent real
authority, rollback, interning, census and AOT payload producer/consumer checks
pass, including malformed payload refusal. No new repository unit tests.

Combined native generations are byte-identical at 2,889,847 bytes, SHA-256
`730f69dac961702ea8593685d5f7641df32cf2d8d20725ba996b38f422e63aae`.
Maki builds and runs its routing/geometry and negotiation checks, producing
byte-identical PCB files at a 22,687,328-byte image size (2,577,312 fewer bytes).
Its raw DATA window alone shrinks by 2,495,588 bytes; no warm snapshot codec
changed. These integrated deltas include the frame-reload change and alignment,
so they are not isolated attribution. Final registry acceptance, independent
reviews, artifacts and all rejected runs are preserved under
`~/.cache/tmp/habu-opt-round2/combined/RESULTS.md`.
