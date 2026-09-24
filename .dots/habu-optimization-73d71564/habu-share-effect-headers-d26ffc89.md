---
title: Share effect headers across word bindings
status: open
priority: 1
issue-type: task
created-at: "2026-09-24T17:18:43.196816+02:00"
blocks:
  - habu-compact-checker-histories-3a1ce692
---

The captured effect store has 20,825 96-byte headers: 1,999,200 raw bytes and 267,431 encoded value bytes. Shared graph nodes/strings/argument runs cost only 20,566 encoded value bytes; graph interning already works. Exact comparison found 1,766 semantic header tuples. Fields compared: ACTIVE, DIN, DOUT, RIN, ROUT, HASR, TVN, RVN, MINI; excluded NEXT, SYM and SYMPREV.

Separate per-word binding/version identity from shared effect content so semantically equal headers share one representation. Preserve ACTIVE/source authority, primitive overload identity, every direct effect reference, symbol IDs, historical lookup, source reconstruction and rollback. This is distinct from dropping unreachable bindings or old versions; remeasure after history compaction and do not add overlapping savings estimates.

Own checker.f effect record layout, publication/readers and persistence plus affected owner/payload ABI boundaries. Verify existing checker effects, overloads, control flow, type constructors, source replay, rollback and capture/restore paths; produce a smaller actual section while preserving semantics. Run native fixpoint, full test/run.f and Maki smoke before landing. Follow history compaction to avoid concurrent changes to the same record representation.
