---
title: Compact checker histories at capture
status: open
priority: 1
issue-type: task
created-at: "2026-09-24T17:18:43.126319+02:00"
---

Persistent native capture copies the complete USIGS and NORETS prefixes. Measured linked histories include 3,858 older user-effect headers costing 45,608 encoded value bytes, and 6,481 older control rows costing 20,804 encoded value bytes. The latter include 1,398 exact repeated control states. These are retained costs, not proven removable totals.

Finalize histories at a valid capture/rollback baseline. Retain every record needed by primitive overloads, source reconstruction through BIND-HORIZON, explicit effect references and remaining compiler state; rebuild newest indices and backlinks consistently. Eighteen older effect headers are directly referenced by PES and must survive unless those references are correctly remapped. Keep symbol IDs stable. Preserve rollback for definitions created after restore and the supported NO.CREATES reference even though this baseline contains none.

Own checker.f USIGS-SNAPSHOT-PERSIST, NORET-SNAPSHOT-PERSIST, append/index/restore paths and capture preparation. Verify primitive overload selection, bounded historical lookup, failed-definition rollback, defer/control behavior, restored compilation and repeated captures. Measure retained record counts and actual image sections, then native fixpoint, full test/run.f and Maki smoke. Independent of reducing the published interface; coordinate its root set with habu-drop-private-signatures-974304d0. Evidence and baseline are in the Optimization parent; temporary probe habu-effect-rca.f.
