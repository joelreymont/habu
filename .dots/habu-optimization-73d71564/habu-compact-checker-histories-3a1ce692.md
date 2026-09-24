---
title: Compact checker histories at capture
status: open
priority: 1
issue-type: task
created-at: "2026-09-24T17:18:43.126319+02:00"
---

Current product remeasurement (engine SHA-256
`2daeb34544e8c081209f2437485d76f606c61ca5ac7eafe8cc28ddf438845639`):
20,827 effect headers cost 267,461 encoded bytes; their allocation including
shared graphs and bitmap costs 321,883 bytes. There are 3,858 older user
headers costing 45,610 encoded bytes, and 18 older headers remain directly
referenced by current metadata. Of 22,952 control rows, 6,482 older rows cost
20,808 encoded bytes; 1,399 repeat their predecessor's state. Those counts do
not prove that source-order reconstruction can discard them.

The original probe hardcoded baseline coordinates and pointer-based symbol
strings. Its adapted copy derives final layout coordinates and decodes arena
offsets. All 16,691 decoded symbol rows match the restored engine; decoded
DATA value bytes match the physical census, and headers plus graph nodes
account for UEND and the independent flat encoded store count. Evidence and
repeatable probes: `~/.cache/tmp/habu-opt-names-scratch/history-rca/RESULTS.md`.

Persistent native capture copies the complete USIGS and NORETS prefixes. Measured linked histories include 3,858 older user-effect headers costing 45,608 encoded value bytes, and 6,481 older control rows costing 20,804 encoded value bytes. The latter include 1,398 exact repeated control states. These are retained costs, not proven removable totals.

Finalize histories at a valid capture/rollback baseline. Retain every record needed by primitive overloads, source reconstruction through BIND-HORIZON, explicit effect references and remaining compiler state; rebuild newest indices and backlinks consistently. Eighteen older effect headers are directly referenced by PES and must survive unless those references are correctly remapped. Keep symbol IDs stable. Preserve rollback for definitions created after restore and the supported NO.CREATES reference even though this baseline contains none.

Own checker.f USIGS-SNAPSHOT-PERSIST, NORET-SNAPSHOT-PERSIST, append/index/restore paths and capture preparation. Verify primitive overload selection, bounded historical lookup, failed-definition rollback, defer/control behavior, restored compilation and repeated captures. Measure retained record counts and actual image sections, then native fixpoint, full test/run.f and Maki smoke. Independent of reducing the published interface; coordinate its root set with habu-drop-private-signatures-974304d0. Evidence and baseline are in the Optimization parent; temporary probe habu-effect-rca.f.
