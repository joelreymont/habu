---
title: Pre-push check tool
status: open
priority: 3
issue-type: task
created-at: "2026-07-21T16:31:12.726263+02:00"
---

Build a cheap checked Habu preflight, not another full gate. The tool and any
launcher logic are checked Habu run by `bin/hb`; no shell implementation or
host-language reducer is permitted. It reads the exact
outgoing Jujutsu range, prints every touched path grouped by owning source class,
fails when a purported dots-only publication contains another file, runs the
native dot dependency check for dot mutations, and selects the focused checks
the change owner must have recorded. It may perform cheap structural inventory
checks whose inputs are already present, but it must not rebuild the engine,
repeat full Maki/PTX/native gates, run performance measurements, or treat itself
as merge proof. The exact rebased integration tree still runs its one owning
merge gate separately; this focused preflight must never cause redundant full
gate runs. Implement the tool in checked Habu; no shell driver,
awk, sed, jq, Python, or JavaScript allowance exists. Exit nonzero on ambiguous
ranges, unowned paths, mixed hidden changes, or dependency findings. It prints
the required focused checks but introduces no new proof-record registry.

Claim: RELEASED 2026-07-21. The empty `fable-prepush` workspace remains
historical evidence; this leaf is open and unassigned.
