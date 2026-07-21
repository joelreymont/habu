---
title: Reconcile unfinished GB10 graph work
status: open
priority: 1
issue-type: task
created-at: "2026-07-21T21:00:00+02:00"
---

Unfinished GB10 dependency-graph work is preserved in
`.jj-ws/control-gb10`, `.jj-ws/control-modelpack-fix`,
`.jj-ws/control-m0-fix`, and `.jj-ws/control-bringup-fix`. Those workspaces are
review evidence only: their changes are not merge-ready and no later lane may
use their unreviewed graph as a dependency contract.

Acceptance: re-derive the complete graph against the current `master` tree and
`docs/inference-engine-plan.md`. Consume only portions that receive independent
hunk-by-hunk approval. Resolve every outstanding finding concerning the
modelpack namespace and safe file-system contract, M0 metric kinds and identity
domains, capacity and end-to-end ownership, and stale dependencies or conflated
identities. Preserve distinct identities wherever lifecycle, lookup, telemetry,
or capacity accounting requires them. Prove immutable publication and telemetry
contracts explicitly rather than through ordering assumptions. Recheck every
edge against the actual implementation owner and current end-to-end path. Run
the exact graph review and native dependency lint on the final graph, then
publish the reconciled graph separately on a green `master` change.

Dependencies: none. This recovery task is open and unclaimed. It does not block
`habu-checker-type-enum-9569edb6`,
`habu-atomic-generated-declaration-4c1e8b7a`, or
`habu-bpe-unicode-data-45a7c2e9`.

The stale claims on compiled-model, GPT-2 forward, M0 measurement, and paged-KV
remainder were released to open during this recovery. Their preserved
workspaces are destruction-review evidence, not live implementation lanes.
