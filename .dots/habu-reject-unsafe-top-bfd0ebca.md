---
title: Reject unsafe top-level rows before execution
status: open
priority: 1
issue-type: task
created-at: "2026-07-21T22:00:40.892118+02:00"
---

Current `src/core/top-row.f` defaults every `HABU_TOP_TIER` value other than 2
to advisory tier 1, and snapshot boots leave the checking hook disarmed. The
default load, stdin, and REPL paths can warn about a statically invalid top row
and execute it anyway.

Make rejecting tier-2 behavior unconditional on cold, snapshot, AOT, stdin,
load, and REPL paths; re-arm persisted hooks before accepting source; remove
the advisory execution path. Convert every current warning site to checked
code or one named source boundary carrying its rationale, retirement owner, and
focused production test. Preserve exact diagnostics and make underflow,
non-execution-token catch, and pointer-as-scalar probes default negative
regressions with exit 70 before any body executes. Snapshot and AOT tests prove
the same rows reject after restore.

Files: `src/core/top-row.f`, hook restore owners, focused top-row tests, and the
enforcement documentation. Verify every exact command path, native fixpoint,
bootstrap/snapshot/AOT, a fresh source census, Maki, PTX standard library, and
the full native gate.
