---
title: Reject unsafe top-level rows before execution
status: open
priority: 1
issue-type: task
created-at: "2026-07-21T22:00:40.892118+02:00"
---

Current src/core/top-row.f defaults every HABU_TOP_TIER value other than 2 to advisory tier 1, and snapshot boots leave the checking hook disarmed. The default load, stdin, and REPL path can therefore warn about a statically invalid top row and execute it anyway. Make rejecting tier 2 behavior unconditional on cold, snapshot, AOT, stdin, load, and REPL paths; re-arm persisted hooks before accepting source; remove the advisory execution path from the product. Burn down every existing warning as real checked code or a named audited boundary with its own capability owner. Preserve exact diagnostics and make the underflow, non-execution-token catch, and pointer-as-scalar probes default negative regressions with exit 70 before any body executes. Add snapshot and AOT parity tests that prove the same rows reject after restore. Files: src/core/top-row.f, hook restore owners, focused top-row tests, docs only after enforcement. Verify all exact command paths, native fixpoint, bootstrap/snapshot/AOT, trust and hook inventories, Maki, PTX standard library, and full native gate. The missing historical identifier habu-typed-top-tier-589c550f is not reused; this dot is the real owner.
