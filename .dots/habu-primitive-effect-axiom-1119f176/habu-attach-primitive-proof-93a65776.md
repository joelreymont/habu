---
title: Attach primitive proof recipes
status: closed
priority: 1
issue-type: task
created-at: "\"2026-07-13T16:05:05.309072+02:00\""
closed-at: "2026-07-19T15:21:41.248167+02:00"
close-reason: Recipe ledger merged at 651f30f4 (315 rows, canary+recipe self-tests) and verified on master.
blocks:
  - habu-ratchet-primitive-effect-43e46e7a
---

Full context: test/prop-test-core.f AX-CHECK proves only output depth, while noexec name lists provide classification but no per-row evidence. Fix: replace category/name lists with one audited recipe per primitive-effect row containing typed operand constructors, expected depth and value-provenance observations, or an explicit fail-closed noexec evidence class. Arithmetic semantic truth beyond the declared type/effect is not invented here. Acceptance: every live row has exactly one recipe; missing/duplicate recipe, declared arity/type mutation, unsafe execution, or stale noexec classification fails with the row identity; deterministic sharded results reproduce. Files: test/prop-test-core.f, test/gate-debug-lib.f if needed, docs/effects.md. Verify: deterministic prop test, default sharded prop gate, gate-debug, typed-local lint, full native gate. Claim: agent=primitive-recipes workspace=.jj-ws/habu-attach-primitive-proof-93a65776.
