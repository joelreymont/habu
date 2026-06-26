---
title: Table-drive gate JSON assertions
status: closed
priority: 3
issue-type: task
created-at: "\"2026-06-25T12:19:43.583877+02:00\""
closed-at: "2026-06-25T22:15:21.519511+02:00"
close-reason: Replaced gate-json repair suggestion and command dispatch branch ladders with checked row helpers. Rows use typed quotations so command action effects remain checked. Validated direct json-one-schema, check-repair-hints-test, repair-packet-test, repair-schema-doc-test, test/gate-stdlib.f, direct stale-status lint, and full native gate.
---

Finding F23. Evidence: docs/factorization-review.md:51; tools/gate-json-assert.f:283 and tools/gate-json-assert.f:468. Root cause: repair suggestion and command dispatch are branch ladders. Fix: use table-driven rows for command arity/xt and repair-class suggestion mapping. Why: gate assertion catalogs should be data, not growing control-flow ladders. Validate with gate-json-assert tests and full native gate.
