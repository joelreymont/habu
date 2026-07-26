---
title: "Checker: linear-scope WITH-owner combinator"
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T09:00:47.465023+02:00"
blocks:
  - habu-migrate-safet-loads-379b3f70
---

Campaign only; do not dispatch this parent. Catch currently restores stack cells
without proving that a throw path retained the same linear owners, and ordinary
stack-preserving catch cannot express an arbitrary successful owner
transformation. The children first make every quotation throw row authoritative,
then prove catch restoration, add the symbol-bound `LINEAR-SCOPE:WITH` effect,
implement its call-local runtime, and migrate the real SAFET load path. Existing
weight-store, streaming-write, and relinquish leaves consume the same public
interface; they must not duplicate it. Close this parent only after every child
has landed and the production SAFET paths prove zero leaked owners.
