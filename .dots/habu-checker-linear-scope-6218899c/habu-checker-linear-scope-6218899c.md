---
title: "Checker: linear-scope WITH-owner combinator"
status: closed
priority: 2
issue-type: task
created-at: "2026-07-26T09:00:47.465023+02:00"
closed-at: "2026-09-30T17:45:00.000000+02:00"
close-reason: "Complete: every other child landed, and the last, habu-unify-all-quotation-56884608, rested on a false premise."
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
