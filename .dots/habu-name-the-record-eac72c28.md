---
title: Name the record that overflows the AOT capture bound
status: open
priority: 3
issue-type: task
created-at: "2026-09-21T12:50:36.093751+03:00"
---

Problem: a native build whose captured window exceeds AOT-REC-MAX (src/habu/aot-decl.f) dies rc 74 with only 'aot-capture: too many records' - it names neither the count, the bound, nor the definition that crossed it; the allocator-vocabulary lane lost a worker to it and isolated it only by building the pristine tree. Acceptance: the refusal prints the record count, the bound and the name (and file where known) of the record being added when the bound is hit; a fixture over a private tree copy with a lowered bound pins the message; no format change. Files: src/habu/aot-capture.f, test/. Verify: the fixture; three generations. Depends: none. Ownership: hazel. Claim: unassigned.
