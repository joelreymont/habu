---
title: Decide the closed-dot archive flow
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T17:14:17.079101+02:00"
---

Full context: .dots holds 1534 files of which only 133 are active; 334 are status closed and 2 done. .dots/archive/ is already ignored and empty, and an open dot (habu-reject-archived-dots-db3cbf63) presumes an archive flow nobody runs. Every dot-dep-lint invocation walks all 1534 files. Decide whether closed dots move to the ignored archive or stay tracked, implement that decision, and make the gate enforce it so the mechanism is either used or removed. Acceptance: the chosen policy is enforced by a gate, dot-dep-lint walks only what the policy says it should, and habu-reject-archived-dots-db3cbf63 is either satisfied or retired.
