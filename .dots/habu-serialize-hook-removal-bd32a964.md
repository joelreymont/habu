---
title: Serialize hook removal with registration
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T14:35:22.883933+03:00"
---

Problem: lib/image-lifecycle.f REMOVE mutates N with `-1 N +!` from PREPARE without taking LOCK, while REGISTER and COUNT (9e266193) read and write N under LOCK, so the lock serializes the count against registration only; the module comment 'Capture runs after application tasks stop' is the sole justification and nothing enforces or tests it. Acceptance: either PREPARE takes LOCK around the hook walk and REMOVE (with the unwind path REGISTER already has, and a stated reason a hook body cannot re-enter REGISTER while held) or the module refuses PREPARE while another task is live and a regression proves it; a two-task test (lib/task.f) registers during a PREPARE and shows the count is never half-applied. Files: lib/image-lifecycle.f, test/image-lifecycle.f. Verify: bin/hb --load test/image-lifecycle.f. Depends: none. Ownership: lib/image-lifecycle.f. Claim: unassigned. Source: audit-testlib worker note 2026-09-16.
