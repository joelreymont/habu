---
title: Check rejects an alias of a learned definer whose file is required
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T15:32:56.056893+01:00"
---

Problem: `bin/hb --load tools/check.f -- --verify-only` of a file that requires the file of a learned definer (a word the definer check learns by its `does>` row) and uses an alias of it answers rc 1 W-CHECK-STOPPED, where a load accepts it (rc 0); it is the same on an engine without the require check, so the cause is in the learned-definer reading of an alias (src/habu/verify-source.f SELECT-ARM and the learned arm). Acceptance: the check answers as the load for an alias of a learned definer, with a case in test/require-check.f or the definer tests that fails before. Depends: none. Claim: unassigned.
