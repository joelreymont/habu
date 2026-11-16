---
title: Agree on a section keyword inside evaluated text
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T13:49:36.886205+01:00"
---

Problem: a section keyword in a string run by evaluate splits the loaders: /tmp/carl-spark/rf/pc-plan/keep/eval-public.f loads rc 0 but --verify-only gives rc 70 (a false E-UNDEFINED after W-CHECK-DEFERRED); /tmp/carl-spark/rf/pc-plan/keep/eval-private.f loads rc 70 and verifies rc 0. Cause: evaluate's deferral in the verifier, not the section machine. Acceptance: one record and rc under --load, tools/check.f and --verify-only for both. Depends: none. Claim: unassigned.
