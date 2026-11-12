---
title: "A red test's logs are gone when the suite ends"
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T13:42:11.093687+02:00"
---

Problem: a full macOS suite's RED line names its test's logs (RED: process-signals out=<TMPDIR>/habu-native-suite-<n>-0/pool-65541-88-out.log err=...), and that directory no longer exists once the suite exits, so a red that does not reproduce alone cannot be diagnosed. Acceptance: after a suite with a red test, every file a RED line names exists, or the suite prints the red test's output itself. Depends: none. Claim: unassigned.
