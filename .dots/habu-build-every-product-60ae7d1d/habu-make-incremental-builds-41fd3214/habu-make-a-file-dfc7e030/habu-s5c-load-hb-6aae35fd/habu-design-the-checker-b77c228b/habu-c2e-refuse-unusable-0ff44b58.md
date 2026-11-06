---
title: "C2e: refuse unusable facts by name"
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T20:16:59.627133+02:00"
blocks:
  - habu-c2c-store-a-1bfc2ab6
---

Problem: design r2 D9: a record without usable facts must stop the check with a named error. Acceptance: named errors for a record written before C1's layout, a record below the build's first, an unparsable facts text and D5's difference between text and proof, each shown by a rejected program. Files: src/core/checker.f, a rejected-program test. Verify: those cases; bin/hb --load test/run.f. Depends: C2c. Ownership: those files. Claim: unassigned.
