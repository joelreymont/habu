---
title: Package memory-test MEMT globals
status: open
priority: 3
issue-type: task
created-at: "2026-07-26T09:02:20.589597+02:00"
---

Problem: naming-audit item - lib/memory-test.f defines the raw global stem MEMT- (MEMT-BUFS, MEMT-SPAN-BUFS, MEMT-SPANS, MEMT-MARK-A, MEMT-MARK-Z, MEMT-HERE, MEMT-TOTAL and the rest, lines 11-50) before any package opens, violating the package-first rule that raw global stems must not substitute for package scope. Required result: open a real test package at the top of the file and move the MEMT- definitions into it with short package-local tails (BUFS, SPANS, MARK-A, TOTAL and so on), qualifying any cross-package callers; no forwarding aliases. Acceptance: the file passes the package diff gate on its own diff with zero findings and the memory suite is unchanged in behavior. Files: lib/memory-test.f. Verify: bin/hb --load lib/memory-test.f plus the package diff gate on the change. Depends: none. Ownership: memory-test naming only. Claim: unassigned.
