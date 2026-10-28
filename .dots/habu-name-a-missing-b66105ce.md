---
title: Name a missing does> clause signature
status: open
priority: 2
issue-type: task
created-at: "2026-10-28T11:38:54.678305+02:00"
---

Problem: ': MK ( n -- ) create , does> @ ;' (no clause signature) gets no named diagnostic: the load prints only 'does> at <file>:1' and exits 76; tools/check.f --json-errors answers E-STATEMENT-THROW with throw_code 7157 and repair_class unknown_rejection (src/habu/verify-source.f:59 E-VS-MISSING-SIGNATURE, :604-607 REQUIRE-SIGNATURE; tools/check-test-lib.f:4955 pins that packet). Acceptance: both front ends name the missing clause signature with its own code, prose and repair class at the does> token; the pinned row updated. Files: src/habu/verify-source.f, src/core/render.f, tools/check-core.f, tools/check-test-lib.f. Verify: /tmp/tim-dots/f2.f through bin/hb and tools/check.f --json-errors; tools/check-test.f. Depends: none. Ownership: checker diagnostics. Claim: unassigned.
