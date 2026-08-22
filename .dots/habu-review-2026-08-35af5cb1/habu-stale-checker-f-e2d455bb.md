---
title: stale checker.f size header and fixed 512 KiB readers
status: open
priority: 2
issue-type: task
created-at: "2026-08-22T22:38:25.858564+02:00"
---

Problem: test/create-axiom-test.f:50-55 says checker.f is 524,245 bytes and 44 under a $80000 cliff in three lints; measured 589,900 bytes - the named lints now read through tools/lint/text.f LINT-SLAB (MAX-BYTES $1000000). Fixed $80000 readers that remain: tools/source-discovery.f:32 SD-SRC-CAP, tools/bootstrap-codegen-test.f:42 SRC-CAP, src/core/include.f:7 INCLUDE-BUF-CAP ('include: file too large', :221). No 1024-line cap lint exists; checker.f 13,431 lines, type-family.f 3,712, sumtype.f 2,131, render.f 1,053. Acceptance: the header corrected; the three fixed readers use the runtime-sized slab (or measure and name why not); a probe that reads checker.f through each passes. Files: test/create-axiom-test.f, tools/source-discovery.f, tools/bootstrap-codegen-test.f, src/core/include.f. Verify: the probes. Depends: none. Ownership: lint buffers. Claim: unassigned.
