---
title: Declare the tools records and delete the dead helper
status: open
priority: 3
issue-type: task
created-at: "2026-09-18T10:24:05.100828+03:00"
---

Problem: tools/lint/text.f LINT-SLAB (0 ptr u8, 1 cap, 2 len) has seven create-d instances (lint/text.f:173, error-code-lint-core.f:90, aot-section-reach-lint.f:47, aot-lint-core.f:13, public-signatures-core.f:45,46, lint/shadow-lint.f:34) read through BUF-FIELD ( ptr n -- ptr ptr u8 ) and plain + @; thirteen *-PTR-U8-FIELD ( ptr a -- ptr ptr u8 ) families over about 60 call sites (tools/json.f, check-core.f, build-fixpoint.f:222, hb-build-lib.f, examples-test.f, lint/diff.f, three json-only*.f, three repair-*.f) spell a two-field record (a pointer slot beside a separate length variable) as two words; tools/check-core.f:137-144 is an indexed ptr-field helper with no caller anywhere. Acceptance: LINT-SLAB declared once (STRUCTURE with DERIVE addr), the seven instances TYPED-VARIABLEs, its accessor words generated away; each pointer-slot family a two-field record with its call sites on the generated accessors; check-core.f:137-144 deleted; no ptr-field cast left under tools/; the lint and check suites assert exactly what they asserted before. Files: as listed. Verify: the lint and check suites; test/run.f. Depends: habu-generate-typed-field-ba63866e, habu-replay-derive-addr-5fd9a813. Ownership: tools. Parent: habu-campaign-c2-mem-c3d7662b. Claim: unassigned.
