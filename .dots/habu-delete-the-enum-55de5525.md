---
title: Delete the enum parity rows that compare one path with itself
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T01:13:58.585577+02:00"
---

Problem: test/enum-decl-suite.f rows 20c, 21a, 21b, 22b and 24b compare the sumtype.f definer with the unified ENUM-DECL:ED-RUN path; the global ENUM keyword now calls ENUM-DECL:ED-RUN (src/core/enum-decl.f GLOBAL-KEYWORD BINDING), so each row compares one path with itself. Acceptance: the five rows are gone and the suite stays green. Files: test/enum-decl-suite.f. Verify: bin/hb --load test/enum-decl-suite.f rc 0. Depends: none. Ownership: test/enum-decl-suite.f. Claim: unassigned.
