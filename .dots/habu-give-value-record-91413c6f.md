---
title: "Give VALUE-RECORD's --load refusals a declaration packet"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T13:29:30.061861+02:00"
---

Problem: src/core/roles.f VALUE-RECORD (200-207) refuses 'missing name' and 'missing END-VALUE-RECORD' with die: no code, packet or place, rc 70 (review of carl-pp1, probes ~/.cache/tmp/rv-ppopen/p/v5-open.f, v3-two.f). Every other family definer refuses an unclosed body under --load as E-BAD-DECLARATION at the name, and the pre-pass (verify-source.f RECORD-VALUE-RECORD) now does too through an inline DECL-REJECT sequence. The reason text has three owners: verify-source.f RECORD-VALUE-RECORD, roles.f 207, tools/check-core.f 1089. Acceptance: one entry, as CHECKER-DEFSUM-NOEND serves SUMTYPE-COLLECT and RECORD-SUMTYPE-BODY, that roles.f VALUE-RECORD and verify-source.f RECORD-VALUE-RECORD both call; --load and --verify-only give the same E-BAD-DECLARATION packet at the name; the inline DECL-REJECT sequence goes. Files: src/core/roles.f, src/habu/verify-source.f, the entry's home. Verify: v5-open.f under --load --json-errors-capable path and under tools/check.f --verify-only --json-errors: same code, reason, place. Depends: carl-pp1 landed. Ownership: roles.f, verify-source.f. Claim: unassigned.
