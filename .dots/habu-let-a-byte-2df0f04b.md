---
title: Let a byte edit make its own storage
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T17:23:02.629755+01:00"
blocks:
  - habu-xml-makes-its-3135a391
---

Problem: package EDIT (src/lib/byte-edit.f:76,99,147,197) takes caller-carved storage as XML:SOURCE-INIT did; dot habu-say-str-for-d3f92b65 covers its ^u8 n rows but not the storage. Acceptance: no caller sizes, allocates or casts storage for an edit; it takes an allocator as XML:SOURCE-INIT does. Depends: habu-xml-makes-its-3135a391. Claim: unassigned.
