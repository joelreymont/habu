---
title: One STR package, a string as one value
status: active
priority: 2
issue-type: task
created-at: "2026-11-23T10:36:06.833602+01:00"
---

Problem: Joel 2026-11-23: merge STRING (src/lib/string.f) into STR (src/lib/string-roles.f) in string.f; remove the split-str marker; STR:DATA and STR:LEN inlined to tight code; no string buffers; no runtime checks (STR's LENGTH, OFFSET, COUNT throw E-STR-BOUNDS). Acceptance: one package STR in src/lib/string.f; no file carries split-str; no str bound as two locals; STR:DATA and STR:LEN emit no call. Depends: none. Claim: agent=jerry, design /tmp/jerry-p1/design-str.md, lanes follow the approved design.
