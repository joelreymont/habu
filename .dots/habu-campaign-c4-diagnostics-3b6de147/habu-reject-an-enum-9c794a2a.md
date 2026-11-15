---
title: "Reject an ENUM payload's bare ^ as a pointer"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T20:33:06.219805+01:00"
closed-at: "2026-11-16T04:07:34.598984+01:00"
---

Problem: src/core/enum-decl.f EN-PTR-TEXT? strips a bare `^` and RESOLVE-TYPE rejects the empty rest as a missing payload type with a help not about pointers; STRUCTURE and SUMTYPE reject it as a pointer with no pointee. Acceptance: the ENUM record names the pointer and its `^T` spelling; test/enum-decl-suite.f pins it. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
