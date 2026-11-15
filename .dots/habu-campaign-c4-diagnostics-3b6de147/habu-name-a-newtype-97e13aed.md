---
title: "Name a NEWTYPE's own parts in the wrap's records"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-16T02:09:12.754497+01:00"
closed-at: "2026-11-16T04:07:35.382531+01:00"
---

Problem: src/core/structure-decl.f writes NEWTYPE's and DEFTYPE's records (the wrap) in a STRUCTURE's words: 'NEWTYPE qqq a' is 'unknown field type' with the help 'list the parameter in the header, as in STRUCTURE box<a>', 'NEWTYPE qqq option<n' is told to close it 'before the next clause', a NEWTYPE base's help offers 'a header parameter', and a duplicate DEFTYPE is told 'rename the structure'. Acceptance: each record the wrap writes names the declaration's own parts, and its help's edit loads. Depends: habu-help-a-deftype-04951d85. Claim: agent=W159 workspace=.jj-ws.noindex/jerry-w159
