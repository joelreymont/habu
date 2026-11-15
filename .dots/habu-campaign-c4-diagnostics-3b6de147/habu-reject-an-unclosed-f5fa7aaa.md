---
title: Reject an unclosed structure at file end
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T06:23:38.769561+01:00"
closed-at: "2026-11-16T04:07:34.574769+01:00"
---

Problem: a file ending inside BEGIN-STRUCTURE loads under bin/hb (rc 0) while the verifier stops it with E-VS-DECL-SYNTAX (src/habu/verify-source.f RECORD-STRUCTURE :3546). Acceptance: all three readers print one and the same record at the BEGIN-STRUCTURE, rc 70. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
