---
title: Name ^T in the help for a ptr type
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T20:29:48.948516+01:00"
closed-at: "2026-11-16T04:07:35.285272+01:00"
---

Problem: src/core/render.f SIG-HELP answers an unknown signature type `ptr` (or a local's `p:ptr`) with 'write a type in scope or a one-letter type variable in place of `ptr`', which never says a pointer is spelled ^T; structure fields (SD-PTR-FIX$) and sum payloads (TDECL-PTR-FIX$) say it. An agent writing the old spelling is not told the fix. Acceptance: the record for `ptr` in a signature and in a local's annotation helps with the ^T spelling; test/signature-nesting.f pins that help. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
