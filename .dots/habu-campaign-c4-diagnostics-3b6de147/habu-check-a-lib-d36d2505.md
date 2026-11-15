---
title: Check a LIBRARY path once
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T10:48:34.071728+01:00"
closed-at: "2026-11-16T04:07:36.631178+01:00"
---

Problem: after the master merge into C4, src/lib/ffi-abi.f checks a LIBRARY path twice: master's PATH-WRITTEN and spelling check, then C4's CHECK-PATH records (which SELECT-LIBRARY also uses). Acceptance: one check owns each path rule and writes its record; the other is deleted; an empty path, a path over PATH-CAP and a misspelt path are each rejected once, under LIBRARY and SELECT-LIBRARY; src/lib/ffi-test.f's path cases pass. Depends: the master merge into C4. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
