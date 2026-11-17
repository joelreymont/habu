---
title: "Delete the restore of an includer's usings"
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T13:37:34.588760+01:00"
---

Problem: once a buffer may close only the packages it opened (habu-reject-a-loaded-37d1c8c7), a buffer's using depth never drops below its entry depth, so state kept to undo a buffer that closed its includer's package and opened usings in its slots is never needed: EVAL-FRAME USE-WIDS (src/habu/layout.f:256-257) with its save and put-back (src/habu/interpreter.f:3020-3022, :3093-3096; src/habu/habu2.f:2200 USE-WIDS-RESTORE; src/habu/habu1.f:1491-1493), PKG-USING's USE-FLOOR lowering (interpreter.f:1148-1156), and in the checker the replay's S-WIDS and CLOSE-PACKAGE's depth restore (src/core/checker.f :14014-14019, :14033) and OPEN-USING's SOURCE-FLOOR lowering. Acceptance: each is deleted or shown reachable by a buffer that opens and closes only its own packages; the engine builds cold and warm byte-equal and the suite passes. Depends: habu-reject-a-loaded-37d1c8c7. Claim: unassigned.
