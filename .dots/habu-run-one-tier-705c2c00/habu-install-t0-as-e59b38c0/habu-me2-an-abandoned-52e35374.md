---
title: "me2: an abandoned definition keeps the outer catch (EM-ABORT-COMPILE-STATE clears HND/RSP/LOOPSP since the Intel merge)"
status: active
priority: 2
issue-type: task
created-at: "2026-10-30T17:38:03.642358+02:00"
---

Found by the ME worker. plkwlqyt moved the three stores into EM-RESET-COMPILE-STATE; xwyrlqpu (Intel merge) put them back in the abort. Workspace .jj-ws.noindex/jerry-me, change on qtznpllo. Brief /tmp/jerry-big/t0-me2-abort-catch-brief.md; regression case in test/engine-suite.f multi-error block.
