---
title: Raise signature-lint per-file cap
status: open
priority: 2
issue-type: task
created-at: "2026-07-28T19:35:27.734913+02:00"
---

Full context: tools/signature-lint's per-file buffer cap SL-FILE-CAP is $10000 (65536) bytes. tools/package-diff-lint-test.f grew to 67005 bytes (paren-word fixtures) and now exceeds it; verified no current gate invokes signature-lint on test files (its gate use lints hb-build program sources via HBB-SRC$), so nothing regresses today, but any future gate widening or further test growth trips a silent-seeming capacity edge. Fix structurally: make the buffer MEM-backed (sized from FILE-SIZE) or raise the cap with a named rationale, and add a regression that lints a >64K file successfully. 
