---
title: "tools: behavioral bootstrap-hide regression (marker order)"
status: open
priority: 2
issue-type: task
created-at: "2026-07-12T22:59:23.779318+02:00"
---

Merge review 2026-07-12 LOW: tools/bootstrap-codegen-test.f:258-262 pins the IMK-NDICT0/SEQ/BOOT-HIDE-DICT-FROM-EARLIEST fix by SUBSTRING only - it would stay green if the tokens appeared in a comment or the markers were passed in the wrong order. The behavioral property (recovery hides from the EARLIEST of the two markers; -1 sentinel == BFR-NOT-FOUND) is only exercised by a real gforth recovery run. Add a behavioral test: forge a dict image (or drive the shell functions directly) with IMK-NDICT0 earlier than SEQ and assert the hide index picks the earlier record; red-prove by reverting to single-marker in a scratch copy. Files: tools/bootstrap-codegen-test.f, tools/bootstrap.sh (read-only).
