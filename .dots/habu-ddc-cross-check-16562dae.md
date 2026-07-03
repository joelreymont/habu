---
title: DDC cross-check of the fixpoint
status: open
priority: 2
issue-type: task
created-at: "2026-07-01T22:54:40.836873+02:00"
---

Diverse Double-Compiling: build bin/hb via the native fixpoint AND via the independent Gforth bootstrap chain (tools/bootstrap.sh HABU_BOOTSTRAP_CHECK_ONLY=1 path, docs/bootstrap.md), require byte-identical output; a seed backdoor must then be mirrored in Gforth to survive - reduces seed trust to 'no coordinated cross-host backdoor'. Deliverable: Habu-native comparison tool (tools/ddc-verify.f) running both chains and diffing sha256 of the artifacts, documented in docs/bootstrap.md, runnable as an explicit (not per-commit) audit gate. Blockers: requires working Gforth recovery host; keep it optional-but-audited (HABU_ALLOW_BOOTSTRAP=1).
