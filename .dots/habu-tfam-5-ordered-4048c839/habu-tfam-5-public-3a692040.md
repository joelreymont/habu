---
title: "TFAM 5: public-signatures + hb-build cache keys consume event closure"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T08:53:59.614788+02:00"
---

public-signatures (tools/public-signatures-core.f :476) loads package/family metadata from the shared ordered event log before rendering rows (currently single-file lex, no cross-file closure). hb-build (tools/hb-build-lib.f HBB-KEY-* :453-517, HBB-SRC$ :720) AOT/REPL/object cache keys must include the ordered replay closure, not only the hardcoded top-level HBB-SRC$. Reconcile the stdin driver closure (src/core/include.f, src/habu/aot-capture.f, src/habu/stdin.f) through one shared stdin manifest per gate 17e. Depends on event-log + discovery dots.
