---
title: Remove unused AOT capture dump
status: open
priority: 3
issue-type: task
created-at: "2026-07-19T20:46:52.205999+02:00"
---

src/habu/aot-capture.f:565-578 defines ACAP-. as a bring-up-only textual dump. An exact repository search finds no call, tick, postpone, export, documentation command, or test reference to ACAP-.; the only occurrence is its definition. Because aot-capture.f is compiled into every stdin metabuild host, this dead word consumes parser/checker/JIT work and transient dictionary/code space on every engine build while providing no reachable debugging interface and no final-bin functionality. Delete ACAP-. and its bring-up comment. Add a source/reachability regression only if the owning dead-code/filemap lint can express it; otherwise the proof is the exact zero-reference search plus native fixpoint parity. Files: src/habu/aot-capture.f and owning source inventory only if required. Verify: exact rg reference census, hb-build focused gate, native fixpoint and AOT positive/negative slices. Depends: none. Ownership: ACAP-. only; do not change capture serialization or its regressions.
