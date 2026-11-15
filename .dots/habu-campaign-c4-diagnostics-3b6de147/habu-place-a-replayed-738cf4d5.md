---
title: "Place a replayed row's rejection at its use"
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T06:23:15.658994+01:00"
---

Problem: tools/event-closure-lib.f EC-CONTEXT-EXPAND (:283-310) re-reads a child with RUN-IN, which replaces SD-BUF, then replays the parent's remaining rows against it, so tools/source-discovery.f SD-R-CONTEXT-FAMILY (:465) and SD-R-CONTEXT-KEY (:552) read the child's bytes at the parent's offsets (record "at `- ) ;\n: b`"). The record also names tools/public-signatures.f with no line, and the tool exits 67 on an uncaught -4100. Reproducers /tmp/jerry-ctxrec/r1, r2 under `bin/hb --load tools/public-signatures.f -- FILE`. Acceptance: the record stands at the use, in its file and line, rc 70; r2 still right. Depends: a design of where discovery keeps each file's bytes. Claim: unassigned.
