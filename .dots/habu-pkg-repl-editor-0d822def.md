---
title: Package REPL editor and stepper
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T21:50:17.679913+02:00"
---

Current master census: src/habu/repl.f:13-165 and src/habu/stepper.f:8-84 add 63 unowned definitions to the AOT bundle seeded into every installed bin/hb, with 383 definition-name bytes and at least 5,616 bytes of REPL/stepper DATA storage. The code is product-reachable but dormant until TTY/editor/step use; it is not loaded on demand and must not be deleted as dead payload. Reopen package REPL across editor and stepper. Export only the qualified boot installation used by stdin.f and the intentional documented step command (or a documented STEPPER:RUN surface); keep DATAB, terminal/history buffers, read hooks, token parser, cursors, formatting, and stepper state private. Update AOT boot-run names directly without aliases. Preserve pipe mode, TTY editor/history, token stepping, terminal restoration, exact output, AOT records/relocations, snapshot, cold/warm startup, and source-independent bare-binary debugging. Add old-global/private rejects, boot/install positives, pipe non-execution proof, TTY/history goldens, step state/error cases, AOT/snapshot restoration, and capacity/canaries. Measure persisted dictionary names, JIT/DATA/CODELEN, startup, editor, and step latency before/after; package privacy may shrink names but does not justify removing the resident feature.
