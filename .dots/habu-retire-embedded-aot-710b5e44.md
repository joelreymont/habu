---
title: Retire embedded AOT REPL
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-17T14:01:16.273588+02:00\""
---

Stop-line child of habu-shrink-the-c-721f214d. Replace the 25.5KB metabuild-captured AOT REPL seed with checked live loading of target repl-term.f, repl.f, debug-watch.f, stepper.f, and debug.f only on the TTY path. Remove the special capture, seed loader/relocator, labels, buffers, closure role, fixed state, manifests, stale docs, and tests while preserving general hb-build AOT. Prove pipe/--load do not read REPL files; TTY fails closed on missing source; proc-pty/debugger/repl gates, native fixpoint, DDC explanation, size map, full owning gates.
