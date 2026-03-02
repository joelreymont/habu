---
title: Diagnose current defint/residu blocker
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-03-08T17:08:50.306323+01:00\\\"\""
closed-at: "2026-03-08T18:07:29.839044+01:00"
close-reason: "done: stale diagnosis replaced. Canonical-bootstrap probe with explicit (load \"lib/stdlib.habu\") showed ../maxima/src/defint.lisp and ../maxima/src/residu.lisp both load cleanly after maxima-load-all + maxima-post-load; old VM stack overflow comment in lib/maxima-loader.lisp was no longer true."
---

Files: lib/maxima-loader.lisp:69-71, src/interp/vm.zig:849-852, ../maxima/src/defint.lisp, ../maxima/src/residu.lisp. What: reproduce current failure under raised VM limits and identify whether blocker is value stack, call frames, catch/unwind depth, macroexpansion, or another compiler/runtime issue. Why: PLAN 3.2/5.3 depends on replacing stale diagnosis with current evidence. Verification: focused load probe capturing first concrete failing mechanism.
