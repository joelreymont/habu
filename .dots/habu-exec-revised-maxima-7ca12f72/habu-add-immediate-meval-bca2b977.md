---
title: Add immediate meval* clearsign mitigation
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-03-07T19:20:07.513927+01:00\\\"\""
closed-at: "2026-03-07T19:59:11.735854+01:00"
close-reason: done (updated lib/maxima-post-load.lisp meval* override to restore normal-path clearsign cleanup without reintroducing upstream unwind-protect wrapper; validated with direct ./zig-out/bin/habu probe showing *local-signs* becomes NIL after meval*)
---

lib/maxima-post-load.lisp:89-103; ../maxima/src/suprv1.lisp:69-85; ../maxima/src/compar.lisp:965-976. Root cause: current meval* override skips with-top-level-environment cleanup and leaks sign-state across sequential evaluations. Fix: add an interim explicit cleanup path without reintroducing the unwind-protect crash. Why: Stage-1/2 pass-fail numbers are not trustworthy while sign state accumulates.
