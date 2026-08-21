---
title: Compile native calls and exceptions
status: closed
priority: 1
issue-type: task
created-at: "2026-07-26T22:56:29.051905+02:00"
closed-at: "2026-08-14T11:20:33.521057+02:00"
close-reason: "Epic closed: all four children closed (return stack 92993f27, exceptions 6ceb7667, calls fef1fed5, typed locals 01db198b - the last two as satisfied-by-audit with clause-level evidence). The epic acceptance is covered: call-clobber/live-home validation (fef1fed5 evidence list), catchable in-process failures (the catch landing's differentials), half-publication negatives (front half native-migrate.f:1765; the back-half refusal test remains owned by open dot 2f4bfdc3)."
---

Full context: design Wave 4 adds typed locals, return-stack operations, direct/indirect calls, and throw/catch/evaluate edges under explicit ABI and transaction ownership. Acceptance: call-clobber/live-home validation, catchable in-process failures, and half-publication negatives pass.
