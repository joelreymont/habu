---
title: "Let FUNCTION: take str inputs"
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T15:46:14.287119+01:00"
---

Problem: Joel's rule (2026-11-19, in kevin's notice): "no agent should be using "^u8 n" anymore! it's either str or vector". FUNCTION: rejects str as an input (E-FFI-SYNTAX "'str' is not an input type", src/lib/ffi-test.f:840 shows the message; input sizes at src/lib/ffi-abi.f:849), so a binding that hands a span to C spells it ^u8 n: Maki's src/worker.f has 12 such rows, e.g. :66 C-WRITE-SHAPE ( n ^u8 n -- n ). Acceptance: a FUNCTION: input str passes its base and its byte length as two C arguments, in that order, as ^u8 n does today, on every target FUNCTION: supports; a test calls a C function through a str input. Depends: none. Claim: unassigned.
