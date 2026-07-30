---
title: Feed the source tape from the real lexer
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T14:49:48.021261+02:00"
---

Full context: src/compiler/native/tape.f can hold the stage N0 token stream and seal it under one digest, but nothing produces one from real Habu source yet - its only producers are its own tests. Design section 7.1 requires that checking, elaboration, diagnostics, and compilation all refer to the same tape, which means the engine's own reader has to append to it as it consumes tokens: kind, byte span, resolved spelling, literal value, parser mode, and expansion origin, once per token actually consumed. Required result: a producer that drives NTAPE:PUSH and NTAPE:PUSH-FROM from the real reader for the Wave 2 straight-line slice, and a checker result that carries the sealed tape digest. Acceptance: compiling a real colon definition through the production path yields a tape whose token count and spellings match the source; the checker's result binds the tape digest through NTAPE:VERIFY; a byte changed anywhere in the source changes the digest. Dependencies: the source tape and the frozen checker environment manifest.
