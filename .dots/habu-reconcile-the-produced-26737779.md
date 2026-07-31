---
title: "Reconcile the produced tape with the elaborator's frame"
status: open
priority: 1
issue-type: task
created-at: "2026-07-31T18:28:19.999708+02:00"
---

Full context: src/compiler/native/feed.f now produces a tape from the checker's own reader (src/core/checker.f CHECK-SCAN), and that reader never sees the definition frame: the engine hands the check hook the RECONSTRUCTED definition text - name, declared signature, body - with the opening ':' and the closing ';' already consumed by the compiler and backslash comments and whitespace runs already gone. src/compiler/native/elaborate.f (package NELAB) reads the opposite shape: it expects token 0 spelled ':', token 1 the name, both consumed while interpreting, and a closing ';' consumed while compiling. So a real produced tape cannot be elaborated today, and the only tapes NELAB accepts are the ones its own tests build. One of the two has to move, and the frame facts are already dotted out of NELAB by habu-bind-the-colon-ea509e61. Acceptance: NELAB elaborates a tape produced by NFEED from a real colon definition end to end, with the definition frame taken from the checker environment rather than from token spellings; a test compiles one definition through the production path and elaborates its tape without any hand-built tape in the middle. Depends on habu-feed-the-src-f7ed8733 and habu-bind-the-colon-ea509e61.
