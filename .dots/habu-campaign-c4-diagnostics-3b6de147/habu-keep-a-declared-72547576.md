---
title: Keep a declared symbol of any length
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T07:02:37.715793+01:00"
closed-at: "2026-11-16T04:07:35.767439+01:00"
---

Problem: src/lib/ffi-abi.f keeps declaration names in fixed buffers and rejects what does not fit, limits Habu set itself: FN-NAME-CAP (47 symbol bytes, SYM!), TOK-CAP (64-byte tokens, TOK!) and LIB-BASE-CAP (32-byte VERSIONED-LIBRARY base names, BASE!). Acceptance: a declaration keeps a symbol, token or base name of any length whole; the three limits and their records are gone. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
