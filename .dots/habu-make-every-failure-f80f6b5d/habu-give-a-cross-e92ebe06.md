---
title: "Give a cross build's top-level site its line"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-12T20:12:46.100999+02:00"
closed-at: "2026-11-12T20:30:40.126230+02:00"
---

Problem: src/core/checker.f:29474 CHECK:TOP-SITE answers no line when code compiled inside a cross (Wasm) build calls it at a file's top level, where the native build answers the line: run at master 3732ec8c, a file two requires deep, line 4 natively and none in the Wasm build, while CHECK:TOP-SITE-PATH$ and the cursor offset agree. It reads TOK-SPELL-A (:14592), a plain variable only the checker's token steps set; in a cross build the build's copy of checker.f answers, and the running hb's checker sets only its own copy, so the build's stays null. Its callers today (src/core/render.f:917, :1877) run in hb, so a program calling it in a cross build is what meets it. Acceptance: CHECK:TOP-SITE reads the token the engine read last (TKA-CELL, hb's own in a cross build, as the cursor is), which a top step's token always is, and in a Wasm build CHECK:TOP-SITE at that top level answers the same line as natively. Depends: none. Claim: agent=jerry-worker workspace=.jj-ws.noindex/jerry-tka.
