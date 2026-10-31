---
title: Gather the error text words in ERROR-TEXT
status: active
priority: 2
issue-type: task
created-at: "2026-10-31T16:21:51.918311+02:00"
---

Problem: c7c77ea3 made core package SOURCE-TEXT (src/core/util.f LINE; c155218a added DIGITS$), which has the name of Maki's own package (maki src/source-text.f:12); the engine seals it, so every Maki build dies at SEAL-PACKAGE, exit 84 (tim, 2026-10-31). Five files carry their own copy of writing text to descriptor 2 (definition.f ERROR-TEXT, check-hook.f SAY, outer.f SAY, main.f SAY, driver-io.f DRV-W2) and check-hook.f SAY-DEC copies DIGITS$. Joel 2026-10-31: "what not error text? wtf is a refusal? use standard programming terms! and you can have error line and column words, yes"; on the copies: "why have several?". Acceptance: package ERROR-TEXT holds LINE, DIGITS$ and TYPE (write to the error output); every copy that loads after util.f calls it; a program's own package SOURCE-TEXT loads; outer-interpret, tier, native-hookless-reject, runtime-regression and the rows covering main.f, driver-io.f and check-hook.f output pass on the candidate; installed, tim told. Files: src/core/util.f, src/core/checker.f, src/core/check-hook.f, src/compiler/native/definition.f, src/habu/outer.f, src/habu/main.f, src/habu/driver-io.f, src/habu/definers.f. Depends: none. Ownership: those files. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-etext.
