---
title: A stripped program cannot require genio-core.f
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T15:24:23.337709+02:00"
---

Problem: src/lib/genio-core.f:238-241 TERMINAL-KEY calls KEY1, a word of src/habu/repl.f, so tools/hb-build.f refuses any stripped program that requires genio-core.f ('E-UNDEFINED: KEY1 at src/lib/genio-core.f:241', rc 74); with --repl the same source builds. Reproducer: printf 'require src/lib/genio-core.f\n: MAIN ( -- ) ;\n' > g.f; bin/hb --load tools/hb-build.f -- g.f -o prog. genio-core.f's header does not say it needs the REPL. Acceptance: either a stripped program can use GENIO devices (the terminal row's key and line words come from somewhere a stripped program has), or the header and the card say GENIO needs --repl and the refusal names that. Files: src/lib/genio-core.f, src/habu/repl.f, docs/forth-card.md. Verify: the reproducer. Depends: none. Ownership: unassigned. Claim: unassigned.

Cost while this is open: test/file-objects.f builds test/fixtures/file-objects/kept-genio.f with --repl (build N), which makes a second kernel, about 9 s of the test's 32 s (17.9 s before the kept-handle cases). Once genio builds stripped, kept-genio.f's CHECK joins kept-main.f (build Q) and REPL-BUILD, TOOL-BUILD's flag and GENIO-RUN go.
