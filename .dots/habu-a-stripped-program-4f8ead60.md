---
title: A stripped program cannot require genio-core.f
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T15:24:23.337709+02:00"
---

Problem: TERMINAL-KEY (src/lib/genio-core.f:225-226) calls KEY1, a word of src/habu/repl.f (:83), so tools/hb-build.f refuses any stripped program that requires genio-core.f with 'E-UNDEFINED: KEY1', rc 74; with --repl the same source builds. Reproducer: printf 'require src/lib/genio-core.f\n: MAIN ( -- ) ;\n' > g.f; bin/hb --load tools/hb-build.f -- g.f -o prog. genio-core.f's header does not say it needs the REPL. Acceptance: either a stripped program can use GENIO devices (the terminal row's key and line words come from somewhere a stripped program has), or the header and the card say GENIO needs --repl and the refusal names that. Once genio builds stripped, test/file-objects.f builds test/fixtures/file-objects/kept-genio.f with the stripped programs, and REPL-BUILD (test/file-objects.f:558), TOOL-BUILD's flag and GENIO-RUN (:780) go. Depends: none. Claim: unassigned.
