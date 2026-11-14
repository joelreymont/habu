---
title: Print a refusal through one package
status: open
priority: 2
issue-type: task
created-at: "2026-11-14T16:00:42.332954+01:00"
---

Problem: refusals print through five place computations (src/habu/layout.f SRCLOC, src/core/checker.f FILE-BUFFER?/LINE-AT and DSRC/DMAP/DIAGFB/DIAG-POS/DIAG-LOCATE, src/compiler/native/definition.f LINE-OF/SAY-WHERE, src/lib/verify-diagnostics.f ORIGIN) and printers that exist only after render.f loads, so a refusal raised before it, or in interpreter.f, has no place and no JSON. Acceptance: the campaign's package in src/core/util.f (opener, place, note, two closers taking the help text) prints the campaign's form and JSON keys from the same calls, resolves places by the campaign's one rule (DSRC/DIAGFB moved out of checker.f), and replaces ERROR-TEXT:PREFIX; one converted refusal per mode (--load prose, --json-errors) shows the form; Zen, Tender and Maki build. Depends: none. Claim: unassigned.
