---
title: Print a rejection through one package
status: active
priority: 2
issue-type: task
created-at: "2026-11-14T16:00:42.332954+01:00"
---

Problem: rejections print through five place computations (src/habu/layout.f SRCLOC, src/core/checker.f FILE-BUFFER?/LINE-AT and DSRC/DMAP/DIAGFB/DIAG-POS/DIAG-LOCATE, src/compiler/native/definition.f LINE-OF/SAY-WHERE, src/lib/verify-diagnostics.f ORIGIN) and printers that exist only after render.f loads, so a rejection raised before it, or in interpreter.f, has no place and no JSON. Acceptance: package REJECTION (src/core/rejection.f) gains the opener, place, note and two closers taking the help text; it prints the campaign's form and JSON keys from the same calls, resolves places by the campaign's one rule (DSRC/DIAGFB moved out of checker.f), and replaces ERROR-TEXT:PREFIX; one converted rejection per mode (--load prose, --json-errors) shows the form; Zen, Tender and Maki build. Depends: none. Claim: agent=jerry-worker workspace=.jj-ws.noindex/jerry-ra2.
