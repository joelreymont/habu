---
title: Print a rejection through one package
status: closed
priority: 2
issue-type: task
created-at: "2026-11-14T16:00:42.332954+01:00"
closed-at: "2026-11-14T17:53:12.381008+01:00"
---

Problem: rejections print through five place computations (src/habu/layout.f SRCLOC, src/core/checker.f FILE-BUFFER?/LINE-AT, src/core/rejection.f's armed source (SOURCE!, MAP!, FILE!, LOCATE), src/compiler/native/definition.f LINE-OF/SAY-WHERE, src/lib/verify-diagnostics.f ORIGIN) and printers that exist only after render.f loads, so a rejection raised before it, or in interpreter.f, has no place and no JSON. Acceptance: package REJECTION (src/core/rejection.f) gains the opener, place, note and help parts and two closers; it prints the campaign's form and JSON keys from the same calls, resolves places by the campaign's one rule; ?TARGET (src/core/include.f) is converted and a test pins its prose and JSON forms; Zen, Tender and Maki build. Depends: none. Claim: agent=jerry-worker workspace=.jj-ws.noindex/jerry-ra2.
