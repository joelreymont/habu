---
title: Deliver standalone native application images
status: open
priority: 1
issue-type: task
created-at: "2026-09-10T18:03:13.378322+03:00"
blocks:
  - habu-relocate-quotations-stored-f112fc21
---

Owner: Cedar. Native app images, current-process MAIN/argv startup, typed stored quotation relocation, fresh checked REPL and source-free recapture are integrated. Combined test/app-image.f passes. The real command bin/hb --load tools/hb-build.f -- --repl /tmp/cedar-polished-main.f -o /tmp/cedar-polished-app succeeds; running from /tmp prints MAIN output then compiles a new word and prints42, exit0. False generic declarations in lib/build.f and tools/build-fixpoint.f were corrected through this path.

Finish public source-root invocation, capture-ready shared libraries and complete Maki/Tender/Radar acceptance on a matching pair. Preserve MAIN effect rejection, startup throw diagnostics, argv/optional --, PTY REPL and repeated save coverage. Final combined native suite and concise runnable documentation remain required; the successful minimal app is not a complete consumer handoff.
