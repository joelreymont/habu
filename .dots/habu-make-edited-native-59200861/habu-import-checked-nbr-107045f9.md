---
title: Import checked NBR package
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T14:29:20.869735+02:00"
blocks:
  - habu-capture-nbr-pkg-659e2546
---

Install one staged NBR contribution into a live fresh native compiler at the ordinary source-load position. Resolve dependency symbols, allocate code/DATA/dictionary/WIDs in source order, relocate internal/external calls and typed addresses, publish dictionary and checked effect/control facts, register protected WIDs and supplied source paths. AOT-FILE:READ/MERGE are staging readers and startup EM-SEED-AOT is not a live importer; add the missing owner API at the publication boundary. Relevant files: src/compiler/native/publish.f, src/habu/aot-file.f, dictionary owner, src/core/checker.f and package importer. Acceptance: imported NBR source is never loaded, public wrappers call private TARGET at shifted placement, a wrong-effect client is freshly rejected, and unsupported relocation/range refuses. Run focused E2E only in cleared CPU window.
