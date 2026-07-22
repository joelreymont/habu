---
title: Package checker core
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T17:33:12.157917+02:00"
blocks:
  - habu-pkg-checker-test-8070bbdc
  - habu-pkg-gate-dictionary-e0bd3a9c
  - habu-pkg-gate-diagnostics-4148169f
---

Files: tools/check-core.f, tools/check.f, tools/check-main.f, plus call updates inside package CHECK and GATE-DIAGNOSTICS. Join package CHECK, keep all production state and helpers private with short tails, and publish MAIN ( -- ), RESET ( -- ), CAPTURE-OFF ( -- ), OPT ( ptr u8 n -- ), SOURCE ( ptr u8 n ptr u8 n -- ), FILE ( ptr u8 n -- ), and RUN ( -- n ). RESET starts an empty direct session; OPT applies one validated command option; SOURCE selects an in-memory source and label; FILE selects one path; RUN materializes the selected input and executes the checker. CHECK's test module uses private package access instead of exported cells. GATE-DIAGNOSTICS uses only this public session API. Keep declaration parsing and diagnostics unchanged; continue calling still-global lint provider APIs in this leaf. Acceptance: no CHK-* definition or storage remains global; no mutable cell or internal buffer is public; command, direct, list, preverify, all-errors, JSON, nominal, and package tests stay exact; no compatibility alias. Verify: tools/check-test.f, gate diagnostics, gate dictionary, checker strict/all-errors slices, typed-local-diff-lint, package-diff mutation, host-lint, filemap-lint.
