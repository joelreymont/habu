---
title: Shorten CHECK dependency internals
status: open
priority: 1
issue-type: task
created-at: "2026-07-23T02:12:37.961302+02:00"
blocks:
  - habu-shorten-check-session-3cd31b26
---

Why: dependency discovery and source-list expansion remain a separately testable block of legacy CHK-prefixed private words after the CHECK session owns selection and materialization. Owner: package CHECK. Files: tools/check-core.f and tools/check-test-lib.f. Rename only the private dependency concern: dependency path/state/order storage and accessors, directory stack, ordered loader-event discovery, direct dependency insertion, recursive expansion, cycle handling, required-form rendering, and expanded-source buffer helpers. Use short package-local tails; keep every caller inside CHECK and expose no new public word or storage. Preserve canonical path handling, dependency order, duplicate suppression, cycle diagnostics, missing-file diagnostics, dynamic-loader rejection, source-list behavior, and expanded bytes exactly. Acceptance: zero executable CHK-prefixed name remains from the dependency row beginning at the dependency bounds/accessors through list-path materialization; no caller outside CHECK reaches a private dependency word; hostile include, included, require, required, provided, comments, strings, duplicates, cycles, missing files, reordered events, and dynamic paths exercise the real discovery and CHECK run path with byte-exact results. Forbidden: aliases, second dependency graph, copied discovery logic, file-level allowlists, suffix path matching, or semantic changes. Pre-change proof: a representative shortened dependency definition fails package ownership while the same definition inside CHECK passes. Verify through tools/check-test.f dependency/list fixtures, source-discovery tests, exact diff ownership/type, host, file-map, gate diagnostics, and gate dictionary.
