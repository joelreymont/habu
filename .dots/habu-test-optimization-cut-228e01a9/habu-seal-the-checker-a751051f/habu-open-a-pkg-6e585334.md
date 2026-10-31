---
title: Open a package inside another
status: active
priority: 2
issue-type: task
created-at: "2026-10-31T19:50:11.762392+02:00"
---

Problem: `package OI-A package OI-B` dies `package at <file>:1`, rc 75 (test/outer-interpret.f ~759-760), so no package can hold parts. First engine lane of the nested-package design (Joel 2026-10-31, LESSONS.md "packages"; /tmp/jerry-big/pkg-design.md). Acceptance: `package B` in A's private section opens the part A:B in both interpreters, with the checker's mirror; under A's public section, under a using A opened, or inside a part it is refused rc 75; `;package` of the part returns to A's private section; a throw inside a part restores both levels; at top level B:X, A:X and `using B` are unknown and a top-level B is a separate package. Lookup through the parent is the next lane. Brief /tmp/jerry-big/pkg1a-brief.md. Workspace: .jj-ws.noindex/jerry-pkg1a (worker). Claim: jerry.
