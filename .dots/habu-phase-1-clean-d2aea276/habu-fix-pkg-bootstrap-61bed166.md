---
title: Fix package bootstrap and reader qualification
status: open
priority: 1
issue-type: task
created-at: "2026-04-01T22:06:02.097404+02:00"
blocks:
  - habu-unify-maxima-manifest-702701ab
---

Problem: reader auto-creates packages and qualified reads do not enforce CL package rules. Acceptance: pkg:sym and pkg::sym are canonical and package bootstrap is explicit. Files: src/reader/parser.zig:1016-1040,1168-1184; src/runtime/primitives/package.zig:315-316; ../maxima/src/maxima-package.lisp. Verify: reader/package regressions for missing package, external-only, and internal-only access. Blockers: habu-unify-maxima-manifest-702701ab.
