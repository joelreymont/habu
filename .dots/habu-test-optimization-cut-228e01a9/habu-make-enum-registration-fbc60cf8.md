---
title: Make ENUM registration linear in variants
status: open
priority: 2
issue-type: task
created-at: "2026-10-01T05:07:21.834496+02:00"
---

Problem: loading one ENUM of 325, 650 and 1302 variants takes 0.17, 0.46 and 1.48 s on the qqrsrlsv engine (superlinear); a profile shows TFAM-CTOR-WORD? doing CORE-STR=CI scans and 66% of samples unattributed to a word. Found by the r4-rows-misc lane while cutting check-cli-boundary. Acceptance: attribute the growth by measurement to the step that grows faster than linearly; make registration linear (or n log n) in variants with identical declarations, diagnostics and image output; time 325, 650, 1302 and 2600 variants before and after. Files: src/core/ (where the measurement points). Verify: the enum and declaration suites (test/enum-decl-suite.f, test/decl-replay-verify-source.f), tools/check-test.f, native build convergence, two-generation build. Depends: none. Ownership: ENUM registration cost.
