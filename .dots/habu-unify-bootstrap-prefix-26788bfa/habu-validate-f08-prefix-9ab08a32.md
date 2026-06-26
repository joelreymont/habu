---
title: Validate F08 prefix-list refactor
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-06-25T14:01:17.728071+02:00\\\"\""
closed-at: "2026-06-25T14:15:49.255509+02:00"
close-reason: "completed: trust-lint passed with 236 TRUST sites and 318 manifest rows; tools/bootstrap-codegen-test.f passed including native/bootstrap prefix mirror checks; bin/hb test/engine-suite.f passed; focused build-helper-fixtures passed after fixing dropped BL,/ZBYTES, punctuation; full native gate passed; recovery probe with gforth 0.7.3 exited 69 on {: locals probe and bin/hb sha256 stayed d2d79b59c70a4de0d160b886ededde6941a92feb29bef3b105d26800b1d3793b"
---

Child of F08. Current working copy has partial prefix-list refactor in bootstrap/cg/forth.fs, src/habu/habu2.f, tools/bootstrap-codegen-test.f, and TRUSTED.md. Root cause: F08 implementation was started but not fully validated before handoff. Fix: rerun trust-lint after TRUSTED.md line refresh, tools/bootstrap-codegen-test.f, bin/hb test/engine-suite.f, full native gate from docs/bootstrap.md, and the no-binary recovery probe; installed gforth 0.7.3 is expected to fail the {: locals probe with rc 69 and leave bin/hb unchanged. Why: F08 changes bootstrap/native source selection and must be proven before commit.
