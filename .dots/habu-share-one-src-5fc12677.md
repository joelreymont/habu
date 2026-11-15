---
title: Share one source builder among the tests
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T18:14:01.622868+02:00"
---

Problem: test/signature-nesting.f (:30-49) and test/structure-decl-suite.f (package SDDEEP) each build test source text in a buffer of their own, beside the shared GE-SRC builder in test/gate-common-lib.f, which a light suite cannot load (it pulls in processes and file systems). Acceptance: one builder in src/lib/test (beside TEST-EVAL) that every test building source text uses, GE-SRC's buffer included. Depends: none. Claim: unassigned.
