---
title: Rename generated-declaration-protection.f to what it holds
status: open
priority: 3
issue-type: task
created-at: "2026-11-06T14:51:54.242001+02:00"
---

Problem: after lane B (Drop the constructor-wordlist protection) src/core/generated-declaration-protection.f holds only package DECLARATION-TRANSACTION's runner (RUN, INSTALL-RUNNER, the capacity preflight and snapshot-reset chain), loaded after every participant; its name says protection. Acceptance: the file becomes src/core/declaration-runner.f, a move with no code change; every path naming it follows: src/habu/boot-files.f, src/habu/native-runtime.f, test/mcode-window-prepare.f, and any other `rg generated-declaration-protection` hit. Files: those. Verify: a candidate build and the rows that load the file (type-decl, structure-decl, enum-decl, declaration-replay-source). Depends: lane B landed. Ownership: jerry. Claim: unassigned.
