---
title: Reject owner package reopen
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T06:16:56.099585+02:00"
blocks:
  - habu-record-owner-namespace-8ca3b072
---

Problem: CHECKER-PACKAGE has direct callers such as VERIFY:SOURCE-BUF that bypass native C-PACKAGE, so native-only owner checks cannot enforce replay. Result: keep the existing CTOR-PKG?-XT hook and error path. Package XREF-OWNER privately installs one composed predicate that returns `TFAM-CTOR-PKG? or XREF-OWNER:NAME?` before any user source, verifier, replay, JIT, or AOT input. C-PACKAGE relies on CHECKER-PACKAGE and adds no second name classifier. Owner: XREF-OWNER hook installation and verifier/package tests only; no new hook or global definition. Production red: after a real owner marker exists, VERIFY:SOURCE-BUF calls CHECKER-PACKAGE directly and accepts the same package name. Acceptance: direct checker, native source, VERIFY:SOURCE-BUF, replay, mixed-case, evaluated, JIT, and AOT reopen attempts reject through the existing E-CTOR-PROTECTED path before checker or native package mutation; generated-constructor protection remains exact; unmarked packages still reopen; source and fixpoint paths prove the composition is installed before external input. Forbidden: TFAM owner-family scan, new hook, unowned global, optional post-boot install, native name scan, second registry, source scan, allowlist, compatibility, or lint. Smallest owning check: close one real marked package, reject its name through VERIFY:SOURCE-BUF, keep one existing generated-constructor rejection, and reopen an unmarked control. Claim: unassigned.
