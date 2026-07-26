---
title: Seal packages owning private destructure
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T20:54:24.344097+02:00"
blocks:
  - habu-parse-structure-destructure-fae64cbb
---

Problem: package-name equality is not authority because user source can reopen an ordinary package; DESTRUCT owner would otherwise be bypassed by reopening the owner and invoking destructure. Required result: any package that successfully declares an owner-policy structure is marked for automatic sealing when that original package block closes. Keep both wordlists writable until close, then protect the package record plus public/private WIDs through the existing native/checker protected-WID authority. After close, package reopen, qualified definition, set-current or raw-WID publication, export/re-export, undefine, dictionary truncation, AOT entry, and case aliases must fail closed in native, checker, bootstrap, replay, and restored images. Preflight protection capacity before close mutation; a failed declaration or failed close publishes no partial seal. No test-origin bypass, environment flag, or friend spelling is allowed. Tests must use public behavior or a narrow typed production provider/observer owned by the module. Owner: package-close sealing triggered only by DESTRUCT owner. Dependency: habu-parse-structure-destructure-fae64cbb. Acceptance: original-block access remains valid until close; every post-close route above rejects; hostile nested evaluate and restart fixtures reject; packages with no DESTRUCT owner remain reopenable, proven through the real white-box idioms in run-budget-cal-test.f and json-read-perf-phase-test.f plus a focused positive fixture; seal/AOT/fixpoint gates pass.
