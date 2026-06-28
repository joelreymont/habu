---
title: Require explicit redefinition before replacing words
status: active
priority: 1
issue-type: task
created-at: "\"2026-06-28T10:59:03.787980+02:00\""
---

Problem: Habu still permits last-definition-wins shadowing in runtime dictionary paths; duplicate-definition lints/reserved-name lints only approximate the invariant and can miss live paths. This caused reserved I/J and duplicate definition rc=70/debug churn. Static invariant: defining an existing visible word is an error unless the source explicitly opts into replacement by first undefining/forgetting that exact word. Owner: dictionary/compiler/checker boundary, not lint-only. Files: src/habu/habu2.f C-QUALIFY-DEF/C-REJECT-DUP-DEF, src/core/checker.f CHECKER-USIG-CERT-ADD/CHECKER-REC-NAME, package wordlist paths, tools/duplicate-definition-lint-core.f only as supplementary audit, docs/forth.md/docs/stdlib.md/STATUS.md docs. Fix: add a first-class explicit redefinition mechanism, likely UNDEFINE/FORGET-DEF that removes or tombstones a word/signature in the active wordlist; ordinary :, TRUSTED:, KERNEL:, create/variable/constant/defer/structure/enum definitions fail closed on duplicate visible names. CHECK!/tools/check.f must mirror runtime behavior for duplicate user signatures and deferred/checker metadata. Acceptance: same-file duplicate, cross-file reload duplicate, package public/private duplicate, primitive shadow attempt, defer duplicate, and checker signature duplicate all reject with a named diagnostic; explicit undefine then redefine succeeds and removes stale signature/defer/control metadata; gate tests cover runtime and tools/check.f; docs state redefinition is explicit only.
