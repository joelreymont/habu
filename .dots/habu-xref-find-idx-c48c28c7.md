---
title: XREF-FIND-INDEX is called only by tests
status: active
priority: 2
issue-type: task
created-at: "2026-11-18T18:30:41.028306+01:00"
---

Problem: the global XREF-FIND-INDEX (src/habu/xref.f:360-367) and XREF-FIND-QUALIFIED-INDEX (:344-349) have no caller in src or tools once XREF:HIDE-DEFS-FROM and XREF:FORGET-DEFS-FROM go; ten test files call it (test/linker.f, test/code-reclaim.f, test/prof-rewind-child.f, test/x86-64-kernel-prof.f, test/fixtures/file-objects/build.f, test/compiler/arm64-place.f, arm64-place-unsafe-bad.f, arm64-link-run.f, arm64-link-data.f, arm64-link-data-bad.f). Acceptance: the two words are gone and each test reaches what it needs through a word a program has. Depends: habu-delete-xref-hide-1063358c. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-xf.
