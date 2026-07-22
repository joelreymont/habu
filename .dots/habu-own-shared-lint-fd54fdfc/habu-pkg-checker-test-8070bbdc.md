---
title: Package checker test driver
status: active
priority: 1
issue-type: task
created-at: "2026-07-22T17:33:11.737490+02:00"
---

Files: tools/check-test-lib.f and tools/check-test.f. Open package CHECK around every outer test region and reopen it after each fixture package. Make outer test buffers and helpers private with short tails. Convert the fixture packages into self-contained providers: CKT-PKG-LINEAR publishes exactly GOOD$, CROSS$, GLOBAL$, and DISTINCT$; CKT-PKG-FAM publishes exactly GOOD$, TWO$, BOGUS$, JSON$, PRIV$, and NONAME$; CKT-ORIGIN publishes exactly SCAN$, VERIFY-BASE, and VERIFY-NEXT. Remove the later LINEAR/FAM action blocks and move their assertions into private CHECK test words using the qualified providers. Move the ORIGIN scan/base assertions into private CHECK test words using its qualified providers. Preserve every generated source and diagnostic string byte-for-byte. Publish CHECK:TEST ( -- ) as CHECK's sole entry and change tools/check-test.f to call it. The production checker remains global in this leaf, so existing CHK-* calls stay unchanged until the checker-core leaf joins CHECK. Acceptance: no CKT-/CKTP outer implementation name remains global; fixture packages expose only the listed providers; all existing checker cases and exact diagnostics run; no compatibility alias or duplicated assertion logic. Verify: bin/hb --load tools/check-test.f, focused nominal/declaration and origin cases, typed-local-diff-lint, package-diff mutation, host-lint, filemap-lint.

Claim: agent=checker_test_pkg workspace=.jj-ws/habu-pkg-checker-test-8070bbdc.
