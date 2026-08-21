---
title: Package all-errors tests
status: closed
priority: 1
issue-type: task
created-at: "2026-07-22T17:33:12.358342+02:00"
closed-at: "2026-07-25T14:39:49.865150+02:00"
close-reason: "Landed in master@origin as commit 79c50e5a9dbf 'Package all-errors core and reset checker scope', atomically with habu-pkg-all-errors-86fd1b4a as that commit message states. The pause note is discharged: the caller-package leakage the migration exposed was corrected by the neutral-scope change in the same commit, so the saved reviewed refactor of tools/check-all-errors-test.f landed with it."
---

Files: tools/check-all-errors-test.f only. Put every outer test region in private package CHECK-ALL-ERRORS-TEST, closing and reopening it around CAE-PKG-NOMINAL. Make CAE-PKG-NOMINAL a self-contained fixture-data package: define its own private LF and publish exactly SOURCE$; preserve the generated source bytes. Remove the later reopened fixture TEST block. In CHECK-ALL-ERRORS-TEST define private TEST-PACKAGE-NOMINAL, which sets the case, calls CAE-PKG-NOMINAL:SOURCE$, and performs CHECK-SUPPORT-PARITY. Make all outer state and helpers private with short tails, rename CAE-MAIN to RUN, and invoke RUN inside the final package region. Call the landed CHECK-ALL-ERRORS public API with qualified names and package CHECK private helpers through its owner; publish no test bridge. Acceptance: no CAE-* outer implementation name remains global; the fixture exposes only SOURCE$; every support, cascade, duplicate, JSON, nominal, trust, export, and replay case remains active; all embedded source and diagnostic strings are byte-identical; no alias or duplicate assertion logic. Verify: bin/hb --load tools/check-all-errors-test.f, typed-local-diff-lint, package-diff mutation, host-lint, filemap-lint.

Claim: agent=all_errors_test workspace=.jj-ws/habu-pkg-all-errors-042245b9.

Paused: the package migration exposed caller-package leakage in the all-errors checker scope. Resume from the saved reviewed refactor only after habu-pkg-all-errors-86fd1b4a lands with that correction.
