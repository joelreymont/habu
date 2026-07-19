---
title: Bound report columns before writes
status: open
priority: 1
issue-type: task
created-at: "2026-07-19T21:15:26.506288+02:00"
---

lib/report.f:12-24 fixes COL-MAX at 64, but COL+ writes COL-HA, COL-HN, and COL-AL through unchecked address arithmetic before COL-XT's generated typed-buffer store checks its index. On the 65th column, the three raw stores corrupt adjacent dictionary data before the later typed store throws; COL-N is therefore also left inconsistent with the partially mutated column tables. Fix the root contract by owning all column fields in one package-local typed structure/layout buffer and preflighting capacity before the first mutation, then committing the row and count as one operation. Throw a dedicated named report-capacity error without changing any column state or backing bytes. Add focused canary regressions proving exactly 64 columns succeed, column 65 fails before every store, the count and all prior columns remain byte-identical, adjacent canaries remain intact, and reset/reuse behaves correctly. Preserve rendered output and published COL+ behavior for valid inputs. Files: lib/report.f, lib/report-test.f, lib/errors.f, lib/std.manifest and documentation if the public error contract is listed. Verify the exact report owning load, typed-local diff lint, stdlib manifest/coverage, host/filemap/error-code/dot lints, and the full native gate. Ownership: report column capacity and transactional storage only; package migration and alignment typing are separate work.
