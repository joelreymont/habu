---
title: Cut the post-harness dead surface, shrink the base reader
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-19T10:00:28.308771+02:00\""
---

Problem: the harness cut left tools/codegen-compare-core.f a 577-line measurement store whose only live part is the timing kernel and the two value projections; tools/codegen-compare-clang.f keeps MEASURE/CALIBRATE/FN@/FN!/FN-CELL, the sole callers of core's MEASURE-CLANG/FLOOR!/CALIBRATE; tools/judge/base.f carries a declared-count check, malformed-row hardening, a reference column it stores and never reads (with a comment at :53-54 describing a report that does not exist) and a dead FINDINGS@; migrated{2,3,4,5}.f require core and lib/errors.f and name nothing from either; JUDGE-PASS:INPUTS has no caller; nine JUDGE-ROW publics and CODEGEN-MACHO:NAME$ have no caller outside their own file.

Acceptance: codegen-compare-core.f is deleted and its live kernel (REPS RUNS PICOS-PER-NS PICOS-MAX FASTEST SLOWEST RUN-ONCE SAMPLE TIME-RUNS TIME-ONLY REAL-BITS FLAG-BITS) lives in one owner with every requirer and every generated-text spelling updated; clang.f keeps only PRESENT? ABSENT-WHY$ FLAGS$ TEXT-BYTES POOL-BYTES SETUP-CALL OPEN; base.f keeps exactly two capabilities - name the differing rows and their directions on a byte-compare disagreement, and the grew-vs-shrank asymmetry attacked both ways - plus CHECKED$/MARK-AT, the row scan/store, both direction checks, both traffic checks and the five counters; base-test.f keeps the fixtures that attack those two; the dead requires and the zero-caller publics are gone or private; the exit code stays the byte compare in tools/judge.f.

Files: tools/codegen-compare-core.f (deleted), tools/judge/cost.f, tools/judge/report.f, tools/judge/corpus3.f, tools/judge-test.f, tools/codegen-compare-clang.f, tools/judge/base.f, tools/judge/base-test.f, tools/judge/pass.f, tools/judge/row.f, tools/codegen-compare-macho.f, tools/codegen-compare-migrated{2,3,4,5}.f, lib/errors.f, docs/codegen-parity.md.

Verify: install --force fixpoint twice byte-identical; bin/hb --load test/run.f rc 0 with no FAIL/RED; bin/hb --load maki/test.f; bin/hb --load tools/judge.f -- --check reports 46 rows agree; bin/hb --load tools/judge-timed.f loads and runs; bin/hb --load tools/lint/schedule-lint-test.f 0 unreached 0 findings; both checked diff lints; tools/error-code-lint.f 0 findings; tools/dot-dep-lint.f 0 findings.

Depends: none. Closes fold-in item 2 of habu-rename-the-sixteen-0fb676c9 (the dead requires in migrated{2..5}.f).

Ownership: the files above.

Claim: agent=prune-1 workspace=.jj-ws/habu-thecut
