---
title: Rehome or retire the four orphaned stage0 fixtures
status: open
priority: 2
issue-type: task
created-at: "2026-08-15T18:50:46.053003+02:00"
---

Found by the schedule-lint disk audit (habu-make-schedule-lint-d9ca528d): four stage0 recovery-emitter fixtures are on disk with no runner and no lint row - test/bootstrap-using-ambiguous-src.f, test/bootstrap-using-unknown-src.f, test/bootstrap-wide-interpret-src.f, test/bootstrap-wide-tick-src.f. Their siblings ARE wired: bootstrap-using-src.f and bootstrap-using-checker-hook-src.f are named by tools/package-diff-lint-core.f STAGE0-ROW+ (lines 1116-1117), bootstrap-using-scope-src.f by tools/package-diff-lint-test.f:1233, bootstrap-wide-memory-src.f by test/candidate-validation.f:261. So the family is half-wired and these four fell out. They are not empty: each encodes a real stage0 refusal contract (ambiguous used-public name USING-AMBIGUOUS=94, unknown used package, wide-effect interpret, wide-effect tick) and each prints an ARMED marker a harness is meant to match. Decide per file: give it a RUN-CASE row in test/candidate-validation.f like its siblings, or delete it if the contract is covered elsewhere. TYPE-FIXES-PLAN.md:545-546 counts two of them, which suggests they were dropped during the type-DSL work rather than retired on purpose. Each carries a schedule-lint: allow-unscheduled pragma naming this dot until then. Files: test/bootstrap-using-ambiguous-src.f, test/bootstrap-using-unknown-src.f, test/bootstrap-wide-interpret-src.f, test/bootstrap-wide-tick-src.f, test/candidate-validation.f. Depends: none.
