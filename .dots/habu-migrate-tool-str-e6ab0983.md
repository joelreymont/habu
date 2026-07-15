---
title: "Migrate tool string callers to STR:"
status: active
priority: 3
issue-type: task
created-at: "\"2026-07-15T15:05:05.436150+02:00\""
---

Full context: MODEL-CAD-V2-PLAN.md B5.5a legacy-STR census, tool lane. Migrate raw STR calls in: tools/codegen-role.f(+test), hb-build-lib.f, bootstrap-codegen-test.f, build-fixpoint.f(+test), ptx/saxpy-test.f, ptx/perf-registry.f, lint/text.f, lint/text-foundation-test.f, public-signatures-core.f, stale-status-lint-core.f, suite-coverage-lint-test.f, typed-local-diff-lint-test.f (FIND-SUB/INDEX-OF/SPLIT-NEXT/BUF-* per census; four files use BUF-APPEND-C). Blocks on the STR:BUF-APPEND-C owner extension. Overlap note: build-fixpoint/codegen-role/hb-build-lib/text-foundation-test touched by the MEM/VEC tool wave (sequential). Build-path caution: build-fixpoint/hb-build-lib require fixpoint x2 proof. Acceptance: fresh rg census empty; focused suites + lint fleet byte-identical + full run.f. Files: the 14 listed + focused tests. Ownership: the 14 tool files.

Claim: agent=toolstr workspace=.jj-ws/fable-toolstr
