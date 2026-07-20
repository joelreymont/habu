---
title: Register lint diff tests
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-07-19T23:18:37.541183+02:00\\\"\""
closed-at: "2026-07-20T12:58:48.729687+02:00"
close-reason: "Merged 523d2f17: diff-parser and lint-intern-set suites registered in the gate, frame-suite registration verified complete, no cache-key rows needed (manual-gate suites bypass the result cache), and a fail-closed LINT-UNREGISTERED inventory rule now rejects any unregistered tools/lint test (negative-proven)"
---

tools/lint/diff-test.f and tools/lint/diff-frame-test.f are present in FILEMAP but absent from every owning gate and cache-key inventory. The closed shared-parser work cites a 495-line suite that the full gate never executes, and the new framing suite is likewise manual-only, so regressions can land green. Register both suites in the native stdlib/lint gate and every cache key that owns their sources and dependencies. Add a suite-coverage inventory rule that discovers lint test modules and rejects any unregistered test or stale cache dependency. Prove each suite runs exactly once in the full gate, an intentional failure makes the gate fail with diagnostics, source/dependency changes invalidate the cache, and host/filemap/dot/full gates remain green. Files: tools/lint/diff-test.f, tools/lint/diff-frame-test.f, test/gate-stdlib.f, cache manifests and suite-coverage tests.

Claim: agent=lintreg workspace=.jj-ws/habu-register-lint-diff-ab335590
