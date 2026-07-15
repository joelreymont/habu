---
title: "Tools: type diff event payloads"
status: open
priority: 1
issue-type: task
created-at: "2026-07-15T08:30:04.444158+02:00"
blocks:
  - habu-compiler-lower-unified-5f599080
---

Full context: tools/lint/diff.f must currently return a compact DIFF:event tag beside an out-of-band span/meta tuple because payload-capable ENUM, named fields, checker effects, and lowering are not on master. This leaves variant-specific payload invariants unenforced: NONE/FILE/HUNK/ADD/CONTEXT/DELETE consumers can observe or fabricate meaningless tuple cells. Fix: after unified ENUM checker/compiler lowering lands, declare full non-generic ENUM event 0 with named payload fields for file role plus path span, hunk text plus new-line role, and content span variants; make DIFF:LINE return only event; migrate typed-local and kernel-perf MATCH consumers with no SUMTYPE/PRODUCT compatibility. Acceptance: the checker rejects missing/swapped/fabricated payload fields; every MATCH is exhaustive; old/new file roles are nominal; parser behavior, diagnostics, hunk counts, line numbers, package API, and zero-allocation streaming remain deterministic. Files: tools/lint/diff.f, tools/lint/diff-test.f, tools/typed-local-diff-lint-core.f and test, tools/kernel-perf-lint-core.f and test. Verify: focused parser/consumer suites, checker negatives, typed-local self-diff, kernel-perf self-diff, lint-tools, host/filemap/trust/dot/stale/full gates.
