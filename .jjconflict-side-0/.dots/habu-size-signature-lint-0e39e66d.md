---
title: Size signature lint buffer dynamically
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T18:30:51.109296+02:00"
blocks:
  - habu-load-lint-sources-a553610f
---

Problem: `tools/signature-lint-core.f` reads source through the fixed 64 KiB
`SL-FILE-CAP`. A larger file fails with `file exceeds buffer` before any
signature is inspected, so mandatory enforcement does not cover current large
sources such as `src/core/checker.f`. Splitting a source is separate structural
work and does not remove the lint's obligation to read every valid source file
whole.

Required result: after `habu-load-lint-sources-a553610f` makes the shared
provider transactional, remove the private fixed source buffer and make
`SIGNATURE-LINT:FILE-AS` load exclusively through `LINT-SOURCE:LOAD` and
`LINT-SOURCE:TEXT`. The lint must inspect the complete file or refuse before
inspection with the provider's named file-system, allocation, or
representability result; truncation and partial scans are forbidden. At the
standalone command boundary, missing, unreadable, and non-regular inputs must
still name the input path and exit with status 1. Preserve every current
signature finding and JSON behavior.

Acceptance: a structural fixture larger than 64 KiB containing a valid
definition near the end is fully inspected and passes; the same fixture with a
late invalid signature fails for that signature, proving the tail was scanned;
ordinary small-file and hostile comment/string cases remain unchanged;
`src/core/checker.f` lints successfully on the exact tree; the signature
lint suite, its exact production command, typed-local diff lint, and package
diff lint pass.

Files: `tools/signature-lint.f`, `tools/signature-lint-core.f`, and its existing
test/fixture owner. Dependency:
`habu-load-lint-sources-a553610f`. Ownership: the `SIGNATURE-LINT` consumer and
standalone CLI boundary only; `LINT-SOURCE` remains the sole source owner.
