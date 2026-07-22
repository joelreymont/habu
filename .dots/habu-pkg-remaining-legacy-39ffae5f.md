---
title: Package remaining legacy lint test libraries
status: active
priority: 1
issue-type: task
created-at: "2026-07-22T16:04:52.514798+02:00"
---

Files: tools/signature-lint-test-lib.f, tools/signature-lint-test.f, tools/reserved-name-lint-test-lib.f, tools/reserved-name-lint-test.f, tools/duplicate-definition-lint-test-lib.f, and tools/duplicate-definition-lint-test.f only. Put the three libraries in packages SIGNATURE-LINT-TEST, RESERVED-NAME-LINT-TEST, and DUPLICATE-DEFINITION-LINT-TEST. Make all state and helpers private with short tails; publish RUN ( -- ) as each package's sole entry; change each thin entry file to call the qualified RUN. Continue calling the current global lint-core APIs in this leaf. Acceptance: no SLT-, RNLT-, or DDLT- definition or storage remains global; every existing clean, rejection, JSON, label, case-folding, loader, type-family, control, numeric, and duplicate-definition assertion remains active; entry loads and owning standard-library slices are green; no compatibility alias. Verify: bin/hb --load tools/signature-lint-test.f, tools/reserved-name-lint-test.f, and tools/duplicate-definition-lint-test.f; typed-local-diff-lint; package-diff mutation; host-lint; filemap-lint.

Claim: agent=claude workspace=.jj-ws/habu-pkg-siblings.
