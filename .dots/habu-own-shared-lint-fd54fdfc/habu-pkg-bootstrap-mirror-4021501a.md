---
title: Package bootstrap mirror lint tests
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T17:32:17.132981+02:00"
---

Files: tools/bootstrap-mirror-lint-test.f only. Add package BOOTSTRAP-MIRROR-LINT-TEST, make every BMT state cell, buffer, and helper private, use short tails, and execute the three existing cases inside the package. Continue calling the still-global bootstrap mirror lint core API in this leaf. Acceptance: the source tree clean case, dirty overlay rejection, and clean overlay acceptance remain active; cleanup registration and exact labels are preserved; no global BMT-* names or aliases. Verify: bin/hb --load tools/bootstrap-mirror-lint-test.f, typed-local-diff-lint, package-diff focused mutation, host-lint, filemap-lint.
