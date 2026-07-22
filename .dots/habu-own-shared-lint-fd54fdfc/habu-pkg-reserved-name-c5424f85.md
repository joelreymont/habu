---
title: Package reserved-name lint command
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T17:32:16.813553+02:00"
---

Files: tools/reserved-name-lint.f only. Add package RESERVED-NAME-LINT-CLI, keep helpers private, rename RESERVED-NAME-LINT-ARGV-FILE to ARGV-FILE and RESERVED-NAME-LINT to RUN, and invoke RUN inside the package. Do not change the still-global core API. Preserve arguments, JSON selection, labels, and exit behavior. Acceptance: this file creates no global definition or storage and leaves no alias. Verify: bin/hb --load tools/reserved-name-lint-test.f, clean and failing command fixtures, typed-local-diff-lint, package-diff focused mutation, host-lint, filemap-lint.
