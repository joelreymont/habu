---
title: Package AOT lint command
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T17:32:16.602480+02:00"
---

Files: tools/aot-lint.f only. Add package AOT-LINT-CLI, keep helpers private, rename AOT-LINT-ARGV-FILE to ARGV-FILE and AOT-LINT to RUN, and invoke RUN inside the package. Do not change the still-global AOT lint core API. Preserve argument parsing, JSON selection, labels, and exit behavior. Acceptance: this file creates no global definition or storage and leaves no alias. Verify: bin/hb --load tools/aot-lint-test.f, clean and failing command fixtures, typed-local-diff-lint, package-diff focused mutation, host-lint, filemap-lint.
