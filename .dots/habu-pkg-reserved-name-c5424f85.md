---
title: Package reserved-name lint command
status: closed
priority: 1
issue-type: task
created-at: "2026-07-22T17:32:16.813553+02:00"
closed-at: "2026-07-22T17:58:42.974801+02:00"
close-reason: Landed and reviewed in fdbf7d4a7bb3; combined closure 58a89f68 passed focused ownership gates.
---

Files: tools/reserved-name-lint.f only. Add package RESERVED-NAME-LINT-CLI, keep helpers private, rename RESERVED-NAME-LINT-ARGV-FILE to ARGV-FILE and RESERVED-NAME-LINT to RUN, and invoke RUN inside the package. Do not change the still-global core API. Preserve arguments, JSON selection, labels, and exit behavior. Acceptance: this file creates no global definition or storage and leaves no alias. Verify: bin/hb --load tools/reserved-name-lint-test.f, clean and failing command fixtures, typed-local-diff-lint, package-diff focused mutation, host-lint, filemap-lint.

Claim: agent=reserved_cli workspace=.jj-ws/habu-pkg-reserved-name-c5424f85.
