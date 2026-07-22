---
title: Package AOT lint command
status: closed
priority: 1
issue-type: task
created-at: "\"2026-07-22T17:32:16.602480+02:00\""
closed-at: "2026-07-22T17:58:42.979961+02:00"
close-reason: Landed and reviewed in ebca0ab1d3ec; combined closure 58a89f68 passed focused ownership gates.
---

Files: tools/aot-lint.f only. Add package AOT-LINT-CLI, keep helpers private, rename AOT-LINT-ARGV-FILE to ARGV-FILE and AOT-LINT to RUN, and invoke RUN inside the package. Do not change the still-global AOT lint core API. Preserve argument parsing, JSON selection, labels, and exit behavior. Acceptance: this file creates no global definition or storage and leaves no alias. Verify: bin/hb --load tools/aot-lint-test.f, clean and failing command fixtures, typed-local-diff-lint, package-diff focused mutation, host-lint, filemap-lint.

Claim: agent=aot_cli workspace=.jj-ws/habu-pkg-aot-lint-cd6f466a.
