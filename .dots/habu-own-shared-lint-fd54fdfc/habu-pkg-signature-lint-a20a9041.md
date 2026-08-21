---
title: Package signature lint command
status: closed
priority: 1
issue-type: task
created-at: "2026-07-22T17:32:16.377803+02:00"
closed-at: "2026-07-22T17:58:42.960274+02:00"
close-reason: Landed and reviewed in e10dc18699b1; combined closure 58a89f68 passed focused ownership gates.
---

Files: tools/signature-lint.f only. Add package SIGNATURE-LINT-CLI, keep all state and helpers private, rename SIGNATURE-LINT-ARGV-FILE to ARGV-FILE and SIGNATURE-LINT to RUN, and invoke RUN inside the package. Do not change the still-global core API in this leaf. Preserve argument parsing, JSON selection, per-file labels, and exit behavior. Acceptance: no definition or storage created by this file is global; the command still accepts the same arguments and diagnostics; no alias remains. Verify: bin/hb --load tools/signature-lint-test.f, one clean and one failing command fixture, typed-local-diff-lint, package-diff focused mutation, host-lint, filemap-lint.

Claim: agent=signature_cli workspace=.jj-ws/habu-pkg-signature-lint-a20a9041.
