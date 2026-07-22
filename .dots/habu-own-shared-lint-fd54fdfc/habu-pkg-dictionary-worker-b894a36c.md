---
title: Package dictionary worker entry
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-23T00:58:14.579824+02:00\""
---

Files: test/run-worker-dict.f only. Put the executable wrapper in package TEST-RUN, keep UNDER! and DICT private, call DICT before closing the package, and leave no public API because this file is the process entry. Continue calling the current global dictionary API in this prerequisite; the dictionary package leaf will change that private call after it lands. Preserve TRW-LOAD-DONE ordering, TR-UNDER-READY handling, GE-HB! propagation, and one dictionary run exactly. Acceptance: no TRWK-* definition or storage remains global; the resident dictionary worker and gate-runner dictionary slice remain byte-for-byte equivalent; no alias or package exception. Verify: resident dictionary worker, gate-runner dictionary slice, typed-local-diff-lint, package-diff mutation, host-lint, filemap-lint.

Claim: agent=dictionary_worker_pkg workspace=.jj-ws/habu-pkg-dictionary-worker-b894a36c.
