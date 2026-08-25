---
title: Package dictionary worker entry
status: closed
priority: 1
issue-type: task
created-at: "2026-07-23T00:58:14.579824+02:00"
closed-at: "2026-07-23T02:05:41.104579+02:00"
close-reason: Landed and remotely verified at ad47ef725011; independent destruction review and exact owning/master gates green.
---

Files: test/run-worker-dict.f only. Put the executable wrapper in package TEST-RUN, keep UNDER! and DICT private, capture the private DICT execution token while TEST-RUN is open, close the package, then execute the carried token so GD-MAIN and its generated package fixtures run from top-level scope. Leave no public API or global entry storage. Continue calling the current global dictionary API in this prerequisite; the dictionary package leaf will change that private call after it lands. Preserve TRW-LOAD-DONE ordering, TR-UNDER-READY handling, GE-HB! propagation, and one dictionary run exactly. Acceptance: no TRWK-* definition or storage remains global; TEST-RUN is closed before GD-MAIN executes; the resident dictionary worker and gate-runner dictionary slice remain byte-for-byte equivalent; no alias or package exception. Verify: resident dictionary worker, gate-runner dictionary slice, a negative fixture proving execution inside the open package fails with the nested-package diagnostic, typed-local-diff-lint, package-diff mutation, host-lint, filemap-lint.

Claim: agent=dictionary_worker_pkg workspace=.jj-ws/habu-pkg-dictionary-worker-b894a36c.
