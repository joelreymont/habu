---
title: hb-build cannot build a source over 64 KiB
status: active
priority: 1
issue-type: task
created-at: "2026-08-22T22:47:07.135914+02:00"
---

Problem: tools/aot-lint-core.f:8,180 FILE-CAP $10000 and tools/signature-lint-core.f:9,269 SL-FILE-CAP read the whole source through READ-FILE (tools/lint/text.f:89-103), which dies 'lint: file exceeds buffer' past the cap; HBB-BUILD-BEGIN (tools/hb-build-lib.f:1020-1025) runs the aot lint on every non-repl build, so a user program over 65,536 bytes fails the production build tool with a buffer message (maki/cad.f is 84,874 bytes). Behind it: lib/source.f:8 SOURCE-CAP $20000 bounds COMMENT-EXPORTS output (hb-build-lib.f:705,709) and tools/build-fixpoint.f:23 BF-SOURCE-CAP 262144 bounds HBB-READ-COMMENTED-SOURCE. Acceptance: both lints take LINT-SLAB:LOAD (tools/lint/text.f:112-167) like the six lints that already do; SOURCE-BUF/BF-SOURCE-BUF sized from FILE-SIZE; hb-build of a 100 KiB program succeeds (test). Files: tools/aot-lint-core.f, signature-lint-core.f, lib/source.f, tools/build-fixpoint.f, hb-build-lib.f. Verify: the hb-build test. Depends: none. Ownership: build tool. Claim: agent=build-64k workspace=.jj-ws/habu-hb-build-cannot-d09df17e
