---
title: hb-build cannot build a source over 64 KiB
status: closed
priority: 1
issue-type: task
created-at: "2026-08-22T22:47:07.135914+02:00"
closed-at: "2026-08-23T14:20:11.688766+02:00"
close-reason: implemented, reviewed, merged, gates green: lib/source.f sealed as package SOURCE with four dead public words deleted and COMMENT-EXPORTS$ sizing its own destination; aot-lint and signature-lint read through LINT-SLAB; BF-SOURCE-ENSURE/BF-READ-SOURCE size the build scratch from FILE-SIZE and release the previous span; the diag-origin child's stdout goes to a file with its 120 s deadline kept (two commits, 4d2f7cc9 + 1d9efebf, landed via merge 6ecb02fe); a 132 KiB program builds and runs on the AOT, strict-signatures and --repl paths; the next cliff is PMAX at ~142 KiB, named and loud (habu-maker-program-buf-9d81f88b); eleven suites, maki, lint-libs, both diff lints, error-code-lint, schedule-lint green.
---

Problem: tools/aot-lint-core.f:8,180 FILE-CAP $10000 and tools/signature-lint-core.f:9,269 SL-FILE-CAP read the whole source through READ-FILE (tools/lint/text.f:89-103), which dies 'lint: file exceeds buffer' past the cap; HBB-BUILD-BEGIN (tools/hb-build-lib.f:1020-1025) runs the aot lint on every non-repl build, so a user program over 65,536 bytes fails the production build tool with a buffer message (maki/cad.f is 84,874 bytes). Behind it: lib/source.f:8 SOURCE-CAP $20000 bounds COMMENT-EXPORTS output (hb-build-lib.f:705,709) and tools/build-fixpoint.f:23 BF-SOURCE-CAP 262144 bounds HBB-READ-COMMENTED-SOURCE. Acceptance: both lints take LINT-SLAB:LOAD (tools/lint/text.f:112-167) like the six lints that already do; SOURCE-BUF/BF-SOURCE-BUF sized from FILE-SIZE; hb-build of a 100 KiB program succeeds (test). Files: tools/aot-lint-core.f, signature-lint-core.f, lib/source.f, tools/build-fixpoint.f, hb-build-lib.f. Verify: the hb-build test. Depends: none. Ownership: build tool. Claim: closed (landed on master via 6ecb02fe).
