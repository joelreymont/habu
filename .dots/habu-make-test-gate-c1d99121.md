---
title: Make test/gate-aot-image.f require its own dependencies
status: open
priority: 2
issue-type: task
created-at: "2026-09-21T17:43:28.139506+03:00"
---

Problem: test/gate-aot-image.f:3 requires only src/arch/arm64/asm.f while its body uses FS-PATH-CAP (lib/fs.f), the GB-* readers (test/gate-build-common.f) and E-BUILD-SOURCE; its header says 'Load after gate-build-common.f and aot-call-report-lib.f', so bin/hb --load test/gate-aot-image.f fails at load with E-UNDEFINED: FS-PATH-CAP (measured on engine 84e0f2a8) and the file loads only through test/gate-aot-positive-lib.f and test/compiler/aot-xt-cells.f, which require lib/fs-mutate.f first. docs/forth-card.md section 7: every file requires its own dependencies. Acceptance: the file requires lib/fs.f, test/gate-build-common.f, test/aot-call-report-lib.f and whatever else its words resolve through, in the card's order, and drops the 'Load after' sentence; it loads standalone with exit 0; both requirers still pass; sweep test/ for other 'Load after' headers with the same gap and fix each in the same commit. Files: test/gate-aot-image.f plus the swept files. Verify: bin/hb --load test/gate-aot-image.f < /dev/null; bin/hb --load test/gate-aot-positive-lib.f; bin/hb --load test/compiler/aot-xt-cells.f; rg -n 'Load after' test. Depends: none. Ownership: test/gate-aot-image.f and the swept files. Claim: unassigned.
