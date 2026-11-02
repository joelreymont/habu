---
title: Read two images through one shared word
status: open
priority: 3
issue-type: task
created-at: "2026-11-02T15:30:42.385588+02:00"
---

Problem (Opus review of the sign lane rlxpkzpu 'Sign every macOS build with the team identity', finding L1, 2026-11-02): tools/build-fixpoint.f BF-READ-IMAGES (~:1487-1499) is a copy of tools/chain-run.f READ-FILES (~:65), the reader of two files that CODESIGN:SAME-IMAGE? compares, because build-fixpoint's preamble cannot load CHAIN-RUN. Two copies of one rule drift. Acceptance: one word reads two image files for CODESIGN:SAME-IMAGE? and both callers use it, at the owner both may load (src/lib/codesign.f beside SAME-IMAGE?, or a file build-fixpoint's preamble already loads); the copy goes. If the shared word is public (a new word in a package's public section), ask Joel through the interview skill before the lane, with its name, effect and both callers. Files: tools/build-fixpoint.f, tools/chain-run.f, the owner file. Verify: hb --load tools/build-fixpoint-test.f and the chain-run tests, rc 0. Depends: the sign lane landed. Ownership: carl. Claim: unassigned.
