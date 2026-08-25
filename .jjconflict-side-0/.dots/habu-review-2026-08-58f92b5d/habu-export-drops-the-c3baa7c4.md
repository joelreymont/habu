---
title: EXPORT drops the internal and kind bits from the alias record
status: open
priority: 2
issue-type: task
created-at: "2026-08-22T22:47:07.233392+02:00"
---

Problem: habu2.f:6698-6703 copies IMM, WIDE and MIN-IN from the LFIND flags but never DNAME-INT (bit 4, folded at habu1.f:3863) nor the DKIND pair (bits 50-51): an alias of an engine-internal word is dispatchable at interpret level (relying on CHECKER-EXPORT refusing unsigned sources), and an exported constant/create alias loses its kind stamp so src/compiler/native/dict.f:112-113 compiles a call instead of folding. Acceptance: the bits copied; a test exports an internal word (refused at interpret) and a constant (chain folds it). Files: src/habu/habu2.f, test/. Verify: the tests; native-chain suite. Depends: none. Ownership: export. Claim: unassigned.
