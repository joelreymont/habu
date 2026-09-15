---
title: Compile the prefix to dense code
status: open
priority: 2
issue-type: task
created-at: "2026-09-15T18:52:59.716456+03:00"
---

Problem: the baked prefix is tier-0 code: 2dup is 13 instructions, STR= (a six-line loop) 41, about 140 B per word over 15,683 words, because the optimizing tier costs about 60 ms per word (213 s for the core prefix) and is not run over the prefix. Acceptance: measured decision between (a) a tier-1 fast enough to compile the whole prefix at build (profile the chain with prof-on on a window build, fix the hot phases) and (b) a register-cached tier 0 (TOS in a register, pair folding) for the prefix; then the prefix compiled by the chosen tier, code bytes per word and the tier-1 build time reported before/after; test/run.f green. Files: src/habu/habu2.f (tier-0 emitters), src/compiler/native/*.f, tools/native-build.f. Verify: census of code bytes per word on the shipped engine; time of native-build.f. Depends: guard removal dot habu-replace-per-transfer-8523fb98. Ownership: compiler. Claim: unassigned.
