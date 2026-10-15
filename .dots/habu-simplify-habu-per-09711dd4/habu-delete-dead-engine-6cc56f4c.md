---
title: Delete dead engine words and stale comments
status: open
priority: 2
issue-type: task
created-at: "2026-10-15T17:16:30.800206+03:00"
---

Problem: words with no caller: jit.f:143-152 FOLD-ENTRY and 105-116 EMIT-VTOP2C; src/habu/bundle-argv.f (never loaded; duplicates src/os/script-argv.f); src/arch/x86-64/rt.f:231-232 EMIT-DOT and 259-281 EMIT-ATOI; xref.f:148, 705; address-cells.f:92; habu2.f:13420; source-exec.f:19; hide.f:41; layout.f dead offsets 1223-1226, 1273, 1653, 2256; task-abi.f dead offsets; habu1.f:497, 505-511, 1093. Stale comments: habu2.f:85-87 and 11231-11233 describe the deleted inliner; main.f:26; prims.f:76 says 223 used (299 rows); bootstrap/cg/forth.fs:76 claims to mirror CF-REC 24 / cap 170, but layout.f:362-390 has 32 / 127. Also in the prefix (and the Gforth seed): src/core/roles.f:202-216 VRDEF-FOLD-C and VRDEF-STR=CI duplicate util.f CORE-FOLD-C and CORE-STR=CI (util.f loads first); util.f:125 RDP is unused (its AOT link cost is noted at aot-closure.f:787). Acceptance: each word deleted after rg shows no caller in src, tools, test, bootstrap or ../etch; comments corrected; engine output unchanged apart from the removed words. Files: the files named above, plus src/core/roles.f and src/core/util.f. Verify: rebuild bin/hb with tools/native-build.f (docs/bootstrap.md); bin/hb --load test/run.f; tools/two-generation-build.f convergence (engine output changes). Depends: carl-final-core chain landed on master (it edits these files); start from master after it lands. Ownership: listed files. Claim: unassigned.
