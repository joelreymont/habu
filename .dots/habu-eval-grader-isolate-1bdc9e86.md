---
title: "Eval grader: isolate candidate checks (namespace per candidate)"
status: open
priority: 2
issue-type: task
created-at: "2026-06-28T12:01:01.569965+02:00"
---

BLOCKING the maki gate (maki/README command, the owning gate for maki changes). RCA: maki/eval.f CHECK-PASSES? ( = CHECK! -1 = ) checks each candidate IN THE HOST DICTIONARY with NO isolation. Two failure modes, both reproduced with the real self-built bin/hb:
1) ACCUMULATION: candidate words persist across CHECK! calls -> a 2nd candidate with the same word name = 'duplicate definition'. eval-repair.f + eval-fixture.f grade the SAME kernel named K across repair iterations -> every run duplicates. (Proven: CHECK-PASSES? on "KK (...)" twice -> 2nd is 'duplicate definition: kk'.)
2) CORE SHADOWING: candidate names collide with core words (case-insensitive): a,b,c,k are core ( in bare core -> duplicate; d,e free). eval-test.f candidates A/B/C and eval-repair/fixture kernel K collide on the very first check.
MECHANISM: defs land in BOTH ndict AND the checker usig registry (UEND/UTERM!, see CHECKER-USIGS-TRUNCATE-FROM checker.f:1050; HIDE-DEFS-FROM xref.f:157 truncates both). CHECK-RESET (checker.f:1928) resets analysis state but NOT the defs. A userland wrapper 'ndict@ UEND @ 2>R CHECK-PASSES? 2R> UEND ! UTERM! ndict!' gives rc=70 (fragile; restore-after also can't prevent the in-CHECK! define colliding with core). 
FIX (root): give CHECK! / CHECK-PASSES? a per-candidate isolated namespace (a fresh wordlist that shadows core during the check and is discarded after) OR an in-checker save/restore of (ndict + usig end) that brackets the define so candidate words never collide with core or prior candidates. Then candidates can be named anything (as real LLM kernels are). Affects src/core/checker.f (CHECK!/CHECK-RESET), maki/eval.f (CHECK-PASSES?). Verify: eval-test/repair/fixture grade green; CHECK-PASSES? on the same name twice both succeed; a candidate named 'k' or 'a' checks fine.
NOTE: likely a recent regression (the maki gate command in maki/README lists eval-fixture/eval-repair; either core a/b/c/k or these eval files changed). master native gate (test/run.f, 102.7s) is green and does NOT include this maki gate, so master looks green while the maki owning gate is red. Blocks FF-ing branch maki-autograd (SUB+SQUARE) to master.
