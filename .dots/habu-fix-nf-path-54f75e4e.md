---
title: Fix nf-path-test after the TMPDIR fold
status: active
priority: 2
issue-type: task
created-at: "2026-10-28T20:42:25.987199+02:00"
---

Problem: bin/hb --load test/nf-path-test.f fails F1-F4 on master 031363d6 (both Gforth builds PASS, then 'the long-root/quoted-root artifact and complete output path exist' assert false). Cause: affdbf58 'Fold HB_TMP into TMPDIR' made test/nf.fs NF-SUFFIX always append '-<pid>' (NF-BUILD at test/nf.fs:44-55), so the fixture writes <root>/nf-bin-<pid> and <root>/nf-out-<pid>; test/nf-path-test.f ROOT-FILE?/ROOT-OUT-OK? (lines 106-117, CONCURRENT-CASE 119-128) still look for <root>/nf-bin and <root>/nf-out. The rename worker saw it fail before 26ac40c2 too. source-unit-range-e2e is not broken: it runs on stdin through test/source-unit-range-gate.f, which passes (test: ok). Acceptance: CONCURRENT-CASE checks the exact names nf.fs writes for each build (the child's pid suffix, which /usr/bin/env keeps by exec); the forged negative control keeps proving that expected-looking artifacts do not turn a nonzero child green; no glob that a stale artifact could satisfy. Files: test/nf-path-test.f (test/nf.fs only if the suffix contract itself is wrong). Verify: bin/hb --load test/nf-path-test.f </dev/null (needs gforth; docs/gate.md:216 runs it on its own). Depends: none. Ownership: test/nf-path-test.f. Claim: carl, workspace .jj-ws.noindex/carl-nfpath (change "Match pid-suffixed nf artifacts in nf-path-test"): the pool does not report the gforth child pid, so each fresh root must hold exactly one nf-bin-<digits> and one nf-out-<digits> with the same suffix and nf-out "ok"; the forged control writes suffixed names. First run went TIMEOUT-UNDER-LOAD at the 120 s deadline (21 s CPU, load ~160); rerun queued for lower load.
