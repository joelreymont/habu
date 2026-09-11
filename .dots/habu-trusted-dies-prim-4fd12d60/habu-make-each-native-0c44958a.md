---
title: Make each native suite select or assert its tier
status: open
priority: 2
issue-type: task
created-at: "2026-09-11T18:54:32.812963+03:00"
---

Problem: the nine native codegen suites (native-rstack, native-catch, native-again, native-emit, native-trap, native-match, native-do, native-loop, native-tail) assert optimizer-only properties but rely on test/gate-stdlib-lib.f's SUITE-AOT? to prepend test/compiler/aot-mode.f; run directly (bin/hb --load test/compiler/<suite>.f) they fail with wrong values and optimizer refusals that never fire at tier 0 (expected -8522 got 0, expected -8286 got 0), which cost two lanes a false regression hunt on 2026-09-11 (bisect proved the reds identical on c6387889 and 0c3099ea). Acceptance: each such suite selects tier 1 itself at its top (or asserts tier@ 1 = with a named diagnostic naming aot-mode.f) so a direct invocation is either correct or fails with one clear line; gate-stdlib-lib.f's prepend stays or goes, stated; test/run.f results unchanged. Files: test/compiler/native-*.f headers, test/compiler/aot-mode.f, test/gate-stdlib-lib.f. Verify: run one suite directly with and without aot-mode.f; test/run.f. Depends: none. Ownership: hazel (granted for these test headers). Claim: unassigned.
