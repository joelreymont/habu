---
title: Delete the JIT
status: open
priority: 2
issue-type: task
created-at: "2026-10-29T20:55:09.916986+02:00"
---

Roadmap step 4, after T0 is the engine's compiler (step 3 = this dot's parent: bake T0 into the engine image, dispatch : to it, stop loading it from source in tests, rename to COMP per habu-name-the-build-abed309a). Open design question, Joel decides after a census: the startup files loaded before the checker exists are compiled by the JIT today; T0 compiles from the checker's notes and cannot compile them; on a cold start (hb-stdin, every build after B1's cold load) something must. Census first: which files precede the checker, what they contain, what the interpreter can run. Proof: cold start without the JIT, full suite, Gforth recovery check.

Census (read-only, scratch /tmp/jerry-t0-engine): ten files precede checker.f (util, cell, pointer-storage, engine-error, exec-vector, checker-fetch-abi, checker-owner-extension, checker-owner-abi, prims.f, does-clause; about 87 colon definitions, 67 in prims.f), then checker.f (31,324 lines, 3,101 definitions; CLAIM-COLD-SOURCE at :31281-85 publishes the owner), then the files up to check-hook.f (about 4,700 definitions before the hook). At an engine build the host's tier 1 compiles all of it (LOAD-CHECKED-PREFIX, tools/native-build-core.f:529-574). Only cold engines (hb-stage0, hb-stage, hb-stdin-mk, hb-stdin; tools/bootstrap.sh:505-529) compile them with the JIT (TIER-CELL starts 0, habu2.f:9679-82); every seeded bin/hb skips the cold prefix (SEEDED-RUNTIME?, habu2.f:1953-60) and tools/cold-load.f reloads under the retained owner. Intel has no JIT: its first hb is cross-built (bootstrap.sh:46). JIT-only code: habu2.f 2210-3191, 6509-6970, 11387-14300, TIER-COLON-DISPATCH 3383-3405, the LCOMPILE hand-off about 10089-10440; 37 EM-P2 words; jit.f 1,453 lines, regalloc.f 111. Pins: 77 files match EM-P2|set-tier 0|tier 0 (38 tests use `0 set-tier`); 438 of 895 test files never set a tier and so run on the JIT today; test/tier.f pins ARM at tier 0 and Intel at tier 1.

Ruling 2026-10-29 (Joel, interview): recovery = cross-build from a running hb, as Intel today; the Gforth seed and the cold-engine chain retire with the JIT. No interpreter-only prefix, no recovery-only JIT. Consequences for this dot: tools/bootstrap.sh's Gforth chain and docs/bootstrap.md "Recovery routes" describe recovery from a released hb by cross-build; CLAUDE.md's Gforth recovery check goes; the cold-start proof is a cross-built engine that starts and runs the suite.
