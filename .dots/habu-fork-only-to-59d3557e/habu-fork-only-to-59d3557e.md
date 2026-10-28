---
title: Fork only to exec
status: open
priority: 1
issue-type: task
created-at: "2026-10-28T19:04:35.597440+02:00"
---

Joel 2026-10-28 (interview): Habu forks only to exec. Fork-and-continue in a multithreaded process (tasks are OS threads) is outside what POSIX supports; src/lib/fork-child.f's reset registry, FORK-HELD and the ACROSS-FORK hooks are a home-made pthread_atfork that grows with every new process state. Callers that fork and keep running Habu: src/lib/test/subject.f (SUBJECT:RUN, 109 files), test/gate-pool.f GT-POOL-FORK, test/gate-common-lib.f GE-EVAL-FORK-SPAWN, test/prop-test-core.f SHARD-FORK and SELFTEST-SWEEP-RED, tools/perf/boot-census-fixture.f, test/live-verify-source-e2e.f WITH-WRITER, src/lib/process-pty-io.f IO-FORK-TARGET, the SPAWN-REAPER in src/lib/process-fork.f. Each becomes a spawn of a fresh hb (or exec of its target); then delete fork-child.f, FORK-HELD, the ACROSS-FORK hooks, PROC-FORK:RAW/CHECKED as fork-and-continue words, and CHILD-MAIN. tim's lanes tim-forktask 6c39a273 and tim-forksig 87df85ba are rejected by this ruling. Children are the steps. Claim: carl.

Landing: this feature spans lanes 1a-1d, SNIPPET, the reaper and the PROCESS merge and changes engine primitives and process lifecycle, so it lands whole after one Fable feature review of the combined diff. Each reviewed lane merges into the integration workspace .jj-ws.noindex/carl-forkexec (stacked in order), not master. Lane 1a (spawn-fds) done: zqylsmst 1abc4839 in carl-spawn, under lane review.
