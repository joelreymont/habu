---
title: Fork only to exec
status: open
priority: 1
issue-type: task
created-at: "2026-10-28T19:04:35.597440+02:00"
---

Joel 2026-10-28 (interview): Habu forks only to exec. Fork-and-continue in a multithreaded process (tasks are OS threads) is outside what POSIX supports; src/lib/fork-child.f's reset registry, FORK-HELD and the ACROSS-FORK hooks are a home-made pthread_atfork that grows with every new process state. Callers that fork and keep running Habu: src/lib/test/subject.f (SUBJECT:RUN, 109 files), test/gate-pool.f GT-POOL-FORK, test/gate-common-lib.f GE-EVAL-FORK-SPAWN, test/prop-test-core.f SHARD-FORK and SELFTEST-SWEEP-RED, tools/perf/boot-census-fixture.f, test/live-verify-source-e2e.f WITH-WRITER, src/lib/process-pty-io.f IO-FORK-TARGET, the SPAWN-REAPER in src/lib/process-fork.f. Each becomes a spawn of a fresh hb (or exec of its target); then delete fork-child.f, FORK-HELD, the ACROSS-FORK hooks, PROC-FORK:RAW/CHECKED as fork-and-continue words, and CHILD-MAIN. tim's lanes tim-forktask 6c39a273 and tim-forksig 87df85ba are rejected by this ruling. Children are the steps. Claim: unassigned.

Landing: this feature spans lanes 1a-1d, SNIPPET, the reaper and the PROCESS merge and changes engine primitives and process lifecycle, so it lands whole after one Fable feature review of the combined diff. Each reviewed lane merges into the integration workspace .jj-ws.noindex/carl-forkexec (stacked in order), not master. Lane 1a (spawn-fds) done: zqylsmst 1abc4839 in carl-spawn, under lane review.

Joel 2026-10-29 (interview) approved the PROCESS API (design .jj-ws.noindex/carl-dots/.out/carl-file/process-design.md, cut by carl): one package PROCESS replaces the ~198 PROC-* words, CMD, PCAP, PROCESS-PTY, staging and fork. Words: NEW ARG ENV HERMETIC CWD FD CLOSED STDIN CANCEL TICK on a linear PROCESS:request consumed by SPAWN ( -- pid ) or RUN ( request out-buf err-buf ms -- outcome ); WAIT WAIT-WITHIN KILL KILL-TREE CATCHES?; SUMTYPE PROCESS:outcome (exited/signaled/timeout); RC (throws on timeout) DEADLINE-RC (124); FIND RESOLVE; TIME:DEADLINE TIME:LEFT-MS. Cut from the draft: ENV-ROW, EXIT-RC, public CPU-NS. Decided by carl: env inherited unless HERMETIC; request consumed; PROCESS:outcome; lanes land incrementally with old words as bridges, the last lane deletes them. Lane map: 1b becomes PROCESS lane 1 (library core: spawn.f reshaped into PROCESS; qtuqxnnn amended to PROCESS words); lanes 2 harness, 3 pool reaper + fork retirement, 4 tools, 5 test suites, 6 PTY, 7 deletion (absorbs 1c's deletion and 1d).

macOS evidence (kevin's SECRET lane, 2026-10-31; files /tmp/kevin-secretspawn/only-{FORK-CURL,TASK-LIVE,CURL-FIRST}.{f,txt}): after the parent used Security (an absent keychain read), a fork-without-exec child crashed in curl_global_init (rc 139); with Security moved into a fork-without-exec child, the child died once the parent had a live task or had run curl_global_init: objc "+[NSString initialize] may have been in progress in another thread when fork() was called ... Crashing instead". System frameworks cannot run in a child of fork without exec on macOS once the parent has threads or has used them. SECRET keeps its Security calls in-process (carl's ruling to kevin, withdrawing the forked-child design); the fork-and-continue callers' conversion here removes the failing pattern.

Acceptance cases folded in by the 2026-11-04 census (the closed dots' full text is in the history before the census commit):
- habu-show-a-refused-f0b3b80b (closed): a failed REFUSED? showing the child's output must hold in SNIPPET after the subject library's spawn conversion
- habu-give-gate-pool-c515de30 (closed): gate pool's orphan reaping must hold once it forks only to exec; readiness via process-fork.f (dot text)
Waiting case (from the test prune, 'Drop tests the suite no longer keeps', 2026-11-06): src/lib/process-fork.f FORK-CALL (:45-49), a defer only a dropped test refilled, has no other setter; it goes with fork without exec.
Done in part (2026-11-09, "Drop the test framework's waiting tests"): test/gate-pool.f's forked slots had no caller and went, GT-POOL-START-FORK, GT-POOL-FORK, its worker-side GT-POOL-ARM-REAPER and tools/why-threw.f with them. The pool's one remaining fork-and-continue is the spawned slots' PROC-FORK:SPAWN-REAPER.

Consumers: Zen loads neither src/lib/process-fork.f nor process-pty-io.f, and its two dots that named this one are closed.

Waiting case (jerry, from the memory-fault throw): on macOS a child of fork without exec has no alternate signal stack (sigaltstack answers SS_DISABLE), so a machine-stack overflow in it cannot reach the crash handler and dies unnamed. It goes with fork without exec.

Waiting case (the heap allocator, src/lib/heap.f): on Linux fork is the raw system call (src/habu/habu1.f, NR-FORK SYS,) where macOS calls libc's fork by name, so libc's allocator locks are not reset in a Linux child. A child that runs Habu code and allocates from HEAP:ALLOCATOR can hang when another task was inside malloc at the fork; a child that only execs is safe. It goes with fork without exec.

Acceptance case 2026-11-12 (jerry; from habu-print-the-one-ea184772's census): the fork child's own-region remap exits 78 with a line off the shape `<class> habu: <text>[ <16 hex>] lr <16 hex>`: src/habu/habu1.f FORK :2987 (Linux, 'hb: fork: cannot map the child's own region') and :3019 (macOS, '...child's region alias'). Accepted when fork only to exec removes the step or its line takes the shape.
