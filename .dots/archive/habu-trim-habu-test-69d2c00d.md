---
title: Trim Habu test audit rows g3b
status: closed
priority: 2
issue-type: task
created-at: "2026-10-23T17:12:47.545467+03:00"
closed-at: "2026-10-23T18:00:31.330042+03:00"
close-reason: "Applied: native-qualified-trusted got T-RESET/T-REPORT (a failing assertion used to exit 0) and joined generated-constructor-aot's row (~0.6-1.0 s). Rejected, each under ~1 s: integer-literals 0.59 s; outer-number <0.92; native-match 0.34; wide-mem-aot (x64-only saving); raw-storage-load-seal; base-pointer-arith-refusals; object-record-codec; object-cache-store; object-source-index; proc-pty-io 0.93 (spent waiting on children); proc-pty-tty 0.59; task-signal-altstack; task-entry; utf16-units; fd-io sleep ~0.27; content-length ~0.05."
---

Problem: the Habu test audit (group g3) found gate suites whose cases repeat setup, duplicate other rows' coverage or fork more processes than their boundaries need.
Acceptance: each MERGE/SLOW/CUT row below is applied after verifying its coverage claim, or rejected with a reason recorded at closure (e.g. measured saving too small, or batching would change the path under test); every distinct assertion and boundary a row names is kept; registry entries (test/gate-*.f) match the moved/removed suites.
Verification: each changed suite passes on its own with wall time before and after; the full registry (bin/hb --load test/run.f) passes after the batch lands.

Rows:
| compiler-native-qualified-trusted | **MERGE** | Into **compiler-native-generated-constructor-aot**. Retain qualified effects, recursion and failed-definition retry. **Current assertions do not enforce failure:** the file prints success without `T-REPORT`. Report them before constructor tests reset counters. [Assertions/end:49](test/compiler/native-qualified-trusted.f:49), [report semantics:142](src/lib/test/assert.f:142), [target reset:158](test/compiler/native-generated-constructor.f:158). |

| compiler-integer-literals | **SLOW** | Twelve overflow spellings × four contexts create **48 fork captures**. Children merely catch evaluation and convert its throw into an exit code. Use the existing in-process capture and scope rollback, preserving all contexts and diagnostics. [Repeated cases:91](test/compiler/integer-literals.f:91), [existing rollback/capture:353](test/gate-common-lib.f:353), [child catch:404](test/gate-common-lib.f:404). |

| outer-number | **SLOW** | Every spelling forks for a caught return-code comparison, including accepted spellings. Reuse the same in-process capture/rollback mechanism. Keep IEEE vectors and formatted-source replay: the outer parser is not the integer reader. [Per-spelling fork:71](test/outer-number.f:71), [independent vectors:18](test/outer-number.f:18). |

| compiler-native-match | **SLOW** | Both rows explicitly switch to tier 1 for emission probes and launch the same hostile-tag child. Run those checks once in **compiler-native-match-aot**. Retain default-tier behavior on ARM; on x64 the default row adds no second compiler tier. [Forced tier:684](test/compiler/native-match.f:684), [duplicated child:911](test/compiler/native-match.f:911). |

| compiler-native-wide-mem-aot | **SLOW** | Retain both compiler paths on ARM; execute the file only once on x64. The twin adds tier selection, while x64 already admits only tier 1. [Twin registry setup:1027](test/gate-stdlib-cases.f:1027), [setter:3983](src/habu/kernel-x64.f:3983). |

| raw-storage-load-seal | **MERGE** | Into **raw-cell-pointer-refusals**. Preserve native-load-created definers and nominal storage cases; both use the same loaded checker/harness and finish synchronously in separate packages. [Distinct definers:51](test/raw-storage-load-seal-test.f:51), [runner:144](test/raw-storage-load-seal-test.f:144). |

| base-pointer-arith-refusals | **MERGE** | Into **raw-cell-pointer-refusals**. Share checker and buffered-diagnostic setup; retain deep DATA chains, input-only quantifier escapes and stepped-pointer provenance. Raw dictionary-cell cases do not replace those assertions. [Shared-style harness:50](test/compiler/base-pointer-arith-refusals.f:50), [distinct chains:270](test/compiler/base-pointer-arith-refusals.f:270), [provenance:366](test/compiler/base-pointer-arith-refusals.f:366). |

| object-record-codec | **MERGE** | Into **object-source-resolver**. Resolver already loads the codec. Preserve malformed external records and capacity failures; successful resolver/build round trips do not cover them. [Refusals:267](src/lib/object-test.f:267), [target dependencies:4](src/lib/object-resolve-test.f:4). |

| object-cache-store | **MERGE** | Into **object-source-resolver**. Keep the returned-key alias corruption case: ordinary cache hits miss it. Separate roots and completed cleanup permit shared-process execution. The “atomic overwrite” case only proves successful repeated storage, not interruption atomicity. [Alias case:73](src/lib/object-cache-test.f:73), [overwrite:58](src/lib/object-cache-test.f:58), [cleanup:91](src/lib/object-cache-test.f:91). |

| object-source-index | **MERGE** | Into **object-source-resolver**. Preserve malformed on-disk index rejection; a valid index pointing at the wrong object is a different case. Resolver already loads the dependencies, and this file cleans its private fixture root. [Malformed index:71](src/lib/object-index-test.f:71), [cleanup:83](src/lib/object-index-test.f:83). |

| proc-pty-io-supervisor-smoke | **MERGE** | Into **proc-pty-tty-smoke**, sharing one outer engine/AIO lifecycle. Retain child starts that test retirement, registry-slot recovery and linear handles. Run TTY cases first or reset argv before them: SIGNAL-OUTPUT leaves shell arguments staged. [Lifecycle cases:54](test/process-pty-io-smoke.f:54), [argv mutation:133](test/process-pty-io-smoke.f:133), [AIO setup:160](test/process-pty-io-smoke.f:160). |

| proc-pty-tty-smoke | **SLOW** | Five fresh TTY engines separately test continuing recovery/prompt/layout/stack exchanges and final tree teardown. Keep one TTY handle for those exchanges, then teardown last. Retain the separate pipe child: its intentional batch exit is the boundary. [Recovery starts:182](test/process-pty-tty-smoke.f:182), [stack case:237](test/process-pty-tty-smoke.f:237), [tree teardown:296](test/process-pty-tty-smoke.f:296), [pipe case:165](test/process-pty-tty-smoke.f:165). |

| task-signal-altstack | **MERGE** | Into **tasking-threads**. Preserve OS-reported stack size/enabled state, profiler pointer preservation and worker-versus-main pointer distinction. One worker is necessary; a separate outer engine is not. Run before final lifecycle sweeps. [Assertions:32](test/task-signal-altstack.f:32), [activation:64](test/task-signal-altstack.f:64). |

| task-entry | **MERGE** | Into **tasking-threads**, last. Preserve three activation/kill cycles, unchanged `cp@`, raw-entry refusal and the two lifecycle preparations. Running last avoids invalidating callback rows before other task cases use them. [Entire boundary:13](test/task-entry.f:13). |

| utf16-units | **MERGE** | Into **utf8-scalar**’s process. UTF16 already uses UTF8:NEXT; neither needs process isolation. Preserve inside-surrogate-pair rounding and malformed-prefix byte/unit behavior—LSP’s ordinary emoji-column assertion does not cover those cases. [Dependency:26](src/lib/utf16.f:26), [distinct assertions:78](src/lib/utf16-test.f:78). |

| fd-io | **SLOW** | BOUNDED-TIMEOUT forks a child that sleeps **300 ms**, although the write deadline is **30 ms**, then waits for that child. Keep the reader descriptor open in the parent without reading; the bounded nonblocking writer still times out. Preserve signal-induced short-write cases. [Padding:173](src/lib/fd-io-test.f:173), [writer’s nonblocking/deadline path:178](src/lib/fd-io.f:178). |

| content-length | **SLOW** | PIECE sleeps 50 ms after **each of five fragments**, including the final byte; the parent then waits for EOF/reaping. Remove the final sleep: it creates no fragmentation boundary. Keep truncation, spent-reader, capacity and actual fragmented-I/O assertions. [Delay constant:58](src/lib/content-length-test.f:58), [five writes/final wait:256](src/lib/content-length-test.f:256). |
