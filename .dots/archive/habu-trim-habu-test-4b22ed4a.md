---
title: Trim Habu test audit rows g4c
status: closed
priority: 2
issue-type: task
created-at: "2026-10-23T17:12:47.578569+03:00"
closed-at: "2026-10-23T17:33:29.535086+03:00"
close-reason: "Rejected all 14 rows by measurement: x86-64 kernel rows save <=0.7 s each; x86-64-kernel-definition sharing (~1.7 s) needs a case-selecting boot harness and changes the peer's image list; stdlib-process-fixtures' cost is the lint loader the row keeps; in-process internal-word-gate JSON checks pollute forked subjects (3 cases failed); MERGE rows each under 0.45 s."
---

Problem: the Habu test audit (group g4) found gate suites whose cases repeat setup, duplicate other rows' coverage or fork more processes than their boundaries need.
Acceptance: each MERGE/SLOW/CUT row below is applied after verifying its coverage claim, or rejected with a reason recorded at closure (e.g. measured saving too small, or batching would change the path under test); every distinct assertion and boundary a row names is kept; registry entries (test/gate-*.f) match the moved/removed suites.
Verification: each changed suite passes on its own with wall time before and after; the full registry (bin/hb --load test/run.f) passes after the batch lands.

Rows:
| x86-64-kernel-ffi | **SLOW** | **10 images.** Preserve shifted-stack alignment, argument folding, floating/integer results, varargs and distinct protected argument/extent/sret failures. They share one kernel build. [kernel-ffi.f:213,250,292–363](test/x86-64-kernel-ffi.f:213). |

| x86-64-kernel-pure | **SLOW** | **6 images.** Positive and negative images both regenerate the entire integer/float corpus although the negative exits at its first check. Keep the full positive corpus, only the existing first failing negative case, and separate fatal-store launches in the shared executable. [kernel-pure.f:288–327](test/x86-64-kernel-pure.f:288). |

| x86-64-kernel-prof | **SLOW** | **13 kernel/profiler images.** Preserve asynchronous sampling, copied-name identity, timer normalization and reset-race checks in fresh launches of one image. The emitted four-second delay is peer runtime, not producer padding. [kernel-prof.f:422,713,817,889–972](test/x86-64-kernel-prof.f:422). |

| x86-64-boot-signal | **SLOW** | **3 complete-kernel images** for armed signal delivery with displaced RBP and idle absorption. Keep those real handler scenarios, sharing the build. Raw ucontext mutation in peer-routines is a different contract. [boot-signal.f:112–138](test/x86-64-boot-signal.f:112); [peer-routines.f:516](test/x86-64-peer-routines.f:516). |

| x86-64-kernel-task | **SLOW** | **4 images.** Pthread completion, caller VM preservation and descriptor/HALT-REQ behavior require fresh runtime state; one selected-case executable preserves it without four kernel builds. [kernel-task.f:140–188](test/x86-64-kernel-task.f:140). |

| x86-64-kernel-callback | **SLOW** | **4 images.** Preserve nested libc qsort results and callback-owner release, plus range/unbound/no-frame fatal cases. Share the kernel, retain separate fatal launches. [kernel-callback.f:59–118](test/x86-64-kernel-callback.f:59). |

| x86-64-kernel-crash | **SLOW** | **12 images** already put faults in forked children and inspect exact dumps, status and EOF. One parent executable can contain the existing selected child cases; keep every faulting child, remove repeated kernel builds. [kernel-crash.f:268–325](test/x86-64-kernel-crash.f:268). |

| x86-64-kernel-definition | **SLOW** | **32 images** use the same constructor. Preserve publication/lookup, long-name allocation, body-buffer/pass2 bounds and abort/close restoration in selected fresh launches. [kernel-definition.f:205,288–372,447–472](test/x86-64-kernel-definition.f:205). |

| stdlib-process-fixtures | **SLOW** | Remove the **80 identical `/usr/bin/true` captures**: they assert only success/empty output, not descriptor cleanup. Also remove the lint loader’s timeout/label self-test and duplicate full command scenarios through its forwarding API. Preserve genuine standalone-load, startup, PTY and process-boundary cases. [process-test.f:429–433](src/lib/process-test.f:429); [lint-cli-standalone-load.f:185–236](test/lint-cli-standalone-load.f:185); [process-command.f:546–593](src/lib/process-command.f:546). |

| internal-word-gate | **SLOW** | Two JSON-diagnostic cases reload the whole CHECK tool. Run those existing cases through one loaded CHECK core with reset/run, as the diagnostic suite already does. Also remove per-cell “qualified missing name, then the same missing name followed by store” repetition: both die at lookup before the store. [internal-word-gate.f:111–123,734–759,1418–1429](test/internal-word-gate.f:111); [diag-position-test.f:146–157](test/diag-position-test.f:146). |

| package-seal | **MERGE** | Into **checker-surface**. Same sealed product and disposable-source boundary. Preserve qualified publication refusal and the all-baked-packages census; checker-surface’s single package reopen assertion does not fully cover them. [package-seal.f:81–89,113–132](test/package-seal.f:81). |

| typed-storage-structural | **MERGE** | Into **typed-storage**. Same definers, allocator and evaluator setup; disjoint fixture package/names. Retain structural disagreement and role/pointer/linear/subcell admissibility, which exercise separate checker branches. [typed-storage-structural-test.f:31–35,70–172](test/typed-storage-structural-test.f:31). |

| storage-binding | **MERGE** | Into **typed-storage**, **last**. Preserve same-tail private/public accessor ownership and capture of the private quotation cell rather than the public numeric cell. Last position matters because capture preparation sweeps checker state. [storage-binding-e2e.f:23–58](test/storage-binding-e2e.f:23); [checker.f:29956–29964](src/core/checker.f:29956). |

| c2-mut-records | **MERGE** | Into **c2-mut-view**. Same subject/refusal setup and disjoint packages. Preserve losing/duplicating a view after generated `UNMAKE` and dropping owned records through sums—direct view movement does not cover generated record operations. [c2-mut-records.f:35–56,73–84](test/c2-mut-records.f:35). |
