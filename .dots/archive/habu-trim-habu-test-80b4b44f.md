---
title: Trim Habu test audit rows g4b
status: closed
priority: 2
issue-type: task
created-at: "2026-10-23T17:12:47.567603+03:00"
closed-at: "2026-10-23T18:00:31.660139+03:00"
close-reason: "Applied: image-bytes pads once (tail-pure 3.9→1.7 s); compile-floor drops its report-only child, which compile-floor-gate covers (9.4→6.8 s); x86-64-kernel-create merged into kernel-token with a byte-identical image (~0.5 s). Rejected: unicode-casefold, argv-stdlib-mocks, property-stdlib, date-helpers, x86-64-peer-image (each under 0.3 s); kernel-syscalls/control/atomics/engine/scope-find/unit/policy and token's shared build: they need an argv case dispatcher in the boot harness and would change the peer's image list."
---

Problem: the Habu test audit (group g4) found gate suites whose cases repeat setup, duplicate other rows' coverage or fork more processes than their boundaries need.
Acceptance: each MERGE/SLOW/CUT row below is applied after verifying its coverage claim, or rejected with a reason recorded at closure (e.g. measured saving too small, or batching would change the path under test); every distinct assertion and boundary a row names is kept; registry entries (test/gate-*.f) match the moved/removed suites.
Verification: each changed suite passes on its own with wall time before and after; the full registry (bin/hb --load test/run.f) passes after the batch lands.

Rows:
| unicode-casefold | **MERGE** | Into **unicode-class-exhaustive**, after verification. Keep malformed UTF-8, expansion-versus-normalization and native-symbol rebinding; the table oracle does not cover libunistring behavior. [unicode-test.f:63–104](src/lib/unicode-test.f:63). |

| argv-stdlib-mocks | **MERGE** | Into **argv-stdlib-script-args**. Both rows compile the same file, then select opposite branches. Run existing real-argv assertions first, followed by existing mocks and rollback checks. [argv-test.f:236–291,337–363](src/lib/argv-test.f:236). |

| property-stdlib | **MERGE** | Into **test-stdlib**, after framework cases. Retain generator cycle/alternation checks, shrinking and named refusals; ordinary randomized consumers can pass with a generator stuck in a small cycle. [property-test.f:36–91](src/lib/property-test.f:36). |

| date-helpers | **MERGE** | Into **tail-pure-fixtures**. Preserve Gregorian century exceptions, malformed dates, capacity/range refusals and concurrent distinct dates. Workers are joined and synchronization objects destroyed before return. [stdlib-date-test.f:27–77,109–134](tools/stdlib-date-test.f:27). |

| compiler-compile-floor | **SLOW** | Performs six complete measurements: three children, an implicit load-time run and two restoration runs. The gate repeats the report-only child again. Share that report with **compiler-compile-floor-gate**, and use the load-time success for the existing success-restoration assertions. Preserve refusal restoration and repeat-call cleanup. [compile-floor.f:105–123,151–205](test/compiler/compile-floor.f:105); [compile-floor-gate.f:206–212](test/compile-floor-gate.f:206). |

| tail-pure-fixtures | **SLOW** | Image bounds cases fill nearly the entire image buffer three times using bytewise padding. Pad once to `MSIZE-1`, run the existing oversized append, exact append and full-cursor refusal in sequence. Removes about **74.2 million setup byte writes** on this ARM configuration. [image-bytes-test.f:113–152](tools/image-bytes-test.f:113); [image-bytes.f:141–168](src/os/image-bytes.f:141). |

| x86-64-peer-image | **MERGE** | Into **x86-64-peer-routines**’ `diff`. Same `BUILD-DIFF` and pass chain; move its two additional sign vectors into the existing three-vector diff, retaining one negative. Five cases fit the existing harness. [peer-image-fixture.f:11–26](test/x86-64-peer-image-fixture.f:11); [peer-routines.f:608,735](test/x86-64-peer-routines.f:608). |

| x86-64-kernel-syscalls | **SLOW** | **21 full-kernel images.** Preserve filesystem/I/O, exec/environment/cwd and protected-buffer checks in selected branches of one executable. The emitted 1100 ms poll is not executed by the producer and does not explain its host timing. [kernel-syscalls.f:188,285,336,355,449–461](test/x86-64-kernel-syscalls.f:188). |

| x86-64-kernel-control | **SLOW** | **24 images** repeat the kernel. Preserve multi-stack catch restoration, `finally` exception precedence, guarded-stack recovery and fatal corrupt-hook cases; use fresh launches of one shared image. [kernel-control.f:352–394,413–478](test/x86-64-kernel-control.f:352). |

| x86-64-kernel-unit | **SLOW** | **5 images.** Hook clearing after success/throw/refusal and actual execution/searchability of published code are useful. Count/WID refusals need separate launches, not five identical kernels. [kernel-unit.f:55–71,105,142](test/x86-64-kernel-unit.f:55). |

| x86-64-kernel-policy | **SLOW** | **6 images** share the same image constructor. Preserve admit/seal state and distinct fatal conditions in separate selected launches. [kernel-policy.f:58–106](test/x86-64-kernel-policy.f:58). |

| x86-64-kernel-atomics | **SLOW** | **23 images.** Keep old-value CAS/add, code publication/patch/re-execution and closed-window refusals. They require distinct case execution, not repeated full-kernel emission. [kernel-atomics.f:76,93,233–245,428–459](test/x86-64-kernel-atomics.f:76). |

| x86-64-kernel-engine | **SLOW** | **68 images.** Fourteen numeric cards alone rebuild the kernel for pairs of parse inputs. Keep search/index rewind, provenance and address-cell checks; select existing cases at startup. [kernel-engine.f:364,399,734,805–925](test/x86-64-kernel-engine.f:364). |

| x86-64-kernel-scope-find | **SLOW** | **23 images:** eleven scan, eleven indexed and one late-primitive case. They repeat the full kernel and eleven-record seed. Select scan/index mode and case in one executable, preserving fresh execution. [kernel-scope-find.f:22,40,80–100](test/x86-64-kernel-scope-find.f:22). |

| x86-64-kernel-token | **SLOW** | **2 full-kernel images** for token cursor/end behavior and immediate-binding lookup. Both sets of generated assertions remain useful; put them in selectable branches of one build and receive CREATE’s case below. [kernel-token.f:47–80](test/x86-64-kernel-token.f:47). |

| x86-64-kernel-create | **MERGE** | Into **x86-64-kernel-token**. Same boot harness and private fixture state. Retain the handler invocation and user-stack-preservation assertions; remove the extra suite setup and include its case in the shared build. [kernel-create.f:19–31](test/x86-64-kernel-create.f:19). |
