---
title: Trim Habu test audit rows g6
status: closed
priority: 2
issue-type: task
created-at: "2026-10-23T17:12:47.600583+03:00"
closed-at: "2026-10-23T18:00:32.306545+03:00"
close-reason: "Applied: include-refusal saves on the keyed app-image engine (3.6→0.7 s); whole-source-read forks its cases from one loaded child module (19.4→2.4 s); linear checks fixtures in-process via CHECK:FILE/RUN in forks (12.7→3.0 s); structure-quotation-field builds one image for both producers (8.1→4.3 s; 4 processes, because tier-0 code cannot resave). Rejected: gate-pool-orphan needs a readiness signal in process-fork.f (dot filed); require-cap 0.4 s; structure-opaque claim false, only VERIFY catches a broken OP call (dot filed); c2-records 0.18 s; 13 rows under ~1 s: process-argv, process-cwd, process-env, event-closure, engine-runtime-regressions, native-gate-debug, native-gate-dictionary, enum-decl, structure-quotation-rollback, c2-init-record-schema, repl-address-cell-rollback, repl-open-definition, repl-literal-segment."
---

Problem: the Habu test audit (group g6) found gate suites whose cases repeat setup, duplicate other rows' coverage or fork more processes than their boundaries need.
Acceptance: each MERGE/SLOW/CUT row below is applied after verifying its coverage claim, or rejected with a reason recorded at closure (e.g. measured saving too small, or batching would change the path under test); every distinct assertion and boundary a row names is kept; registry entries (test/gate-*.f) match the moved/removed suites.
Verification: each changed suite passes on its own with wall time before and after; the full registry (bin/hb --load test/run.f) passes after the batch lands.

Rows:
| process-argv | **SLOW** | The successful result-wrapper case repeats the same API/output assertion already made by `PAT-RUN-ARGV-CAPTURE`. Remove that extra child; retain its nonzero-exit arm and the stdin/overflow cases. [process-argv-test.f:152–160,247–260](src/lib/process-argv-test.f:152). |

| process-cwd | **SLOW** | Both successful cat cases recreate and delete the same directory/file fixture. Prepare it once. The additional successful `true` child also repeats success-result conversion already checked by cat; retain the failure arm and missing-cwd reset. [process-cwd-test.f:60–92,120–141](src/lib/process-cwd-test.f:60). |

| process-env | **SLOW** | Identical reset/spawn/wait assertions run **16 times**, without varying input or a cumulative leak assertion: keep one. Another successful result arm duplicates existing capture coverage. Preserve large inheritance, guarded-input and growing-stream cases. [process-env-test.f:135–142,293–298,438–451,474–482](src/lib/process-env-test.f:438). |

| event-closure | **SLOW** | Rebuilds unchanged closure/order results through both `BUILD` and `LOAD-ORDER`, although `LOAD-ORDER` is exactly an alias for `BUILD`. Read both outputs after one computation; remove repeated alias-refusal cases. Preserve root/context/shadowing cases. [event-closure-test.f:284–295,341–344,468–469](tools/event-closure-test.f:284); [event-closure-lib.f:447](tools/event-closure-lib.f:447). |

| include-refusal | **SLOW** | The save-refusal case loads/compiles `app-image.f` into a fresh candidate although the existing app-image engine supplies it. Use that image and retain the root-scope refusal, exact message and no-output-image assertions. [include-refusal-e2e.f:175–197](test/include-refusal-e2e.f:175); [app-image-engine.f:46–47,75–78](test/app-image-engine.f:46). |

| whole-source-read | **SLOW** | Eight engines each install the same FILE-SIZE override and load both checker and LSP code. Load one patched host, then fork isolated existing cases with their mode/path state. Preserve growing-file, unrelated-error and missing-file assertions. [whole-source-read-test.f:69–81,168–175](test/whole-source-read-test.f:69); [whole-source-read-child.f:65–69](test/whole-source-read-child.f:65). |

| linear | **SLOW** | Four children reload the same checker frontend for different fixtures. Use one loaded checker host with isolated cases, preserving diagnostic assertions. Keep the separate save/reopen test: it already uses the shared image and tests persistence. [linear-suite.f:276–286,319–366](test/linear-suite.f:276). |

| engine-runtime-regressions | **SLOW** | Bare `LOOP`/`+LOOP` children are duplicated between orphan-closer and loop-opener groups. Remove two. Also batch the recoverable cases using the same catch wrapper in one child with distinct names; retain terminating counterparts. [runtime-regression-test.f:756–774,870–898,920–987,1137–1160](test/runtime-regression-test.f:870). |

| native-gate-debug | **SLOW** | LIMIT1 duplicates EXACT(1). INCLUSIVE, ROW and JSON-ALL independently resample the same phase: sample once, stop, then emit separately delimited reports. Removes three execs and two busy phases; retain actual timer/reset-race cases. [gate-debug-lib.f:156–175,261–312,374–422](test/gate-debug-lib.f:156). |

| native-gate-dictionary | **SLOW** | BAD-IS repeats one failure merely to inspect two substrings: inspect both after one capture. Remove its rc-only DATA-overflow child, covered more strongly by runtime-regressions’ boundary/diagnostic assertions. [gate-dictionary-lib.f:301–313,1390–1394](test/gate-dictionary-lib.f:301); [runtime-regression-test.f:563–593](test/runtime-regression-test.f:563). |

| enum-decl | **SLOW** | “Legacy versus frontend” declarations/layouts/diagnostics now compare the parser with itself: global ENUM calls `ED-RUN`. Keep one expected-result case per behavior and remove the duplicate parity side. Preserve provisional-state, collision rollback and replay failures. [enum-decl-suite.f:495–509,643–646,759–770,1200–1206](test/enum-decl-suite.f:495); [enum-decl.f:470](src/core/enum-decl.f:470). |

| gate-pool-orphan | **SLOW** | Keep SIGKILL parent-death coverage, which graceful gate-signal cleanup cannot replace. Replace four unconditional 300 ms pre-kill settle sleeps with readiness notification; two capture cases already report their PIDs. Retain actual death/timeout observations. [gate-pool-orphan-test.f:29,124,193,263,295,353–366](test/gate-pool-orphan-test.f:124). |

| require-cap | **SLOW** | Separate 513- and 650-entry children both rebuild and recheck the same prefix. Retain the 650-entry run and its all-path dedup check; remove the smaller repeat. Current doubling also means 650 does not demonstrate another growth after 513. [require-cap-test.f:54–61,89–93](test/require-cap-test.f:89); [include.f:710–712](src/core/include.f:710). |

| structure-quotation-field | **SLOW** | Six processes, two native-compiler loads and three saves. Combine both producers in one initial image; first restore checks both, mutates and resaves; final restore checks mutations. That preserves restore boundaries with three processes, one compiler load and two saves. [structure-quotation-image.f:69–124](test/structure-quotation-image.f:69). |

| structure-quotation-rollback | **MERGE** | Run its existing file after **structure-certify** in the same product process. Preserve 64 failed quotations, nested quotation and SUMTYPE recovery; they stress scratch saturation. Names are distinct and certify reports before the next reset. [structure-quotation-rollback.f:5–18](test/structure-quotation-rollback.f:5); [structure-certify-suite.f:218](test/structure-certify-suite.f:218). |

| structure-opaque | **SLOW** | Remove standalone VERIFY. FORGE reloads the same fixture, accepts its public ROUND definition, then asserts the exact forbidden constructor/token failure. That already establishes the successful prefix. Keep PROGRAM’s runtime behavior. [structure-opaque-e2e.f:19,38–59](test/structure-opaque-e2e.f:19). |

| c2-init-record-schema | **MERGE** | Run with **c2-init-schema** on the same whitebox engine. Both construct terms and query the same metadata boundary; preserve nested field placement, scope substitution and invalid field-ID cases. No separate process boundary is tested. [c2-init-record-schema.f:24–69,88–119](test/c2-init-record-schema.f:88); [c2-init-schema.f:21–57](test/c2-init-schema.f:21). |

| c2-records | **SLOW** | Program/refusal/lifecycle/scope fixtures launch four same-tier hb-root sessions and reload common dependencies. Load those files sequentially in one child and check all markers; retain separate tier-one and saved-image consumers. Removes three starts, not image builds. [c2-records-e2e.f:29–76](test/c2-records-e2e.f:29); [c2-image-pair.f:72–75](test/c2-image-pair.f:72). |

| repl-address-cell-rollback | **SLOW** | Each recoverable case starts a PTY engine, selects tier and reloads the same probe. Reuse one child per tier, snapshot before each bad line and use distinct follow-up names. Keep relocation-offset/DP restoration assertions. [repl-address-cell-rollback.f:28–31,54–57,70–77](test/repl-address-cell-rollback.f:28); [repl-address-cell-probe.f:8–15](test/repl-address-cell-probe.f:8). |

| repl-open-definition | **SLOW** | Multiline success, rejection recovery and using-state cases separately start sessions that all recover normally. Combine them per tier. Keep EOF-inside-definition and throwing-exit-hook sessions separate because termination is their boundary. [repl-open-definition.f:72–148](test/repl-open-definition.f:72). |

| repl-literal-segment | **MERGE** | Add its recovery scenario to **repl-address-cell-rollback**, in the matching-tier child after relocation checks. Preserve segment bytes, later-allocation safety and intern identity; these need the same PTY recovery boundary, not another process. [repl-literal-segment.f:27–34](test/repl-literal-segment.f:27); [literal-segment.f:66–71](test/compiler/literal-segment.f:66). |
