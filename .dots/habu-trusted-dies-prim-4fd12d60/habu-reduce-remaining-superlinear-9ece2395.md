---
title: Reduce remaining superlinear optimizing allocator work
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-09-11T16:07:57.874253+03:00\\\"\""
closed-at: "2026-09-16T14:34:48.005439+03:00"
close-reason: "superseded by habu-campaign-c1-finish-1f129a00: Remaining superlinear allocator, spill-rewrite and combiner scaling above 256 values; residue is the measured growth, not a target multiple."
---

Owner: Cedar; src/compiler/native/regalloc.f and measured combiner work. Parallel arena/context and symbol owners are separate. Latest integrated class-member optimization d8b108ae is reviewed and focused allocator/loop/edge tests pass; actual Tender ANSWER-COUNT forced-AOT compile7.817084667s to7.263791792s (7.1%), small call100/150 only2-3%. Do not claim full-app7%.

Earlier sorted operand-use index, anchor buckets, direct definition rows and indexed combiner uses are integrated. Remaining old-profile scaling: allocator vs values^1.71 top decile, spill rewrite^1.94, A64RAV ops^1.48, combine^1.21.79definitions above256values account for47.8s total/25s allocator. Largest ANSWER-COUNT~7s,ANSWER-RECORD~5.5s,CHECK-GROUP~3s on older base. Profile .jj-ws/rowan-context/tmp/measurements/tender-perdef.tsv. Large isolated reducer /tmp/cedar-answer-count-aot.f.

Acceptance: identify and remove remaining measured repeated scans at responsible passes, preserve all semantic/verifier checks, compare same definitions and full Tender entirely tier1. Current full source AOT159.013s and latest executable build242.316s are not the requested seconds. Do not substitute JIT speed for this acceptance or add per-pass profiling infrastructure beyond measurements required to locate cost.


Update 2026-09-11 13:59 UTC: Owner now /root/compiler_xhigh_review for the next bounded regalloc.f algorithm repair, separate workspace on 92ef13f0. MB-PLACE-PINNED and MB-PLACE-REST scan all module values at every instruction, and read/write pressure repeats all-value scans. Agent investigates class-start scheduling and sparse pressure queries; no verifier edits (Rowan owns regalloc-verify.f). No performance claim yet. Root owns selfbuild; quiet controlled timings follow it.


Update2026-09-11 14:15 UTC: Two fixes frozen as e9eb714d and c2c9c85a, independently cleared by check_api, integrated as94ac1ecb/7f4c1f77. Pressure iteration uses a complete/unique/bounded evicted-root list; MB-CROSSES uses existing UF member chains with unchanged strict lifetime endpoints. Matching combined binary8aa44a7c9ce424ccd69b82ca478afac85fa2ea3bdd90e1098ca04fe5c8355a89. Frozen current ANSWER-COUNT7.612s to5.179s (32%), call50/100/150:130/291/494ms to124/267/441ms with correct answers. ForcedAOT native-regalloc/loop pass all rejection controls. No full-app or scaling-target claim. Remaining measured costs: class-start scans~454ms and MB-FORBID position lookup/interval walk~1.17s. Agent now finished; remaining algorithm follow-up needs ownership before edits.
