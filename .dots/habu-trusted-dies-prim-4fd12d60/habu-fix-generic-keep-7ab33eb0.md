---
title: Fix generic KEEP call preservation across variable output rows
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-09-11T16:07:57.843696+03:00\\\"\""
closed-at: "2026-09-16T14:34:48.774999+03:00"
close-reason: "superseded by habu-campaign-c1-finish-1f129a00: Generic KEEP still mis-orders variable output rows in the optimizing path; the fix is prototyped but not landed or reviewed"
---

Owner: /root/check_api; workspace .jj-ws/cedar-return-quotations. Independent review explicitly requested from Rowan on BB 20260911-130440.104-cedar-c5a3. Parent quotation metadata fix b169c546 is reviewed and integrated as a83fac94; it preserves RQ/ARG-Q and distinct literal body identities.

Reproducer: `: GENERIC-KEEP ( R a [ R a -- S ] -- S a ) over >r execute r> ; 3 [: 2 * ;] GENERIC-KEEP` produced stack (3,6), expected (6,3); callback producing (6,9) yielded (3,6,9), expected (6,9,3). Fixed-width KEEP worked. Actual newly optimized combinators.f then miscompiled BI/TRI. Files /tmp/cedar-keep-row-reduce.f and /tmp/cedar-combinators-replay.f reproduce without app dependencies.

Prototype in elaborate.f preserves return/local/loop values as live SSA across calls, allowing native-frame spills rather than transporting them through callback data-stack windows. Fresh cold build22.609s; actual KEEP(6,3), BI(6,15), TRI(6,15,21) and zero/one/two-output-row reducers pass. Not landed: focused suites expose MODE-OF frame-token rejection -8522, assigned separately to compiler_xhigh_review. Acceptance: freeze implementation, Rowan review, correct actual combinators source and local/loop/return values across ordinary/indirect calls, retain meaningful rejections, run focused native neighbors then integrate and selfbuild. Do not retain cold-prefix KEEP as final acceptance.

Frozen candidate: 931a543cc1d392734dc446baee6e1f55fa18ae92; binary cb7547b6ce0d0d5a5d4ee47a5e0f8d41ddf6fd62f94ee949321c22a0396d098c. Real assertions1.171s and floating-value preservation0.319s pass; six broader suites stop at shared MODE-OF -8522. No running tests; Rowan has exact commit for review.

After frame-order correction80716458: KEEP assertions2.067s; native-quot6.195s,quot-scope5.651s,generic-calls3.448s,internal-call4.297s,many-locals3.325s pass. native-rstack now stops later at NRS-QUOTE-BEFORE-IF -8442; separate habu-fix-spill-shape-5bb56182 owns it. Rowan review subsequently completed; see the current handoff below.


Current handoff, 2026-09-11: Rowan review is NOT clear. The earlier frame-order (-8522) and sibling frame-shape (-8442) repairs are frozen as 80716458 and cfcf1baf and independently reviewed. A matched cfcf1baf binary (SHA256 5f28af107fbcff650f3cf7335b00a07e672cccbe7553ac6e6fbbb4cb7fe6b464) passes native-rstack (6.273 s) and native-regalloc. Historical KEEP/float assertions on an older baked host are not evidence for all candidate code paths.

Three additional blockers from Rowan's review now have active, assigned dots:
- habu-preserve-computed-real-32bc30fe: computed real values across calls; cedar, cedar-real-call.
- habu-preserve-saved-values-efba5b18: hidden values read after BEGIN UNTIL; compiler_xhigh_review, candidate 19d081a6, independent review by check_api.
- habu-support-spills-inside-11052a3b: nonconstant spills in quotation functions; check_api, candidate 5137130d, independent review by compiler_xhigh_review.

A separate exploratory elaboration rejection is recorded as habu-diagnose-lexically-nested-56aafc89 and remains unassigned. Integration and matched full compiler validation remain pending; do not mark KEEP complete from isolated reducers.


Update 2026-09-11 13:59 UTC: All three additional Rowan review blockers now have independently reviewed fixes combined in 92ef13f0, alongside the earlier 80716458/cfcf1baf repairs. A current-layout bootstrap checkpoint built in 21.150 s, SHA256 114c1c09c9c729c052fcc7a84763dbed2ecce424051857ecff04305dc675289b. This is a temporary bootstrap intermediate, not all-AOT acceptance. A fresh process selecting tier 1 before loading tools/native-build.f is now running the actual optimizing selfbuild. Nested-quotation follow-up remains separately assigned.


Combined c0bd71d4 checkpoint SHA2563e40801b9e492da4af82f8e80f414a00fdef7be85220e7e4514f821e4f60dccf passes explicit-tier1 native-rstack4.469s, native-quot4.719s and native-regalloc11.682s. These exercise all three KEEP repairs together with lexical nesting, trap-join repair and allocator optimizations. Actual optimizing selfbuild is still running; no all-AOT compiler output claimed.
