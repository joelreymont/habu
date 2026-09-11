---
title: Fix generic KEEP call preservation across variable output rows
status: active
priority: 1
issue-type: task
created-at: "\"2026-09-11T16:07:57.843696+03:00\""
---

Owner: /root/check_api; workspace .jj-ws/cedar-return-quotations. Independent review explicitly requested from Rowan on BB 20260911-130440.104-cedar-c5a3. Parent quotation metadata fix b169c546 is reviewed and integrated as a83fac94; it preserves RQ/ARG-Q and distinct literal body identities.

Reproducer: `: GENERIC-KEEP ( R a [ R a -- S ] -- S a ) over >r execute r> ; 3 [: 2 * ;] GENERIC-KEEP` produced stack (3,6), expected (6,3); callback producing (6,9) yielded (3,6,9), expected (6,9,3). Fixed-width KEEP worked. Actual newly optimized combinators.f then miscompiled BI/TRI. Files /tmp/cedar-keep-row-reduce.f and /tmp/cedar-combinators-replay.f reproduce without app dependencies.

Prototype in elaborate.f preserves return/local/loop values as live SSA across calls, allowing native-frame spills rather than transporting them through callback data-stack windows. Fresh cold build22.609s; actual KEEP(6,3), BI(6,15), TRI(6,15,21) and zero/one/two-output-row reducers pass. Not landed: focused suites expose MODE-OF frame-token rejection -8522, assigned separately to compiler_xhigh_review. Acceptance: freeze implementation, Rowan review, correct actual combinators source and local/loop/return values across ordinary/indirect calls, retain meaningful rejections, run focused native neighbors then integrate and selfbuild. Do not retain cold-prefix KEEP as final acceptance.

Frozen candidate: 931a543cc1d392734dc446baee6e1f55fa18ae92; binary cb7547b6ce0d0d5a5d4ee47a5e0f8d41ddf6fd62f94ee949321c22a0396d098c. Real assertions1.171s and floating-value preservation0.319s pass; six broader suites stop at shared MODE-OF -8522. No running tests; Rowan has exact commit for review.

After frame-order correction80716458: KEEP assertions2.067s; native-quot6.195s,quot-scope5.651s,generic-calls3.448s,internal-call4.297s,many-locals3.325s pass. native-rstack now stops later at NRS-QUOTE-BEFORE-IF -8442; separate habu-fix-spill-shape-5bb56182 owns it. KEEP agent is idle after frozen candidate/tests; Rowan review still pending.
