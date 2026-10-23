---
title: Give runtime-regression tier-0 cases tier-1 branches
status: closed
priority: 2
issue-type: task
created-at: "2026-10-22T13:12:42.348646+03:00"
closed-at: "2026-10-23T11:57:30.243366+03:00"
close-reason: "landed on intel as four reviewed commits (6ee1f7d0 6ec67be9 adc89b0d f0ed13be); ARM runtime-regression-test.f rc 0 at the tip; x86 per lane: 211 of 213 FAILs gone, GE-PROCESS-PTY's two left (x86 SQB BP+ crash, breakpoints lane); intel b63e453c; Mac ARM: two-gen ok gen 5 = gen 4, run.f 501/501 with one red (hookless-reject, master 569dc3bf not on intel)"
---

Problem: on x86 (tier 1 only) 211 runtime-regression assertions depend on tier 0; the tier-1 refusals are named elaborator errors (E-NELAB-CTRL -8502, E-NELAB-QUOT -8651, E-HIR-UNMODELED -8286, E-NELAB-MATCH -8650, E-NCOMP-BINDING -8575, E-NELAB-UNDER -8304) and ARM '1 set-tier' matches x86 on 201. Helpers/counts: GEENDCAT -8651 52/-8286 14, GEKT 39, XM/XI/XMF 19, RXSF 15, GE-SINK -8304 10, GEP2SAY 8, GEKC0 6, RXSC 6, RXQ 4, RXQLB 4, RXMK 4, RXDOES 4, does> rc76 4, XO1-3 3, RXQLR/RXQLN 4, RXSQ 2, GEQDEEP 2, cf-cap 3, do-cap/BEGIN-nesting 6, GEENDMAC 2; plus GE-P2-FRAME-WIDE (GEP192 at tier 1 = -8303, keep the GEP64 part) and test/compiler/native-eval.f HOOK-THROWN/NAME-THROWN/ROW-THROWN (run '0 TIER-TEXT'; x86 '0 set-tier' exits 70). Acceptance: each branch selects by tier@, keeps the ARM tier-0 assertion byte-identical, asserts the ARM '1 set-tier' output; green on pulsar and ARM. Files: test/runtime-regression-test.f, test/compiler/native-eval.f. Depends: habu-review-and-land-d6a57b7c. Ownership: those tests. Recipes: ~/.cache/tmp/jerry-c/lane-recipe.md (Mac/pulsar build+run), ~/.cache/tmp/jerry-c/lane-queue.md. Rule: master (ARM) behaviour and tests win; fix x86, never rewrite a master test. Claim: unassigned.
