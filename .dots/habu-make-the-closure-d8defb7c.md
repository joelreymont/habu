---
title: Make the closure walk linear in the member count
status: open
priority: 3
issue-type: task
created-at: "2026-09-22T12:13:17.231836+03:00"
blocks:
  - habu-make-the-stripped-df05ef4d
---

Problem (measured by the link-linear worker on engine 6dc6cd7b at c921e431): src/habu/aot-closure.f IN-CLO? (:318) scans every closure member per ADD-CLO (:346), so the closure WALK is quadratic in NCLO - about 3.6e8 comparisons on a generated chain of 27,000 words each calling the previous (27,004 members, 1,082,700 B of code), the same order as the OLD>NEW/MEMBER-AT scans df05ef4d removed, which measured 2.4 s. After df05ef4d the AOT maker spends 42.8 s on that chain and the compile alone (the same words behind a one-member closure) is 39.3 s, so the walk is the largest part of the 3.5 s left. Members arrive in discovery order, so an index built when the closure is final (df05ef4d's MORD in aot-lib.f) cannot serve the walk; it needs a structure kept incrementally as members are added. Acceptance: membership answered in O(1) or O(log N) per ADD-CLO - a mark per dictionary record for record members (ndict@ of them, sized like the closure tables), a bitmap per span index for span members (SPAN-N) and a scan of the at most XTC-N body rows, or one sorted-insertion structure - never a value-range guess; ADD-CLO's identity rule unchanged (the entry is the identity: EXPORT's second name reaches one member, aot-closure.f:292-294); before/after maker time on the 27,000-word chain (generator and sources under ~/.cache/tmp/hazel-link-linear/gen/ and src/genchain.f there, kept until this lands) and on Tender's standalone build with aspen; every image byte-identical (cmp on the hb-build-test fixtures, the chain and the Tender image). Files: src/habu/aot-closure.f. Verify: tools/hb-build-test.f, test/gate-aot-positive.f, test/compiler/native-code-span.f, test/run.f. Depends: df05ef4d (its measurement program and the disjoint-members rule). Ownership: hazel. Claim: unassigned.
