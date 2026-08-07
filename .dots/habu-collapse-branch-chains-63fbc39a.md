---
title: Collapse branch chains and join blocks
status: active
priority: 2
issue-type: task
created-at: "2026-08-07T13:36:13.427534+02:00"
---

Audit: ~100 gap bytes of pure control shape — the chain emits b-to-a-b trampolines (BYTE-FIND 3 of them, CALL-LOOP-3 3, SUM-TO, STORE-LOAD) and one mov;b join block per arm where clang falls through (LADDER 28 bytes of arm exits). A post-layout cleanup: retarget a branch whose target is an unconditional branch, and merge single-predecessor join blocks into fallthrough. Order-of-blocks work in the emitter's layout stage (ORDER-BLOCKS owns placement); the validator must hold retargeted branches to the same reachability. Measure-first on the five named rows. Claim: agent=brcollapse workspace=.jj-ws/habu-collapse-branch-chains-63fbc39a
