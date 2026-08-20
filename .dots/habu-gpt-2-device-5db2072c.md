---
title: GPT-2 device tests need the named-refusal precondition
status: open
priority: 2
issue-type: task
created-at: "2026-08-20T13:50:11.696533+02:00"
---

From gpt2-1 (2026-08-20): the three GPT-2 *-device-test.f members carry no KVT-REQUIRE-DEVICE-style precondition, so a driverless host gets anonymous assertion failures instead of one named refusal - unlike the two properly migrated members. Recorded in docs/ablation.md; the fix is the existing precondition idiom applied to three files. Also carried: the checkpoint-requiring host tests (gpt2-generate/token-guard/serve-close) are named-with-commands in the inventories but run nowhere automatically - they belong in a checkpoint-carrying device lane when one exists. And tree-wide: generic constant over plain n records an open cell (same erasure one type-class up, harmless today) - noted on the review epic 5cb4522c, not actionable alone.
