---
title: Attribute the trusted-inventory red
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T13:31:03.467615+02:00"
---

Full context: measured by agent asmguards 2026-07-30 on pristine tree 53cbae17 with an empty working copy - tools/trusted-inventory-test.f (the TRUSTED.md ratchet) fails 4 assertions in the lint-tools slice, identically before and after the assembler-guards change, so it is a pre-existing red on the proofs branch, probably from the master merge or a landed rename touching trusted rows. Root-cause with evidence (which 4 assertions, what changed, which commit introduced it via jj bisect or file history), then either fix the ratchet baseline through its documented refresh procedure or fix the offending rows. Blocks a fully green lint-tools slice.
