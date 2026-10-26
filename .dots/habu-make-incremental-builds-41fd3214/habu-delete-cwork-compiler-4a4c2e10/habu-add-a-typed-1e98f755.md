---
title: Add a typed arena allocation definer
status: open
priority: 1
issue-type: task
created-at: "2026-10-26T17:34:59.223881+02:00"
blocks:
  - habu-move-the-ir-be2d618a
---

Lane 1b of /Users/joel/.cache/tmp/carl-file/wstate-recommend.md: ALLOCATOR NEW-X x generates NEW-X ( ptr ARENA:arena n -- ptr x ): bounded take, zeroed, concrete pointer type, minted through the storage-definer window; plus IR-CTX:ARENA@ ( IR-CTX:ctx -- ptr ARENA:arena ). Proof: a record with a foreign NEWTYPE field and a nominal array certify and run. Claim: unassigned.
