---
title: dot add keeps only the last -a blocker flag
status: open
priority: 2
issue-type: task
created-at: "2026-07-20T11:32:41.386328+02:00"
---

The dots CLI accepts repeated -a flags on dot add (dot add "title" -a X -a Y -a Z) but silently keeps only the LAST one: each -a overwrites the previous instead of accumulating, so a dot minted with three blockers records one. Proven twice on 2026-07-20: the vision epic mint recorded 5 of 8 intended edges and the convolutional epic mint 8 of 12; the dropped edges had to be repaired by hand-editing the blocks: lists. Fix the flag to accumulate into the blocks: list (dedup, reject self-reference), add a regression covering multiple -a, and error loudly on an unresolvable id rather than partially minting. Tool source is the dots CLI; find its argument parsing and the blocks emitter.
