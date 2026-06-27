---
title: Mechanical checker-guided repairer for the eval loop
status: open
priority: 3
issue-type: task
created-at: "2026-06-27T12:11:42.956736+02:00"
---

repair-rounds in maki/eval-repair.f are counted from author-supplied trajectories. Build a mechanical repairer that consumes tools/repair-packet.f's structured repair classes (remove_producer/add_producer/fix_type/fix_return_stack) and applies them to a rejected candidate automatically, so repair-rounds/tokens-to-green are produced by the checker+repairer loop, not hand-authored.
