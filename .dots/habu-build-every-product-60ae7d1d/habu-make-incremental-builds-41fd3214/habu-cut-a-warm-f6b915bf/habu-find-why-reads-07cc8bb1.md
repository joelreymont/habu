---
title: Find why READS-HOLD? costs 20 times more outside a jj tree
status: closed
priority: 2
issue-type: task
created-at: "2026-11-13T04:37:15.142019+02:00"
closed-at: "2026-11-13T04:45:37.378122+02:00"
---

Problem: the same warm engine build run from a plain copy of the tree (no .jj) takes 2.6 s instead of 1.95 s: READS-HOLD? rises from 37 to 717 ms; consumers built from such copies may pay it too. The /tmp symlink is ruled out. Reproducer: /tmp/jerry-warm/scripts/run.sh /tmp/jerry-warm/ihb tag. Acceptance: the cause as path:line, then its fix, so a plain tree's warm build costs what a jj tree's does. Depends: none. Claim: agent=jerry worker workspace=.jj-ws.noindex/jerry-rh.
