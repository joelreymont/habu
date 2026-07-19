---
title: Recover safe filesystem primitives campaign
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T11:33:05.864677+02:00"
---

Forensic sweep 2026-07-19: eight stranded lanes held the safe filesystem primitives, side-content codec, and canonical-cache campaign. The side-content codec subsequently landed on master in a2b35a9c and is no longer recovery scope. Governing dot habu-fs-checked-no-7b20610f remains OPEN, and master lib/fs.f still lacks the mkdirat and no-follow primitives this campaign built. Superset lane: sol-safe-change (18 own commits, tip preserved; bookmark recover-safe-change, pushed to origin). Remaining relevant subsets: sol-mkdirat, sol-primitive-proof, habu-fs-checked-no-7b20610f lane (bookmark recover-fs-checked-no, pushed), habu-nofollow-repair (bookmark recover-nofollow-repair, LOCAL-ONLY: its commit chain contains conflicted commits jj refuses to push - resolve the conflicts locally when harvesting). Treat sol-review-side-fixes, sol-review-side, and habu-diff-land-side-98dd8f40 only as landed-history references; do not recover their codec over current master. The remaining filesystem lanes are ~415 commits behind their original base; re-derive rather than raw-rebase.

2026-07-19 note: the recover-nofollow-repair tip is preserved ONLY in the Mac jj store - conflicted ancestor commits block a git push to origin. Harvest that chain on the Mac; the pushed recover-safe-change and recover-fs-checked-no bookmarks remain the durable origin anchors.
