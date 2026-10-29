---
title: T0 lazy copies and three-operand compare
status: active
priority: 1
issue-type: task
created-at: "2026-10-29T05:11:01.089524+02:00"
---

Lane 1 of the Fable joins design. Workspace jerry-t0j1 on lvwnlymn (B+C line). COPY pushes a shared vreg entry; RELEASE frees a vreg only when no entry holds it; OWN / WRITE-TARGET for in-place and full-overwrite ops; seam CMP, CMP-LIT, SET-FLAG replace CMP-SET/CMP-SET-LIT. Tests SHARE-CASE in t0-install.
