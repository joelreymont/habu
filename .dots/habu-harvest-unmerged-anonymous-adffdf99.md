---
title: Harvest unmerged anonymous work heads
status: active
priority: 2
issue-type: task
created-at: "2026-07-28T20:21:33.349400+02:00"
---

Claim: agent=harvest workspace=.jj-ws/habu-harvest-heads

Full context: a repository-head audit found real work sitting in unmerged anonymous commits that no workspace or bookmark names. Known heads (ids from 2026-07-28): 35bf3f75 modifies bootstrap/cg/forth.fs and src/habu/habu1.f — the stage0-mirror worker's code in progress; 736d60e2 and 856c99b4 each add .dots/habu-fix-stage0-pre-88a4297e.md — the stage0 defect dot that docs/debugging.md:113 and LESSONS.md:47 reference but which is absent from the merged tree (dangling references today); af8f6a42 is the original raw-storage seal worker's head (STATUS.md, bootstrap/cg/forth.fs, src/habu/habu2.f + the seal dot) — superseded by the landed seal commit ac309500, verify then abandon; 44f619e9 and 87ffc20c ('Claim compiler ID parity binding') and b2075717 ('Remove tracked FIXME comments', touches lib/memory.f among others) need triage; 3f5761ce (dots-retire) and the census chain (74984a90..dc54972d) are known held work with workspaces. For each head: diff it, decide merge / supersede / abandon, and prove the decision (a superseded head needs the superseding commit named; an abandoned head needs its content shown redundant). Merge the stage0 dot file so the doc references stop dangling, or retire those references with it. Acceptance: no anonymous non-empty head remains untriaged; dot-dep-lint green; every decision recorded here.

Also triage four orphaned former-workspace directories OUTSIDE the repository
whose jj records were already forgotten: /Users/joel/Work/habu-minion-audit-rRsm,
-bootstrap-wide, -doc-esJW, -seal-UllK. Any uncommitted content there exists
only on disk. Diff each against the repo, harvest anything real, then delete
the directories.
