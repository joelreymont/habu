---
title: "M6: integrate and close change-file"
status: open
priority: 1
issue-type: task
created-at: "2026-07-17T13:03:37.062893+02:00"
blocks:
  - habu-m5-framed-change-fa1fd960
---

Start only after M5 is closed on green master. Freeze one exact final revision; rebase onto current master; run independent architecture, soundness, test, and destruction reviews; apply the union of findings; run every focused owner gate plus full native, maki, ptx-stdlib, bootstrap/recovery, fixpoint, typed-local, host, filemap, dot, and performance-budget gates. Fast-forward and push green master only; close every associated dot with evidence; forget and trash only workspaces with no unique changes.
