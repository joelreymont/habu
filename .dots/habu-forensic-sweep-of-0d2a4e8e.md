---
title: Forensic sweep of held-work workspaces
status: open
priority: 2
issue-type: task
created-at: "2026-07-18T23:01:36.978185+02:00"
---

A workspace audit on 2026-07-18 found 109 jj workspaces (of 178) whose ancestry holds non-empty commits absent from master's ancestry; 65 provably-clean ones were already removed. Absent-from-ancestry does not mean lost: most lanes land rebased, so the commits' CONTENT may be fully on master (the maki-layout-valid case), while a few may be genuinely stranded work (the sol-fields-add-shared option-promotion case, which a blind cleanup would have destroyed). Task, read-only: for each remaining workspace except default, merge-gate, checker-exec-of-5923c543, and type-the-new-13b0d871, take the held commits (revset: (::NAME@ ~ ::master) & ~empty()), and classify the workspace as SUPERSEDED (every held commit's diff is content-present on master - check by touched-file inspection or by whether a rebase/apply onto master would be a no-op), STRANDED (some diff content is absent from master and looks like real work - summarize what it is, one paragraph per find), or UNKNOWN (conflicted/divergent states needing a human call - list them). The audit list with commit ids is the starting point; regenerate it fresh rather than trusting a stale scratch file. Deliverable: a written report (docs/ is NOT the place - report text only) with the three lists and evidence per STRANDED find; the orchestrator does all deletions and recoveries. No edits to any workspace, no forgets, no abandons, no bookmark changes.
