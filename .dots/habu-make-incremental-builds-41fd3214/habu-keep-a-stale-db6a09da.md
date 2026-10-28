---
title: "Keep a stale replay's restart free of repeated effects"
status: open
priority: 2
issue-type: task
created-at: "2026-10-24T03:41:15.755172+03:00"
---

Astra review of TR1 (F1), applies to the restart design in zoqvzolv: a later segment found stale during replay throws REPLAY-STALE after installing earlier segments and running children cold; hb-build restarts in a new process, so a cold child's external effects (APPEND-FILE etc.) happen twice in one build, against 'a miss leaves exactly the cold state'. Needs a design: decide later-segment eligibility before any external effect, or restart only before one. Evidence ~/.cache/tmp/carl-orc-tr1.RMDQ/out.txt.

Status 2026-10-28: the process restart is gone; a stale replay now resumes in process (1bc5a3be, tools/native-definition-cache.f RESUME-RUN). Remaining question: does a resume re-run a cold child's external effects (APPEND-FILE etc.) already performed before the stale segment? Trace RESUME-RUN; if it does, decide eligibility before any external effect. Claim: unassigned.
