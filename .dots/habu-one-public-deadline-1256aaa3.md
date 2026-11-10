---
title: One public deadline word pair for the private copies
status: open
priority: 2
issue-type: task
created-at: "2026-11-04T19:43:36.639842+02:00"
---

From tim (Maki), 2026-11-04: 'deadline = now + ms' and 'ms left until a deadline', saturating, is written privately in Habu at src/lib/pg.f:935-950 (DEADLINE bound, LEFT-MS), src/lib/task.f:2095-2110 (SLEEP-LEFT-NS, the shape process.f states as PROC-DEADLINE-AT and PROC-LEFT-MS), src/lib/process.f, src/lib/net/http.f:656 BOUND+; and in Maki at src/tool.f WAIT-RUN, src/project-job.f WAIT/LEFT, src/job.f LEFT-MS. Needs a design: one owner (TIME), its words and effects, and each Habu caller moved onto them; new public words go to Joel.

Consumers: Maki's three private copies are src/project-job.f:263 LEFT and :431-432, src/job.f:321-324 LEFT-MS and src/tool.f:474-478 WAIT-RUN.
