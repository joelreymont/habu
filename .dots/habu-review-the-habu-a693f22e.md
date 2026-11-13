---
title: Review the Habu infrastructure
status: open
priority: 2
issue-type: task
created-at: "2026-11-03T19:00:17.902474+02:00"
---

Joel 2026-11-03: "run a heavy-duty review of the habu infrastructure. save the report somewhere. i will ask about it tomorrow." and "try to simplify things without me". Six read-only code-reviewer lanes on master 37916ffa in .jj-ws.noindex/jerry-review: A image capture and windows, B engine build and install, C build caches, D cross-target links, E test harness, F runtime layout and memory; reports /home/joel/Work/reviews/habu-infra-<A..F>.md, combined /home/joel/Work/reviews/habu-infrastructure-2026-11-04.md. Acceptance: the combined ranked report exists; each simplification without a public change lands as its own reviewed commit with master green; each needing a public change becomes a dot. Landed 2026-11-03 on master 77676bcd: ten reviewed commits from 3cacc110 (section 3 rows 1-6 and 8, row 7 C-M1, the INTERN move); report section 7 lists them and what was examined and not done. Claim: agent=jerry, waiting on Joel (callback rev 5); F-L1 DIRTY-N removed 2026-11-04 (no caller) and the linker rev 6 design.

Census 2026-11-04: the claim named a workspace or agent whose work is gone or landed; reset to open, Claim: unassigned.
