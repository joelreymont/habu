---
title: Classify held-work Mac workspaces
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T11:12:08.660150+02:00"
---

Requested by the spark session (their cross-host reference: habu-forensic-sweep-of-0d2a4e8e, which does not exist at master@origin — this dot is the Mac-side owner). Roughly 109 held-work lanes exist only on the Mac: registered jj workspaces, .jj-ws/ directories, old-convention ../habu-* sibling directories, and unmerged heads in the repo store. Any silently-lost work is hiding there. Classify every lane as EMPTY (nothing unmerged — retire with workspace forget + rm), SUPERSEDED (content landed on master via another commit — name the superseding commit, then retire), or STRANDED (real unmerged work — describe it in plain English and mint a recovery dot per item), with evidence for every non-STRANDED verdict. A read-only Opus scout ran the sweep on 2026-07-19; its report is the starting inventory (scratchpad forensic/sweep-report.md of that session). Close only when every lane has a verdict, every STRANDED item has a recovery dot, and the retirements are done.
