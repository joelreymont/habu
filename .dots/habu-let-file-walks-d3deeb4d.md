---
title: Let file walks show every entry
status: open
priority: 3
issue-type: task
created-at: "2026-10-30T18:24:04.100814+02:00"
---

FS:WALK-FILES-IF and WALK-PATH (src/lib/fs.f WALK-PATH ~:795, SKIP-DIR? and SKIP-ENTRY?) silently skip entries named .jj, .jj-ws, .git, .out and .dots without asking the caller's allow filter. A caller that must refuse names its rules reject (Maki's PostgreSQL version store walking a built tree) never sees them, so a file a build writes under such a name is left out instead of refused. FS-LIST:EACH-TYPED sees every entry but only in one directory. Fix: the walk hands every entry to the caller's filter; callers that want the repository skip say so (an explicit filter word), so no walk hides entries. Source: tim (Maki), 2026-10-30; Maki's COLLECT comment records it; not blocking.
