---
title: Rebuild an application warm without -8599
status: active
priority: 1
issue-type: task
created-at: "2026-10-30T19:59:08.834181+02:00"
---

Problem: a second tools/hb-build.f build of a small app (require src/lib/memory.f, a variable, MAIN) into the same HABU_OUT dies 'hb: uncaught throw code -8599' (E-NUNIT-FILE), rc 67, no executable; the cold build into a fresh out root succeeds. Seen by lane MAP on an engine from mlszotnk without MAP's change (carl-fmap/.out/map/link/before2.log). Lane WLINK: reproduce on the lane 11 tip and master, root-cause, fix at the responsible layer, add the warm rebuild to the existing hb-build app test (red before, green after). Brief carl-dots/.out/carl-file/brief-fix-wlink.md. Acceptance: both builds rc 0, both apps print 3, the second reports reuse. Claim: carl worker, workspace Habu .jj-ws.noindex/carl-fwl.
