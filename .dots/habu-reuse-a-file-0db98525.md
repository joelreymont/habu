---
title: Reuse a file whose top level rolls back a compile
status: open
priority: 3
issue-type: task
created-at: "2026-10-31T11:21:33.050735+02:00"
---

Found by lane 11's types triage (test/native-file-types-e2e.f ROLL-HIT-CASE): decl.f's top level compiles a STRUCTURE's MAKE and UNMAKE, then catch rolls them back, so no index row holds them. Every warm build compiles them live, counts decl.f compiled (SPIKE-HOOK's F-MISS) and rewrites the same object. Output is correct (warm image == fresh-cache image); the cost is a perpetual miss for such files. The test asserts the current counts (files reused 1, compiled 1).
