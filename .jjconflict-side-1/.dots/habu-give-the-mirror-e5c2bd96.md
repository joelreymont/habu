---
title: Give the mirror lint a working entry
status: open
priority: 2
issue-type: task
created-at: "2026-08-06T08:16:30.628086+02:00"
---

tools/bootstrap-mirror-lint.f's header claims bin/hb --load runs it standalone, but nothing invokes RUN — it exits 0 silently, a lint that lies about being runnable. Needs the core/CLI split every other lint uses (a bare RUN at file end would fire during the test's require). While there: TEST-FILE? skips any path containing 'test', which would also skip a legitimate corpus file with test in its name — over-broad, harmless today. Found by the adt-fix lane 2026-08-06.
