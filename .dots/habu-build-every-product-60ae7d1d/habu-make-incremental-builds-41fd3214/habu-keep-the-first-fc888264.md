---
title: "Keep the first attempt's miss diagnostics across a restart"
status: closed
priority: 2
issue-type: task
created-at: "2026-10-24T03:41:15.771412+03:00"
closed-at: "2026-10-28T17:13:33.893742+02:00"
close-reason: "obsolete: no retry attempt exists since 4c8a3f05"
---

tools/hb-build-lib.f:831 discards the failed attempt's diagnostics; the retry reports 'excluded' and the named miss (e.g. task-slots) is lost. Astra TR1 review F1.
