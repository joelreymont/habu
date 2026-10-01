---
title: "Refuse a redefined definer's stale does> clause"
status: closed
priority: 2
issue-type: task
created-at: "\"2026-10-01T11:34:48.888164+02:00\""
closed-at: "2026-10-01T17:37:56.682184+02:00"
close-reason: Fixed by ksyuunrk ce03350c (review 178 ACCEPT)
---

Problem: undefining a definer and defining it again leaves the window's created words pointing at the old does> clause, and the AOT capture refuses REFUSE-SITE from that stale clause (r4-acap probe a1 in $HOME/.cache/tmp/kestrel-r4-acap/probe/). Decide at the responsible layer: either the redefinition retires the old clause's children so the capture names the new definer, or the refusal names the stale clause and the redefinition, with the loader agreeing. Acceptance: a1 gives the decided result in the loader and the capture, a case seen to fail first. Files: src/habu/aot-capture.f, the undefine path in src/habu/habu2.f or src/core/checker.f.
