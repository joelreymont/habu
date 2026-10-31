---
title: Replay checked writes as cold does on a faulted device
status: closed
priority: 2
issue-type: task
created-at: "2026-10-24T04:43:55.279743+03:00"
closed-at: "2026-10-31T20:52:56.095665+02:00"
close-reason: "Obsolete after lane 11: the cache keeps no WRITE/EMIT replay records."
---

Workspace: carl-tee (smormxxm on sxkulsnz). A hit replays WRITE records through type (unchecked), so a cold GENIO:WRITE that would raise E-IO on a device faulted at replay time stays silent warm. EMIT records replay through GENIO:EMIT (checked), so they agree. The fix needs checked and unchecked writes told apart in section-19 records, or a hit refused when the output device is faulted. Evidence: re-review of smormxxm, open question (a).
