---
title: Reuse file objects at any HERE alignment
status: active
priority: 2
issue-type: task
created-at: "2026-10-26T18:18:29.348889+02:00"
---

SEG-ALIGNED? (tools/native-unit-object.f ~4376) misses a stored segment with DATA bytes unless HERE mod 8 equals the cold open's; an earlier file that allots a different byte count makes every later non-consumer miss (frame e2e nreq.f, HB_TMP length 50 fails, 51 passes; edge-align-handoff.md). Breaks the gate's exactly-that-file rule. Design: Plan brief /Users/joel/.cache/tmp/carl-file/brief-segalign-design.md (running). The edge lane's test align line waits on this ruling.

Joel approved 2026-10-26 (interview): the loader aligns HERE at every file start and end (src/core/include.f LOAD-UNIT); object format unchanged. Design a60c (Plan). Lane in carl-align on roqvyyxu, brief /Users/joel/.cache/tmp/carl-file/brief-align.md. The edge commit no longer carries the test align line.
