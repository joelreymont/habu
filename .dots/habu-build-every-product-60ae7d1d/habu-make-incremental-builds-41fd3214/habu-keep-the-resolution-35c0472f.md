---
title: Keep the resolution observer across file retirement
status: closed
priority: 1
issue-type: task
created-at: "2026-10-26T13:24:06.632532+02:00"
closed-at: "2026-10-27T05:44:04.172663+02:00"
close-reason: "landed on master 9f905e31: full suite 503/503 (477 s), two-generation gen2..5 byte-equal, landing review ACCEPT (qxkzytxn)"
---

First earlier-write site RESOLUTION-SOURCE-XT+0 (71 reports, census carl-cen.hpML): RWIN-RELEASE resets the observer when a child load retires checker scratch. Release the RWIN arenas without resetting the observer; reset it only at NS-DEFAULT and full teardown/capture. Test-first frame e2e case. Workspace: carl-rwin, brief ~/.cache/tmp/carl-file/brief-rwin.md.

Done as pzmmwxtq (e80c84ad, unpushed). The edge lane (lxkqyyuo, roqvyyxu in carl-edge) now stacks on it: its census lost three objects only because it lacked this fix. Proof of the combined tree in ~/.cache/tmp/carl-edgerw.bEyr.
