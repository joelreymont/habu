---
title: Read the package marker from its cell in habu2.f
status: closed
priority: 2
issue-type: task
created-at: "2026-11-04T13:51:16.751900+02:00"
closed-at: "2026-11-04T20:07:09.792227+02:00"
close-reason: "implemented, reviewed (ACCEPT), merged: candidate /tmp/jerry-pushA/hb built from 605fb2f5 (rc 0); full suite bin/hb --load test/run.f ran 521 of 521, rc 0, no red rows (/tmp/jerry-pushA/suite.log)"
---

Step 3b, lane wlcell2, on 3a's commit. habu2.f NAMESPACE-RECORD, NAMESPACE-PRIVATE, PACKAGE-SCOPE, RECORD-WID, REPLAY-PRIVATE, the qualified definition's row, C-PACKAGE-NEW-RECORD, C-PACKAGE-RECORD-MATCH and FIND-SHADOW read the two cells. Capture, seed and snapshot sites stay (step 7 deletes them). Workspace .jj-ws.noindex/jerry-wlcell2; brief /tmp/jerry-big/wlcell2-brief.md.
