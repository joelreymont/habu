---
title: Model unloop as popping a loop frame
status: open
priority: 2
issue-type: task
created-at: "2026-08-13T21:45:23.388283+02:00"
---

PRIORITY 1 - CERTIFIED JUNK, found by the model-j lane: : B3 ( -- n ) 0 3 0 ?do unloop i + leave loop ; certifies (rc 0) and the engine answers 0 via a read BELOW the loop stack - neither the checker nor the elaborator models unloop popping a runtime loop frame while the engine pops one (habu2.f loop stack). Any i/j/leave AFTER unloop in a live loop body reads a frame that is gone. Existing dot habu-reject-unloop-outside-635a62dc covers only unloop with NO loop open - this is unloop with a loop open and readers after it. Work: the checker's CF frame for the counted loop must be consumed by unloop (or unloop-after readers refused); elaborator second-derives; regression differential pins B3 as refused. Files: src/core/checker.f, src/compiler/native/elaborate.f. Depends: none.
