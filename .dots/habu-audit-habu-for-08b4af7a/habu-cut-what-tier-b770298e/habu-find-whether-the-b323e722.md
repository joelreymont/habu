---
title: Find whether the extra-pad width rows have a reader
status: closed
priority: 2
issue-type: task
created-at: "2026-11-13T18:08:07.684730+01:00"
closed-at: "2026-11-13T19:01:19.075832+01:00"
---

Problem: TFC-CON-XPAD-RECORD and TFAM-MATCH-XPAD-RECORD (src/core/type-family.f) store a WF row for each extra pad, but tier 0 takes pad counts from MWIN-CELLS!, and no reader of those rows was found; MATCH's negative-extra refusal may guard nothing tier 0 does. Acceptance: a reported reader for each row and the refusal, or their removal with the comments made true. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-fu4.
