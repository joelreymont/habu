---
title: "Send the linker's other rejections as one record each"
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T16:23:19.740538+01:00"
---

Problem: src/habu/linker.f REFUSAL's other callers, TYPE-SOURCE (~:1695, E-COMP-STATE), GOING's failed branch (~:5257, E-FAILED), END-CK (~:5405 E-FAILED, ~:5411 E-LIVE-TASK), print a hand-written 'linker: FILE: ...' line with SAY and then throw a code hb reports again as a generic record, so the reader gets two texts and no fix. The address rejections move to REJECTION's parts in dot habu-give-the-kept-0ed67e30 (link-map.f:44-54 is the pattern). Acceptance: each of these prints one record (code, place or file, what was found, the fix) and nothing after it; REFUSAL is gone; the tests that reach them assert one record. Depends: habu-give-the-kept-0ed67e30, habu-reject-a-build-1499f333. Claim: unassigned.
