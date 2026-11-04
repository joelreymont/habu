---
title: Reduce definition frames to an ordered flag
status: closed
priority: 2
issue-type: task
created-at: "2026-10-28T17:45:51.246184+02:00"
closed-at: "2026-10-31T20:52:55.941733+02:00"
close-reason: "Obsolete: its premise that frame rows have no reader is false; DEFINITION-EXPORT feeds each stored definition's DEPS-SLOT."
---

Problem: once the per-definition key is gone (habu-key-and-check-9cad1739, lane carl-key), the NUNIT-DEP definition frame's row bytes have no reader but two HAS-ORDERED? scans (tools/native-unit-dependency.f ~:1341, ~:1972). The frame storage exists only to feed them: BYTES, FRAMES, USED, FRAME, ROOM, DEF-ROW and its calls in ADD-ROW, OBSERVE-RESOLUTION and BEGIN-DEFINITION, CLOSE's USED reset and RESET's lines. Acceptance: a per-depth 'ORDERED seen' flag, set in ADD-ROW for kind ORDERED while DEPTH > 0, replaces the frame rows; DEFINITION-STORABLE? reads the flag; the row-shape check DEF-ROW performs (ROW-SIZE, ~:267-275) is kept where observations are recorded with no file frame open, or shown redundant. Check whether PUBLISH-PREPARED can then fold into PUBLISH-DEFINITION (INSTALL-COLON, PREPARE-FILE-DEFINITION's bind route). Files: tools/native-unit-dependency.f, tools/native-definition-cache.f. Verify: the 15 native-file gate rows. Depends: habu-key-and-check-9cad1739. Claim: unassigned.
