---
title: Give an XML reader a cursor record
status: open
priority: 2
issue-type: task
created-at: "2026-11-22T12:32:58.567461+01:00"
---

Problem: the XML:reader state (src/lib/xml/state.f:27-60 cursor, :254-258 MINT, ERASE and STATE) crosses as ^n and is read through ptr-field and table offsets in state.f, read.f and names.f (about 160 words). Acceptance: MINT and ERASE take and answer ^cursor; the frame, attribute and namespace tables are typed storage the cursor owns; no cast or offset reaches the state. Depends: none. Claim: unassigned.
