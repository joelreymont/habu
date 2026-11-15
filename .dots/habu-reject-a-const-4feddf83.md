---
title: Reject a constant of an open-width value as such
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T22:31:34.823950+01:00"
---

Problem: src/core/check-walk.f TOP-CONSTANT-REJECT also fires for a value of open width (TOP-WIDE-TYPE? includes open widths), and its message says the value is held in more than one cell, which is not known for such a value. Acceptance: 'constant' over an open-width value gets the open-width record, worded as TOP-OPEN-DROP-REJECT words the drop of one, and a test pins it. Depends: none. Claim: unassigned.
