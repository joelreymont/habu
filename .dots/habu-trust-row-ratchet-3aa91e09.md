---
title: Trust-row ratchet gate metric
status: closed
priority: 2
issue-type: task
created-at: "2026-07-18T14:05:59.649814+02:00"
closed-at: "2026-08-02T15:17:47.172095+02:00"
close-reason: "Superseded by the a8c716c5 hard cut: TRUSTED.md and its row-count gate were deleted, so no trust-row surface remains to ratchet."
---

size-review item 5. TRUSTED.md has 727 rows = the real unchecked surface. Add gate metric failing on count increase (size-guard pattern); lower ceiling per discharge wave; group TRUSTED.md rows by discharge mechanism (ADT rows per census-switchover, nargs seal, etc.) so burn-down is visible.
