---
title: Single edit point for trusted-word bookkeeping
status: open
priority: 2
issue-type: task
created-at: "2026-07-18T23:21:11.280481+02:00"
---

Every TRUSTED: word today needs two hand-maintained entries: a TRUSTED.md markdown table row AND a line in the trusted-inventory-classes block, and the lints check each separately - the mirror is the gratuitous part. Design: the class/dot annotation becomes a column of the one markdown table; the trusted-inventory ratchet derives its per-site facts by parsing that table (tools lint side) and compares against source reality (the actual TRUSTED:/TRUST sites found by scan). Adding a trusted word then means writing exactly one table row; drift between table and source stays a loud lint failure (that check is the valuable one and stays). Touches tools/trust-lint*, the inventory block format in TRUSTED.md, and the lint tests. Serialize behind habu-retire-deftype-onto-07227854 stage A (both edit TRUSTED.md).
