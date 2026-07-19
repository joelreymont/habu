---
title: Verify ancient sibling checkouts before retirement
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T20:50:09.102353+02:00"
---

Forensic sweep 2026-07-19 leftovers needing a human-grade verdict each: (1) five ancient sibling directories habu-agent-compiler, habu-agent-gc, habu-ansi, habu-jit (2026-02) and habu-tfam (2026-07-11) have operation-pruned jj state - the working copy cannot be recovered through jj, so compare their on-disk trees against the repo store (diff against their likely base or scan for unique files) and either archive real content or remove them; (2) the odin-habu sibling family is held by design (private project, local bookmark recovered-odin-habu) but its origin bookmark odin-habu@origin was DELETED at the remote - flag to the odin owner and decide whether to re-push from the local anchor; (3) workspace audit-ffi-parent (registered 2026-07-19 20:27, empty, parent on master) appeared during a multi-agent wave and no lane admits to creating it - identify the creator, then forget and remove it if confirmed abandoned.
