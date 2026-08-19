---
title: dot on re-quotes created-at
status: open
priority: 2
issue-type: task
created-at: "2026-08-19T12:08:51.184052+02:00"
---

CLI bug (effstore-2, 2026-08-19): dot on rewrote created-at: "2026-08-18T..." as created-at: "\"2026-08-18T...\"" - it re-quotes an already-quoted value. Roughly half the active dots in the tree carry the corrupted form (earlier sightings noted it as a quirk; it is a bug). Fix the CLI's frontmatter writer to parse-then-serialize instead of wrapping; sweep the corrupted leaves in the same change. Until fixed, repair by hand after every dot on.
