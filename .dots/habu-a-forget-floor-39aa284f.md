---
title: A forget floor from a low record can reclaim live code
status: open
priority: 2
issue-type: task
created-at: "2026-08-10T10:14:27.829961+02:00"
---

PRE-EXISTING, found by the publog lane 2026-08-10, shared by publish.f and clobber.f: FORGET-DEFS-FROM takes its reclamation floor from a record's START, so forgetting a low-indexed word whose REPUBLISHED code sits high reclaims code belonging to records that are not retired - their rows are then correctly dropped by the watchers, but the CODE was live. Derive the true rule (the floor must be the minimum over retired records' starts only, or reclamation must refuse when a live record sits above the floor) and fix it in the engine's forget path with a fixture that forgets low while a republished-high word is live and then CALLS the high word. Files: src/habu/xref.f (FORGET-DEFS-FROM), watchers in publish.f/clobber.f unaffected. Depends: none.
