---
title: The dot CLI corrupts created-at on dot on
status: open
priority: 2
issue-type: task
created-at: "2026-08-10T22:06:16.616628+02:00"
---

dot on rewrites a leaf's created-at as a doubly-quoted string ("\"2026-08-10T...\"") - observed independently by three lanes (trap, pkgasm, noret). The lint accepts it so nothing gates, but every touched leaf accumulates quoting. Fix the CLI's re-serialisation to round-trip YAML scalars. Files: the dot CLI. Depends: none.
