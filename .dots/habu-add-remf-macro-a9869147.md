---
title: Add remf macro
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:23.423209+02:00"
---

lib/stdlib.habu: Implement remf macro
- remf: remove property from place (property list)
- Expand to scan and remove pair from plist
- Handles place update via setf
- Return t if removed, nil otherwise
- Add tests for plist modification
- Est: 15 min
