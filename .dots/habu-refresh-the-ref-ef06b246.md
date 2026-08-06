---
title: Refresh the reference handle after a rebuild
status: open
priority: 2
issue-type: task
created-at: "2026-08-06T08:53:02.856025+02:00"
---

CODEGEN-CABI has no unmap: after CODEGEN-CC:REMOVE plus a rebuild, a second PREPARE silently keeps the handle to the old unlinked library — benign today (same twins), but silent staleness. A real consumer needs DLCLOSE through both image writers and the FFI trusted band; do not build it before that consumer exists — this dot records the staleness so the next PREPARE caller knows. Found by the refuse-bisect lane 2026-08-06.
