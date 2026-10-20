---
title: Keep warm native image equal
status: active
priority: 2
issue-type: task
created-at: "2026-10-16T08:57:26.028131+03:00"
---

Make ordinary warm definition replay preserve the complete engine and map bytes for identical source. The earlier lost-name problem is fixed in the private source. A real edit/restore run on c739cd4a now reuses 13,015 definitions and recompiles 916, but its restored image is 6,192,247 bytes (c34cca86), versus the original cold image's 6,208,759 (7d204974); maps differ only in the image hash. The next unchanged artifact hit returns the original cold bytes. Retained reproducer/images are in ~/.cache/tmp/habu-engine-edit.x7Ibli6t. Reduce the first logical difference before changing capture or keys; distinguish the already-fixed LTS/FMEND query scratch from another owner. Preserve full image equality in the existing native-build edit E2E, then run required native and convergence checks.
