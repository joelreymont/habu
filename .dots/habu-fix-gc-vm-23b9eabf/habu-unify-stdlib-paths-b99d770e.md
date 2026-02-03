---
title: Unify stdlib paths
status: open
priority: 3
issue-type: task
created-at: "2026-02-03T10:40:46.720862+01:00"
blocks:
  - habu-fix-gc-work-76dc7cfe
  - habu-ir-skeleton-d7b83105
---

src/main.zig:40-46 loads lib/stdlib.habu; repo also has stdlib.habu; docs/cl-symbols.md points at stdlib.habu line refs. Fix: single source of truth (prefer lib/stdlib.habu); remove/redirect root stdlib.habu; update docs/cl-symbols.md locations to match; add CI guard to prevent drift. Verification: zig build test + REPL loads stdlib.
