---
title: Migrate library allocation callers
status: open
priority: 2
issue-type: task
created-at: "2026-07-13T14:14:22.896827+02:00"
blocks:
  - habu-migrate-mem-numeric-8b11a9be
---

Full context: after MEM:ALLOC-BYTES lands, the exact library callers must stop passing raw sizes. Fix only lib/codesign.f, lib/content-key.f, lib/object-cache.f, lib/process-argv.f, lib/process-env.f, and lib/source.f plus their existing focused tests: construct CAD-NUM:BYTE-LEN, require AS-ALLOC-BYTE-LEN, then call MEM:ALLOC-BYTES. Acceptance: raw/zero sizes cannot reach allocator; byte-identical positive behavior; fresh rg census shows every listed call migrated and no unowned file changed. Depends on packaged MEM owner.
