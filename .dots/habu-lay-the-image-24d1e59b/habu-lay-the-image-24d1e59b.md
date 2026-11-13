---
title: "Lay the image's import table from the tree's writer"
status: active
priority: 2
issue-type: task
created-at: "2026-11-13T14:20:44.558766+02:00"
---

Problem: src/arch/arm64/link-kernel.f READ dies at SAME on a kernel object whose MACHO:IMPORTS-SUM or ELF:IMPORTS-SUM is not the running hb's, because the running hb's writer lays the image's import table (src/habu/link-data.f:516-521) while the tree's kernel reads each libc slot at the tree's writer's row; every hb with the check refuses, so no stage can build a tree that changes the table. Acceptance: the link lays the import table from the tree's writer, SAME and READ's import checks are gone, and a tree that adds a libc import builds with the running hb. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-imp (Mach-O half).
