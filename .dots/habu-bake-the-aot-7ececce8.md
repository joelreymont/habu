---
title: "Bake the AOT DATA window's content"
status: open
priority: 2
issue-type: task
created-at: "2026-08-10T18:38:00.849316+02:00"
---

EM-AOT-RELOC-DATA (habu2.f:3853) reserves the captured DATA span as ZEROED anon-mmap and never copies content, so any initialised DATA in a capture window silently arrives as zeros in the seeded engine (aotsite lane 2026-08-11, reproduced). Replace the zeroed reserve with a copied blob of [d0,d1). Independent of the site record. Acceptance: an initialised cell in the window reads its value after capture+boot; the zeroed-reserve mutation reds it. Files: src/habu/habu2.f. Depends: habu-per-site-relocation-bb9b6d70.
