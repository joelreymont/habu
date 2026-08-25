---
title: "Widen EMIT-ADDRS to the site's own register"
status: open
priority: 2
issue-type: task
created-at: "2026-08-10T18:38:00.874998+02:00"
---

Option ii ruled (aotsite 2026-08-11): EMIT-ADDRS' shape re-verification hard-codes x9 (W-MOVZ0..W-MOVK3); widen it to take Rd from word 0 and require the other three lanes to equal W-MOVK_k|Rd - STRONGER than today (nothing currently checks the four lanes agree on a register). Unblocks the [']/CODE half without the x9-copy patch. Carries the formal/Common/Reloc.v update. Files: src/habu/habu2.f, formal/Common/Reloc.v. Depends: habu-per-site-relocation-bb9b6d70. Blocks: habu-capture-a-tick-f2bf9d91's chain half.
