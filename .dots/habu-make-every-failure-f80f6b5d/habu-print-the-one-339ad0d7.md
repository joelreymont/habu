---
title: "Print the one line at habu2's kept exits"
status: active
priority: 2
issue-type: task
created-at: "2026-11-12T18:16:44.159532+02:00"
---

Problem: these kept arm64 kernel exits (census at master 045c8ff4) write a fixed line off the shape `<class> habu: <text>[ <16 hex>] lr <16 hex>` (src/habu/crash.f LDIE-LINE): src/habu/habu2.f EMIT-REPLAY-TRAP :1200 ('hb: replay record executed', rc 76); EM-MMAP-CODE-REGION :1941 (78), :1951 (81, code region out of BL range), :1972 (macOS alias, 78), :1988 (Linux views, 78); EM-MMAP-DATA-REGION :2005 (78); EMIT-MARK :2407, :2410 (96), :2413 (98), :2416 (99), the address-cell checks of xt! and ptr-cell!. Each text is fixed when hb is built. Acceptance: each site writes the one line through DIE-LINE, with its exit status unchanged; a test that reaches each site through a real program shows it where one can. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/exits-habu2.
