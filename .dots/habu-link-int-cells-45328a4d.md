---
title: Link integer cells whose value looks like a pointer
status: open
priority: 1
issue-type: task
created-at: "2026-09-23T10:31:24.074928+03:00"
---

hb-build --stripped refuses a plain variable holding 24997232 (0x017D6D70): rc 70 word=VALUE data-off=11885376 value=24997232 undeclared pointer, on 36f8d059/f42251c4 (frozen tender-pg-prepare-link). Reproducer /home/joel/.cache/tender/habu-gaps/stripped-download/integer.f: variable VALUE; 24997232 VALUE !; : MAIN ( -- ) VALUE @ . cr ;. The same value lives in Tender AUTH ROLE-LITERAL$ string bytes (pm} plus 01). The closure scan classifies a cell by its value where the checker already classifies it by its declaration: an n cell cannot hold a pointer. Fix the scan at the declaration, never pad data. Blocks Tender --stripped; Tender runs --repl until landed.
