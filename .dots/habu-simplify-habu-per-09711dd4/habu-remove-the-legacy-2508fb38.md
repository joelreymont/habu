---
title: Remove the legacy VEC- names
status: closed
priority: 2
issue-type: task
created-at: "2026-10-26T11:28:42.556986+02:00"
closed-at: "2026-10-26T12:55:27.852526+02:00"
---

Joel 2026-10-26 (public API removal approved). One vector API: delete the legacy VEC-* words and migrate Etch first (src/kicad_read.f:81-105,198-202 use VEC-HEADER-BYTES, VEC-DISPOSE); Etch owns its downstream checks.

Active: jerry. Commits: habu .jj-ws.noindex/jerry-vecapi (add VEC:HEADER-CELLS, DATA@, LEN!), etch .jj-ws.noindex/jerry-vec (Etch to VEC:), habu .jj-ws.noindex/jerry-vecrm (remove legacy words). Design: VEC: only, callers declare their own ITEM>N casts and index helpers.
