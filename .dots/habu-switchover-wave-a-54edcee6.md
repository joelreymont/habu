---
title: "Switchover wave A: option<scalar> + option<idx> over sentinels"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T22:18:57.000713+02:00"
---

docs/census-switchover.md section 5 wave A. After items 8+9+12: migrate the 65 single-value+flag parser/lookup words (STR>NUMBER? string.f:230 r16, STR-PARSE-POS/NEG, DATE-N, PARSE-YMD, MAP-GET map.f:206 r7, FL-*/STR>FLOAT, FIND-EXECUTABLE*, PTXIR-FIND, tools imgdump/imagedisasm/date/json/trusted-inventory parsers) and ~15 -1-index finders (FIND-SUB string.f:81 r27, INDEX-OF :92 r21, A-FIND-INDEX(I), FIND-TAG, HM-PROBE, MAP-INDEX/PROBE, ACAP-POOL-FIND, FS-TRY-*STAT-MODE) to option<T>. Callers rewritten to MATCH. Full site list + radii in the census. DEPENDS: items 8, 9, 12.
