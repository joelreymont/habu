---
title: Bind generated STRUCTURE and LAYOUT text to engine words, not the declaring scope
status: open
priority: 1
issue-type: task
created-at: "2026-10-30T11:20:57.575455+02:00"
---

phil 2026-10-30 (ARENA lane reviews, outside the lane, reproduced on the installed hb): the text STRUCTURE and LAYOUT generate binds engine words by name through the declaring scope's search order, so a package definition captures them: sumtype.f:2220 spells field-project and :2234 spells '* record-at' unqualified; layout-buffer.f:159-166 and :489-506 spell 'swap * + @ if … throw then' and LDEFER-BIND/GROW unqualified. Probes /tmp/phil-arena/rat.f and star.f (a package ': record-at ( a n -- a ) drop ;' or its own * before a STRUCTURE makes '2 S3-AT' answer element 0: prints 'element 0 holds 55'); /tmp/rev-arena-fa3/m/shstruct.f prints 'V holds 77'. Type-safe so far; a hygiene hole. A qualified spelling does not fix it: checker.f TOK-IDENTITY gives qualified tokens no intrinsic identity. phil's ARENA lane closes only the unsound case (refuses field-project as a definition name). The general fix (generated text binding engine words by identity, not by name through the caller's order) needs a design.
