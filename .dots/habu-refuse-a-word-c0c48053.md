---
title: Refuse a word defined before its PRODUCT names it
status: open
priority: 3
issue-type: task
created-at: "2026-10-03T21:18:22.299969+03:00"
---

A word defined before the SUMTYPE or PRODUCT that generates the same name: printf ': CKDPROD:MAKE ( n -- n ) 1 + ;\nPRODUCT ckdprod 0 FIELD x n ;PRODUCT\n'. --load exits 67 with only 'hb: uncaught throw code 7110' (E-TDECL-NAME), check.f 70 with a statement-throw record at ;PRODUCT. Both should refuse it with a located, named diagnostic. Found while landing 1ca23983.
