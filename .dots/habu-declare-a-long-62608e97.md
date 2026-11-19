---
title: Declare a long by-value chain of families
status: active
priority: 2
issue-type: task
created-at: "2026-11-19T12:57:24.505975+01:00"
---

Problem: declaring 20000 structures each holding the previous by value takes 8-10 s, 2.3-3.6x per doubling from 5000: TFAM:SUMV-FIND (src/core/type-family.f, called from TYPE-DECL:SUMV-ADD) is 70% of a 2 ms profile. VNX-HASH keys a variant row by its name alone and every structure adds `make` and `unmake` (src/core/structure-make.f:40-41), so one bucket holds a row per structure and each add walks it; TF-CTOR-QUAL-FIND walks the same bucket for each constructor word it recognizes. Acceptance: chains of 5000, 10000 and 20000 structures declare in time linear in the count. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-chainv.
