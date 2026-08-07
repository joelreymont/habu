---
title: "E-NCLOB-CAP is the native chain's new dominant refusal"
status: open
priority: 2
issue-type: task
created-at: "2026-08-07T12:59:13.475745+02:00"
---

Problem: with cell widths exported (dot habu-export-the-checker-2bbc831c) the census's E-HIR-UNMODELED bucket collapsed - lib 1183 -> 350, src 507 -> 249, and 424 of lib's 438 distinct refused spellings vanished - but the definitions that now get past name resolution do not compile: they fail with -8568 E-NCLOB-CAP ('more published routines than the clobber record's table holds'), which grew lib 628 -> 1265 and src 166 -> 382. The census's total 'compiled' count did not move (128 in both lib and src), so the win is real but entirely upstream of the ceiling: the clobber record's table is now what bounds how much of the tree the chain can compile. Two things to do and they are separable. (1) Raise or make dynamic the clobber record's table - find its owner and its sizing rule first; a fixed table sized for the old population is the likely cause. (2) The census's code table does not name -8568, so it prints as 'unlisted code -8568 [dialect]' and is CLASSED AS A DIALECT GAP, which is wrong - it is a capacity limit, the 'pressure' or 'instrument' class. Acceptance: -8568 is named and correctly classed in the census, and the table's bound is either raised with a measured new population or documented as the right bound. Files: src/compiler/native/ (clobber record owner), tools/chain-census-core.f (code table). Verify: bin/hb --load tools/chain-census.f -- lib, compare 'compiled' and the refusals-by-class table. Depends: none. Ownership: native clobber record + census code table. Claim: unassigned.
