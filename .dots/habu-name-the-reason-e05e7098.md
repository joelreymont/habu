---
title: Name the reason when catch or execute is refused
status: open
priority: 2
issue-type: task
created-at: "2026-10-27T00:45:43.119334+02:00"
---

carl-rstk finding: src/core/checker.f RSCATCH (~:4511) and RSEXEC (~:4398) refuse the opaque-xt atom (~:2913, what a top-level ' evaluate pushes) through a bare 'ELSE 0 OK !' and their unify calls never capture expected/actual rows or underflow, so the refusal prints only at 'catch' (bodies too: ': B ( -- ) [: 1 ;] catch ;'). Naming them changes codes pinned by test/top-row-storage-test.f:304-316 and docs/forth-card.md:149 (' F execute underflow E-REJECTED -> E-INPUT-UNDERFLOW; '0 0 catch' has no reason code yet). Needs the code decision before a lane.
