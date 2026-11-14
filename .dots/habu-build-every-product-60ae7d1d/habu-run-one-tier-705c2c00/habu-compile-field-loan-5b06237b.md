---
title: Compile field-loan, init and stow sites in T0
status: open
priority: 2
issue-type: task
created-at: "2026-10-29T17:41:00.477438+02:00"
---

Tier 1 lowered a C2 stow site on its own path (STAGE-C2-STOW, chosen by C2-STOW-SITE?, deleted with tier 1 in c7dec3ddaf05); T0 compiles a stow site as a plain call, with the wrong arguments. T0 must lower it as tier 1 does before tier 1 retires. Claim: unassigned.
