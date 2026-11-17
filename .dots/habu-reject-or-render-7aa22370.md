---
title: Reject or render an effect nested past 64 levels
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T14:54:50.688637+01:00"
---

Problem: a declared effect whose quotations nest 65 or more levels deep (`: K ( [ [ ... ] ] -- )`) dies with status 76 at K's definition: FACTS-DIE (src/core/check-hook.f:149) runs because the renderer set RQM, so the facts text cannot say the effect. A correct program meets an internal error. Reproducer: /tmp/jerry-uwl/r4*.f via /tmp/jerry-uwl/run.sh (generator gen.py). Acceptance: the effect's facts text says it at any depth the effect grammar accepts (EFFECT-DEPTH-MAX), or a deeper effect is rejected at its declaration as the grammar's other depth limits are; no status 76. Depends: none. Claim: unassigned.
