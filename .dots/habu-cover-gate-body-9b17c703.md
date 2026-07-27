---
title: Cover gate body in lint slice
status: open
priority: 2
issue-type: task
created-at: "2026-07-27T18:16:53.158268+02:00"
---

Measured coverage split (found during the wall extraction, and the mechanism behind an earlier masked census miss): the standalone slice invocation (test/gate-stdlib.f -- lint-tools) runs the SUITE table by spawning member files and NEVER loads test/gate-stdlib-lint-tools.f itself, so changes to that gate body file - including calls to removed globals - pass every standalone slice run; only the resident path (test/run.f phase GSI-LINT-TOOLS) loads it. This is how a deleted-global caller at gate-stdlib-lint-tools.f:43-44 stayed green through repeated slice verification. Owned result: either the standalone slice loads the gate body file (preferred - one behavior for both paths), or the split is made structural and loud (the slice output states the body file is NOT covered and a dedicated check compiles it); plus a regression proving a broken reference in the gate body reds the standalone slice. Same class exists potentially for the other gate-stdlib-*.f body files - sweep and state which are resident-only.
