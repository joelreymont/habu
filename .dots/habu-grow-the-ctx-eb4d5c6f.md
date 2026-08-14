---
title: Grow the context mapping past four modules
status: open
priority: 2
issue-type: task
created-at: "2026-08-14T07:53:40.254724+02:00"
---

The recorder landing measured E-IR-CTX-SCRATCH (-6644) x9 in the new tranche (lib/fs.f READ-ALL 788B, lib/report.f MD 725B, 6 in cg-mma.f) - ir/context.f MAP-BYTES 512K holds four modules live and its prose already records three doublings for this reason. Proven NOT the tape's fault (tape at raw/3 leaves the same 9). The mapping needs the slab treatment or a derived bound, not a fourth doubling. Also in the tranche, smaller capacity residue to take with it or re-dot on diagnosis: E-IR-OP-CAP x5, E-A64RA-CAP x2, E-NELAB-LOCAL-CAP x2. Files: src/compiler/ir/context.f. Depends: none.
