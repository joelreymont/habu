---
title: Attribute the compare refuse-stage red
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T17:30:26.975497+02:00"
---

test/run.f's codegen-compare gate member fails in a nested 'refuse' phase: the forked worker throws -8264 E-CODEGEN-COMPARE-STAGE. Reproduces by loading exactly the member's three files, on both 756c7d06 and ee6463fc; the callsplit lane proved its own diff untouched by it. Attribution needed: bisect today's landings — candidates are the scaffold-del gap-variant collapse (34423441) and the pool-crash root-process PREPARE (in the 28ad6123 lineage, which changed what a forked member inherits); check 28ad6123 and 7d841402 with the same three-file load. Then fix at the root. Found by the callsplit lane 2026-08-05.
