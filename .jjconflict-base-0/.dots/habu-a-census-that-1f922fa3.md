---
title: A census that survives an entry file
status: open
priority: 2
issue-type: task
created-at: "2026-08-13T02:31:00.362850+02:00"
---

Found by the ij-locals lane (2026-08-13): the whole-tree census is not reproducible for tools/ because most tools run a MAIN word at load and tools/build-fixpoint-main.f EXITS THE PROCESS mid-census - the run prints no report at all. Either the census learns to survive an entry file (skip top-level execution? load in a child?) or the supported scope is documented as src lib and the tools/ population is measured another way. Interacts with make-the-census-reach-everything 859ea853. Files: tools/chain-census*.f. Depends: none.
