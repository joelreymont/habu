---
title: Refuse or lower spills in a multi-block routine by name
status: open
priority: 2
issue-type: task
created-at: "2026-08-01T11:06:57.218798+02:00"
---

src/compiler/native/spill.f rebuilds one block (BLOCK-OF, E-A64SPILL-SHAPE). It now copies successors and the comparison condition across, so the rewrite itself is ready for more blocks, but the anchoring of a spill decision is not: a plan row names an operation POSITION, and with more than one block a position has to name a block as well before a store can be placed in the right one. Until that lands, a multi-block routine whose register pressure forces a spill must be refused by name rather than lowered into the wrong block - the named boundary the control-flow slice is allowed to keep. The corpus words need at most six registers, so the boundary is not on their path.
