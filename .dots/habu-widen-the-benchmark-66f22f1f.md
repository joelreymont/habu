---
title: Widen the benchmark with the surveyed hot words
status: open
priority: 2
issue-type: task
created-at: "2026-08-02T09:45:19.020992+02:00"
---

The survey's seven recommended rows, each a shape the 11-word corpus lacks, each genuinely hot: TAG (checker.f:152), WS? (json-read.f:252), SYM-FOLD-C (checker.f:3542), MAX-DIM (maki/tensor.f:76), COUNT-CHAR (lib/string.f:103), T-RES-WALK (checker.f:887, loop test IS a call), VEC-COPY-CELLS (lib/vector.f:139, calls in a ?do body). Build as a SECOND corpus file with its own committed baseline per the established convention (the original 11 stay pinned), measured with the same methodology, results identical by execution. Needs: the comparison/bitwise leaf, the while/else leaf, and the call leaf (constants references must be checked early - T-VAR/S-PUSH/W32 style names may already fold through the fixed/const meanings). T-DIST2 (maki/array.f:32) is recorded here as the first float-campaign row for when floats land. Blocks on habu-complete-the-comparisons and habu-compile-while-and.
