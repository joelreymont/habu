---
title: Compile branching and looping corpus words
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-01T10:09:18.274400+02:00\""
---

The native chain compiles only straight-line code: 3 of 11 corpus rows, the three easiest, and 2 of those 3 are net SLOWER per call than the old emitter today because entry cost swamps tiny bodies. The rows where codegen quality actually shows are the loops (SUM-TO cost 14589, COUNT-DOWN 24921, old emitter), and the chain cannot express them. Wanted: comparison and branching through the whole chain so that MAX2, SUM-TO and COUNT-DOWN compile, execute identically to the old words on the pinned inputs, and appear in the comparison table. Pieces: HIR gains the comparison and control words those three corpus bodies use (read the corpus - if/else/then, begin/until or do/loop as actually written); A64IR gains compare and conditional-branch forms plus multi-block functions (schema successor machinery exists - SET-CONTROL); the elaborator maps the source control words; the selector lowers them; the allocator handles multiple blocks (real live intervals across blocks - the linear-scan interval representation the regalloc.f header already promises when control flow arrives); the validator re-derives across blocks; the emitter lays out blocks with labels and fixups (dot habu-lay-out-branches-7e04eab2 has the design; the reach check before encoding). Acceptance: MAX2, SUM-TO, COUNT-DOWN compiled by the chain, results identical by execution on the pinned inputs, rows in the table with honest total-per-call numbers reported win or lose. blocks: habu-lay-out-branches-7e04eab2

Claim: agent=looplane workspace=.jj-ws/habu-compile-branching-and-53b03eaf
