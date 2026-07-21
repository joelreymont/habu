---
title: Audit aot-closure legacy chain recognizers
status: active
priority: 3
issue-type: task
created-at: "\"2026-07-21T07:45:16.839799+02:00\""
---

Loose end from the FINDPTR retirement (stack d2c2be29): src/habu/aot-closure.f still carries top-level CALL?/TGT/CALL-AT? (lines ~27-36) recognizing the movz/movk/movk/blr absolute-call chain for AOT closure analysis. Post-direct-BL (1e9a3926) no native emitter produces that chain - only the gforth seed does, and seed output never reaches AOT closure analysis. Verify dead, then retire with the same discipline (test rework against the surviving surface, honest delta measurement); if NOT dead (some path still analyzes seed-built artifacts), record exactly which and keep.

Claim: agent=aotchain workspace=.jj-ws/fable-aotchain machine=spark (owns the aot-closure legacy chain-recognizer audit/retirement: src/habu/aot-closure.f + tests)
