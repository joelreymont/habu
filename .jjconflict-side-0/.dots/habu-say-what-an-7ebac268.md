---
title: Say what an engine-compiled word destroys
status: open
priority: 2
issue-type: task
created-at: "2026-08-03T15:02:53.031764+02:00"
---

A call site narrows its caller-save discipline against src/compiler/native/clobber.f, which has a row only for a routine the native chain published. A word the engine's own emitter compiled has none, so a chain-compiled caller of one saves every live value - and worse, a chain routine that CALLS such a word records A64EFF:GPR-ALL as its own destroyed set (src/compiler/native/emit.f NOTE-CALLEE), so its callers save everything too and the pessimism spreads one level at a time. The engine's emitter is not a mystery: src/habu/habu2.f compiles every word under one convention and uses a fixed handful of registers, so what a word it compiled destroys is derivable from its own code or stated once for that emitter. Landing it would let a mixed program narrow everywhere instead of only where both ends are migrated. It must be DERIVED and not assumed: a wrong row here is wrong code, so the derivation needs the same treatment the chain's own got - a check against the emitted instructions and a refusal that falls back to the whole file. Owners: NCLOB, A64EMIT.
