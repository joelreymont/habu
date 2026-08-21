---
title: Verify straight-line SIR
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T22:57:03.035116+02:00"
---

Full context: design sections 7.3-7.4 require an independent SIR verifier before any optimization or lowering. Validate definitions, types, effects, uses, ownership, source bindings, terminator, and schema. Acceptance: one hostile mutation per invariant rejects with location; valid SQUARE and arithmetic chains pass. Dependency: stack-to-SSA conversion.

RESTATED 2026-07-30 (from the stack-SSA subsumption analysis): most of this
dot's acceptance list (definitions, types, effects, uses, ownership, source
bindings, terminator, schema) is already enforced - IR-VERIFY's 47 checks run
inside IR-BUILD:FREEZE for every module the elaborator produces. The REAL
remaining work is the clause "rejects with location": verify.f throws bare
codes and names no operation id, block, or source span. Implement located
diagnostics - each verifier refusal reports the module, function, block,
operation ordinal and the operation's tape span (spans are already on every
elaborated op) - through the render surface, with fixtures asserting the
located text, not just the code. Do not re-implement any check.

Blocker sweep 2026-08-21 (tracker GC): the blocks: list is gone because every entry in it was already closed - habu-convert-stack-to-6c547119. The prose above still names them as prerequisites; they are satisfied, and nothing in the tracker blocks this leaf now.
