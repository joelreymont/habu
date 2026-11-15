---
title: Delete the pre-hook constant workarounds
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T14:22:56.505788+01:00"
---

Problem: several places work around a limit that no longer exists: that a checked body after the check hook cannot name a constant defined before it. DEF-CONSTANT (src/habu/interpreter.f:2236-2244) publishes every constant's ( -- n ) row as it is made, and an engine built without the workarounds builds both generations and accepts checked bodies naming those constants. The workarounds: constant rows in src/core/cell-effects.f and src/core/engine-error-effects.f; DECL-FRONT's local names (src/core/generated-declaration.f:465-479, NAME-MAX, CON-N/BOOL/R); decl-event.f E-DEV-ARITY; structure-make.f E-SM-DUP; checker.f CHECKER-VIS-PUBLIC; and the comments that state the limit. Acceptance: each is deleted and its callers name the owner's constant; the engine builds twice byte-equal and a checked body after the hook names each constant whose row was deleted. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-prehook.
