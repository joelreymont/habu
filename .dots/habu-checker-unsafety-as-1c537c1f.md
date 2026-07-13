---
title: "Checker: unsafety as sym-set, not token names"
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-13T15:18:02.237094+02:00\""
---

Problem: src/core/checker.f UNSAFE-TOK? classifies unsafety by token-NAME membership (deftype/deflinear/value-record...), so unsafety is a property of the name, not the symbol. E-EXPORT-UNSAFE (landed part-a) rejects direct alias minting at EXPORT-RESOLVE, but renamed/aliased/deferred bindings can still launder an unsafe word past export gates. Fix: intern unsafe words into a checker-owned symbol set at definition/seal time and gate on symbol identity at every escape point (export, tick, defer/is). Acceptance: negative checked regressions prove (a) alias of an unsafe word rejects at export, (b) defer/is laundering of an unsafe word rejects, (c) existing E-EXPORT-UNSAFE fixtures stay green. Files: src/core/checker.f, test fixtures. Verify: test/run.f, boot-pin suite, typed-local-diff-lint. Depends: none. Ownership: src/core/checker.f unsafety classification + its tests. Claim: agent=symset workspace=.jj-ws/fable-symset
