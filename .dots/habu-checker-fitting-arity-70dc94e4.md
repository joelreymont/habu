---
title: "checker: fitting-arity immediates certify with wrong runtime certificate (p5 soundness hole)"
status: open
priority: 2
issue-type: task
created-at: "2026-07-13T10:37:06.567985+02:00"
---

HIGH. Found by the typed-top-level design probes (2026-07-13, doc docs/typed-top-level.md sec 5 sub-dot 1; workspace probe p5): an IMMEDIATE word with fitting declared arity CERTIFIES inside a checked definition, but executes at COMPILE time (reading below base at compile-time stack state) and leaves an EMPTY runtime body under the declared certificate - e.g. a ( n -- n n ) certificate over a body that does nothing at runtime. Fully-checked source produces a wrong certificate: downstream checked callers unify against effects the runtime body does not deliver. This is not a depth-guard residual - it is a checker MODEL error (immediates' compile-time effects are modeled as runtime effects). Fix shape per the design doc: model immediate execution as compile-time (the certificate must describe the RUNTIME body; immediate tokens contribute their compile-time expansion, not their declared effect), or reject immediates in checked bodies pending the model (fail-closed interim, mirroring the opener treatment) - the doc's sub-dot 1 chooses the model fix with the reject as the interim. Reproducer in the design workspace probes; reduce to a minimal committed fixture FIRST, negative regression, then fix. Type-system lane; blocks tier-1 of the typed top level.
