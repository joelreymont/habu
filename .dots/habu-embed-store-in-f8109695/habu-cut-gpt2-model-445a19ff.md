---
title: Cut gpt2-model over to embedded store
status: closed
priority: 2
issue-type: task
created-at: "2026-07-26T22:59:20.791150+02:00"
closed-at: "2026-07-28T17:04:53.129893+02:00"
close-reason: "Landed at fc2805cd on verified master@origin: GPT2LOAD:gpt2-model embeds WSTORE:store directly in both mapped and allocated commit arms; RELEASE-MODEL consumes it through WSTORE:DISPOSE; mapped, copied, and payload suites, both exact-diff lints, and independent destruction review passed; no model-path WSTORE:resident, WSTORE:HOLD, or WSTORE:RESIDENT-DISPOSE remains."
---

Why: `GPT2LOAD:gpt2-model` still wraps its validated `WSTORE:store` in the
obsolete `WSTORE:resident` handle. That duplicate owner blocks model-owned
weight access and creates a disposal API scheduled for deletion.

Owner and interface: package `GPT2LOAD` changes only `gpt2-model.weights` from
`WSTORE:resident` to `WSTORE:store`. `COMMIT-MAPPED`, `COMMIT-ALLOCATED`,
`MODEL-NL`, `MODEL-KEY`, and `RELEASE-MODEL` keep their public effects. Both
commit arms place the new store directly in the model; `RELEASE-MODEL` consumes
it through the existing `WSTORE:DISPOSE` result surface. Generated `MAKE` and
`UNMAKE` consequently take and return `WSTORE:store`.

Dependencies: the landed `WSTORE:store`, its mapped and allocated constructors,
and `WSTORE:DISPOSE` are sufficient. Fatal disposal and sealed generated
destructuring are separate changes: this leaf neither changes failure semantics
nor adds a read or destructuring interface.

Exact write set: `maki/infer/gpt2-load.f`,
`maki/infer/gpt2-checkpoint-fixture.f`, `maki/infer/gpt2-mapped-test.f`,
`maki/infer/gpt2-copy-test.f`, and `maki/infer/gpt2-payload-test.f`.

Forbidden: no edit to `weight-store.f`; no resident deletion; no raw store
getter, pointer export, mutable owner cell, compatibility wrapper, dispatcher,
weight-read API, forward pass, disposal retag, or unrelated cleanup. Do not
preserve `WSTORE:HOLD` or `WSTORE:RESIDENT-DISPOSE` in the model path.

Checkpoint: on the accepted renamed loader tree, run the mapped, copied, and
payload suites; identify the exact legacy field and both `WSTORE:HOLD` calls;
then change the field and one commit arm and run both diff lints. Stop if direct
store construction or the generated record surface does not check.

Acceptance: the five-file write set has no model-path reference to
`WSTORE:resident`, `WSTORE:HOLD`, or `WSTORE:RESIDENT-DISPOSE`. Real mapped and
copied models round-trip through `GPT2LOAD`, preserve the existing owner and
live-counter stations, and release the same byte counts. Candidate-routed
checker tests accept the store-shaped generated surface and reject raw proof,
wrong-owner, drop, duplicate, and double-release cases. The focused mapped,
copied, and payload suites, both exact-diff lints, and independent destruction
review pass. The result is one store owner and one disposal path, with no shim.

This leaf deliberately stops before model-owned weight reads. A separate frozen
scope must thread caller state through `WSTORE:WITH-SLOT` and restore the owner
on every exit before forward code can consume the embedded store.
