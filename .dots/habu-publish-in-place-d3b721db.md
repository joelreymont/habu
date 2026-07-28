---
title: Publish in-place row addition
status: active
priority: 1
issue-type: task
created-at: "2026-07-28T19:52:24.760108+02:00"
---

Why: GPT-2 forward needs residual additions (dst[i] += src[i]) as a public compute primitive; ROW-ADD exists but is private to maki/embedding.f (currently line 62, re-verify at head), where it serves embedding scatter internals - generic buffer math trapped in a domain file. Exact result: move ROW-ADD ( ptr a ptr a n -- ) byte-identical from maki/embedding.f into maki/array.f alongside the other buffer element operations, public in package MAKI, embedding.f consumes the public word. Zero behavior change. Loader-independent: no GPT2LOAD, WSTORE, or model-config contact. Owner: package MAKI in maki/array.f. Acceptance: embedding and autograd suites green UNCHANGED; new maki/array-test.f cases - exact hand-pinned f64 accumulation on integer inputs, n=0 no-op, in-place proof (destination identity, source untouched); a pre-change probe proving MAKI:ROW-ADD does not resolve publicly today; both diff lints with the commit checked out. Forbidden: behavior or signature change, duplicate implementation left in embedding.f, second add formula, loader types.

Claim: agent=claude workspace=.jj-ws/habu-publish-in-place-d3b721db
