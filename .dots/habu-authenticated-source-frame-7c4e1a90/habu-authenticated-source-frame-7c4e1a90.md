---
title: Authenticated source-frame identity and lifecycle
status: open
priority: 1
issue-type: task
created-at: "2026-07-21T21:10:00+02:00"
---

Problem: compiler and evaluator consumers need one authoritative source-frame
identity and nesting protocol. Ambient input cells, parser pointers, and ad hoc
save/restore sequences cannot prove that a parser is bounded by the current
authenticated source or that child return restores the exact parent state.

This is a campaign controller; do not dispatch it directly. Its owned result is
split into authenticate-owned-bytes, parser-bounds, nested-parent restoration,
failure unwind, and image-parity leaves. The controller closes only after those
five reviewed results land together as the sole source-frame substrate.

Acceptance: add one package-owned source-frame substrate. Its pre-checker
storage follows the private named-offset implementation-layout exception below;
all APIs exposed after checker startup are checked and typed. A provider
opens a frame with canonical logical source identity, a required authenticated
content digest, exact bytes, immutable byte extent, and optional live parent
identity. Before push, the substrate copies and authenticates those bytes into
frame-owned immutable storage; it rejects a digest mismatch and never accepts a
raw pointer plus a claimed digest. The bytes cannot change for the full live
frame lifetime. The substrate assigns a generation-checked frame
handle, and owns the sole nested push/pop/restore stack. Each live frame carries
its exact cursor and limit; every parser reads through that bound. Pushing a
child saves the complete parent input state, normal EOF pops exactly one child,
and throw or candidate rollback unwinds in reverse order to the exact prior
frame and cursor. Publish typed push, pop, restore, and rollback notifications
for consumers. Pop, EOF, throw, and rollback release the frame-owned immutable
byte storage exactly once. A failed open, copy, digest check, or push releases
only storage already acquired and leaves no leak. Popped or rolled-back handles
never alias later frames. Depth,
extent, parent, digest, generation, underflow, double-pop, and restore mismatch
fail before state changes. Native, recovery, ahead-of-time, snapshot, replay,
and fixpoint paths use the same identity and lifecycle contract.

Add focused tests for nested and repeated frames, empty and no-final-newline
sources, every cursor boundary, maximum depth, stale handles, injected failure
after each mutation boundary, child EOF, parser throw, and nested rollback.
Prove child parsing cannot consume parent bytes and every failure restores the
parent state byte-for-byte. Repeated nested-frame accounting plus failure
injection at every allocation/copy/digest/push/release boundary proves no leak
or double release. Canonical identity is independent of address,
handle, allocation order, checkout root, and diagnostic coordinates.

Files: one narrow source-frame identity/lifecycle module, focused tests,
required assembly/bootstrap mirror rows, manifests, and public frame API docs.
Because this is a pre-M1 prerequisite, its private pre-checker state uses the
explicit named-offset implementation-layout exception in
`MODEL-CAD-V2-PLAN.md`; it publishes no raw handle and adds no legacy or
parallel type declaration. Prove the representation on the exact prefix and
recovery load paths.
It does not read or resolve files, own the provider's frozen-source table,
capture persistent declaration provenance, own field
metadata, compose source text, or render diagnostics.

Verify: focused source-frame lifecycle and rollback suites, native/recovery,
snapshot, ahead-of-time, replay and fixpoint parity; typed-local, trust, package,
host, and dot lints. Dependencies: none. Ownership: authenticated
source-frame identity plus nested push/pop/restore lifecycle only.
