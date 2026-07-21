---
title: Stable nested declaration source origins
status: open
priority: 1
issue-type: task
created-at: "2026-07-21T19:22:10+02:00"
blocks:
  - habu-authenticated-source-frame-7c4e1a90
---

Problem: declaration metadata needs stable source identity, byte span, and
parent-frame chain, but parser pointers and ambient input cells are transient.
This dot consumes the sole authenticated frame identity and lifecycle API owned
by `habu-authenticated-source-frame-7c4e1a90`; it must not build a second input
stack or frozen-source provider.

Acceptance: add a checked immutable `SOURCE-ORIGIN` arena and capture/reflection
API over that frame owner. The exact provenance identity key is canonical
logical source identity, required content digest, exact byte start and length,
and the canonical bounded parent-origin chain. Diagnostic path, include chain,
line, and column are reflected separately. Popped live frames become unusable, while
captured immutable origins referenced by published metadata remain valid.
Candidate rollback retires provisional records; generation checks prevent stale
handles aliasing later storage. Reject malformed, out-of-range, cyclic,
depth-overflow, or identity-mismatched records. Re-intern canonical records on
snapshot, ahead-of-time, and replay so allocation order and raw handles never
become identity.

The existing canonical declaration identity remains authoritative, including
visibility through its exact owner; this dot neither redefines nor narrows that
identity's field set. No provenance component—including logical source identity, content digest, parent
chain, path, include chain, line, column, and byte span—nor checkout root, handle
number, or allocation order ever enters a family, type, constructor, or other
semantic hash. Provenance remains immutable metadata reflected separately from
semantic identity.

Files: a narrow typed immutable-origin module, focused
origin/declaration/diagnostic tests, and public origin documentation. Do not
read files, resolve loaders, push/pop evaluator frames, compose/remap source, or
own field rows.

Because this is a pre-M1 prerequisite, use a bootstrap-safe typed
representation such as opaque or linear `DEFTYPE` handles with named outcomes.
Do not consume unified ENUM/STRUCTURE or add legacy PRODUCT/SUMTYPE
declarations. Verify the exact prefix and recovery load paths.

Verify: direct and nested source-origin fixtures, checker candidate rollback,
include/evaluate recovery, diagnostic JSON/text spans, snapshot, ahead-of-time,
replay, recovery, fixpoint, and full native gates; typed-local, trust, package,
host, filemap, and dot lints.

Dependencies: `habu-authenticated-source-frame-7c4e1a90`. Ownership: immutable
typed provenance capture, persistence, and reflection over the sole
authenticated frame API. Frame identity/lifecycle, the frozen provider, loader
semantics, field rows, and generated publication remain with their exact owners.
