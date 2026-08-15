---
title: Store compiler control structure
status: closed
priority: 1
issue-type: task
created-at: "\"2026-07-26T22:54:58.876373+02:00\""
closed-at: "2026-08-15T14:07:28.878467+02:00"
close-reason: "Closed (vintage audit 2026-08-15, re-executed after the pool incident): control store (op-lane handover landed). Production-consumed by the native chain; suites dual-registered, green through the real entry."
blocks:
  - habu-store-compiler-ops-10440e3e
---

Full context: design section 6.3 requires function/block parents and deterministic windows over generic operation pools. Add function signatures/regions, block arguments, parent identities, operation/successor windows, visibility, and terminator placement metadata. Acceptance: duplicate insertion, cross-function block use, bad parent/window/order, and foreign-owner cases reject. Dependency: operation pools.

Notes from the operation/value pool lane (landed 2026-07-29):
- NAMING TRAP: `block-` is a RESERVED checker prefix. ATOM-TOK? in
  src/core/checker.f:2715 reserves block-* for GPU shared-memory atoms, so an
  ENUM variant or word cannot be named `block-arg` and friends. IR-OP had to
  spell its variant `blk-arg`. Pick names accordingly before writing code.
- IR-OP already stores the definition-kind ENUM with BOTH members
  (op-result, blk-arg) but only mints operation results; a block argument
  cannot exist before this table does. When this stage lands, add the
  block-argument minting path plus VALUE-BLOCK@, keep the wire codes
  unchanged, and add fixtures for a block argument reading back its block and
  argument index.
- IR-OP validates a successor block's owning module but NOT its existence,
  because a branch to a block still under construction is ordinary SSA
  construction. Existence is a freeze-time check (design section 6.5); do not
  add it here.

Claim: agent=ir-ctl workspace=.jj-ws/habu-store-compiler-control-e652c33a
