---
title: Bound declaration participant arena
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-22T22:24:41.125880+02:00\""
---

Destruction review found that DECLARATION-TRANSACTION:GROW-TABLE doubles signed capacity and multiplies row and cell sizes without a ceiling, so wrapped byte counts can reach the allocator and corrupt the logical capacity. Ownership: package DECLARATION-TRANSACTION only. Add named E-PARTICIPANT-CAPACITY using the free transaction error code 7168, a maximum row count derived from the maximum positive byte count divided by the exact row stride, and checked count-to-byte and next-capacity helpers. INIT must reject capacities outside 1 through the maximum before changing state. Registration at the maximum must reject before calling the allocator or changing table, capacity, count, or row bytes. Normal growth retains sorted registration and exact allocator old and new byte counts. No raw state setter, magic range, saturating arithmetic, alternate allocator contract, or test-only production mutation API. Red proof: the real public INIT or REGISTER path accepts a one-over capacity today; a counting allocator proves rejection makes zero calls. Acceptance: exact-limit and one-over boundaries, normal growth, allocator failure byte identity, duplicate and sealed behavior unchanged, generated declaration transaction suite, typed-local and package diff lints, exact owning load, native fixpoint. Files: src/core/declaration-transaction.f and test/generated-declaration-transaction-suite.f only.

Frozen interface and boundary semantics on base `674e2d49b891`:

- Publish `E-PARTICIPANT-CAPACITY = 7168` from
  `DECLARATION-TRANSACTION`.
- Keep private `ROW-BYTES = ROW-CELLS cells`,
  `MAX-ROWS = $7FFFFFFFFFFFFFFF / ROW-BYTES`,
  `ROWS>BYTES ( n -- n )`, and `NEXT-CAP ( n -- n )`.
- `NEXT-CAP` rejects `cap >= MAX-ROWS`; it doubles only when
  `cap <= MAX-ROWS / 2`, otherwise it returns exactly `MAX-ROWS`. This is a
  checked pre-overflow boundary, not post-overflow saturation.
- `INIT` rejects `cap < 1` or `cap > MAX-ROWS` before the first state write.
- `REGISTER`, after active and sealed checks, rejects `COUNT >= MAX-ROWS`
  before duplicate scanning, allocator execution, row movement, or any state
  mutation.

The focused test may reopen package `DECLARATION-TRANSACTION` in the test
process only and publish a test word that writes private `ST.N` in the supplied
test instance. It must not add a setter, hook, or mutable authority to production
source. Seed the test instance to `MAX-ROWS`, call the real public `REGISTER`,
and prove exact `E-PARTICIPANT-CAPACITY`, zero allocator calls, and byte identity
of the complete state and table. Also prove zero, negative, exact-limit, and
one-over initialization, normal growth with exact allocator old and new byte
counts, and allocator-failure identity. Run the direct suite, candidate
validation, exact-diff typed-local and package lints, and native fixpoint.

Claim: agent=tx_capacity_impl workspace=.jj-ws/habu-bound-declaration-participant-9967c968
