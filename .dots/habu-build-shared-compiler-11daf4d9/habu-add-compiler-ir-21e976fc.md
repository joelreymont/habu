---
title: Add compiler IR IDs
status: closed
priority: 1
issue-type: task
created-at: "2026-07-26T22:54:38.085129+02:00"
closed-at: "2026-08-04T20:24:43.797083+02:00"
close-reason: "delivered: src/compiler/ir/id.f with test/compiler/ir-id.f and test/compiler/ir-id-concurrency.f are on the proofs branch, and proofs@origin is at the same revision (1bb76eab). That was the dot's own stated close condition. Five blocker edges repointed with that evidence."
---

Depends on active prerequisite `habu-cast-v2-family-741e7bae`, which owns all
shared checker, declaration, verification, package-provider, and refine-lint
changes. This leaf owns `src/compiler/ir/id.f`,
`test/compiler/ir-id-concurrency.f`, `test/compiler/ir-id.f`,
`lib/errors.f`, and `docs/compiler-ir-design.md`. `PLAN.md`,
`LESSONS.md`, and both dot records are shared integration records.

IR-0.1 freezes one `IR-ID` package owner with public nominal identity and
semantic APIs, 26 private checked representation casts, protected
public/private wordlists, one process-wide aligned atomic module-serial
allocator, and a nonforgeable `ir-module-key` required by pack/check. No
`IR-RAW` package exists. The raw names are package-qualified authority:
unrelated or global same-tail role APIs are distinct.

Concurrency runs in a fresh outcome-bounded child. Four tasks publish `READY`,
wait on parent `GO`, allocate disjoint slices, validate typed key/owner pairs,
erase validated owners through one private test-only projection, and write raw
serials to process-shared storage. Removing the barrier fails the overlap
witness; genuine stalls time out at the process boundary; cleanup attempts
every task on every caught parent exit; the exact `E-TASK-STATE` activation
failure is followed by reuse of all four task objects. A worker throw remains
process-fatal inside the isolated child. The observer cannot mint a nominal,
is outside `IR-ID`, and is absent from production loads and compiler APIs.

Checkpoint: owner `IR-ID`; production entry `require src/compiler/ir/id.f`
through `test/compiler/ir-id.f`; the verified baseline has no compiler-ID
module, and the same entry fails before the interface exists. The interface is
the exact ID, allocator, pack/project/check, error, seal, and suite surface in
PLAN.md IR-0.1. Forbidden alternatives are `IR-RAW`, public raw casts, runtime
kind tags, per-dialect copies, and changes to TASK or typed storage. Focused
acceptance is `bin/hb --load test/compiler/ir-id.f`; broader gates are refine,
package, error, typed-local, suite coverage, Maki, PTX standard
library, and the native publication gate.

Acceptance: key round trips; nonzero unique concurrent owners; require replay;
reachable local/owner/bound negatives; static wrong-family rejection; scalar
`ir-count` and `ir-pool-offset` never acquire module bits; sealed authority; no
public raw converter; exact dictionary inventory; and every named gate above.
Module-zero, range, and exhaustion guards are private defensive states
unreachable through the sole public allocator path; the allocator Rocq leaves
own transition coverage and monotonic, nonzero, unique,
exhaustion-before-wrap proofs. This leaf changes no manifest, context, arena,
source, builder, codec, dialect schema, operation, or shared structural lint
source. Before integration or push, root coordinates fetch/rebase, reconciles
active lanes, verifies ownership, and runs combined publication gates.

Claim RELEASED 2026-07-29: work landed (src/compiler/ir/id.f and its suites are on the proofs branch); the agent and its workspace are retired. Dot stays active only until the branch is pushed and verified at the remote, per closure procedure.
