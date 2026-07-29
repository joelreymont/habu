---
title: Atomic generated-declaration transaction
status: closed
priority: 1
issue-type: task
created-at: "2026-07-21T19:22:10+02:00"
closed-at: "2026-07-29T22:42:01.341276+02:00"
close-reason: "Already landed: STRUCTURE uses the five-participant generated-declaration transaction; generated-declaration and structure suites pass. Capacity/size evidence landed in e5fd1de5675a and 8e7e608062c5."
---

Problem: current multi-word declaration paths have no shared all-or-nothing
publication owner. Validating or publishing the first generated word does not
prove the rest of the set, and a later failure can leave current dictionary,
checker, type-family, declaration-event, package, or protected-wordlist state
partially visible.

Acceptance: compose the existing engine `evaluate` savepoint, checker rollback
frame, type-family rollback, `DECL-EVENT` transaction, package/dictionary state,
and protected-wordlist count; do not create a parallel transaction system. Add
one generic typed participant protocol with failure-atomic registration,
ordered snapshot and prepare, forward commit, and reverse-order rollback to the
captured snapshot. The transaction core knows only participants, never
owner-specific rollback code, so later state owners can enroll without changing
the protocol. Preserve the original compile or runtime throw as the primary
failure even if rollback reports another error; rollback diagnostics are
supplemental and cannot replace or swallow it.

Expose one package-owned transaction for current multi-word declaration paths.
Preflight their complete ordered set of names, effects, bodies, visibility, and
current registry capacities before the first publication. A throw, compile
reject, capacity failure, or injected failure restores every enrolled current
owner byte-for-byte. Publish `DECL-EVENT`, protection, sealing, and ready state
last. Prove the protocol with one representative declaration that generates
multiple words, including failure in its second word and at every participant
boundary. Successful commit preserves declaration order and the same callable
words, signatures, visibility, snapshots, ahead-of-time image, and fixpoint
identity as the current successful path. Repeated failed and successful
evaluations neither leak nor duplicate rows.

Files: the existing declaration transaction and generated-declaration owners,
compiler/checker rollback integration only where the shared transaction needs
it, focused generated-declaration transaction tests, assembly/load manifests,
and `TRUSTED.md` only for a proved missing typed boundary.

Verify: red-first failure injection before and after participant registration,
snapshot, prepare, publication, commit, rollback, and final
protect/seal/ready boundaries; failure in the representative declaration's
second generated word and in each enrolled current owner; participant prepare,
commit, and rollback failures; reverse-order unwind with original-throw
preservation; duplicate and failed participant registration; nested savepoint
commit/rollback; one-short capacity with zero mutation and exact-capacity
success; failure-then-success byte identity versus clean success; current
dictionary, checker, type-family, declaration-event, package,
protected-wordlist, rollback, snapshot, ahead-of-time, recovery, and fixpoint
suites; typed-local, trust, package, host, and native dot gates.

Dependencies: none. Ownership: the generic participant/savepoint protocol and
adapters for owners that exist on current `master`, proved through one
representative multi-word declaration. Source-origin/provenance enrollment
belongs to `habu-stable-source-origin-frame-9d4b2a61`. Exact generated ENUM and
STRUCTURE integration belongs to their generator dots; neither is acceptance
for this leaf.

Candidates e5fd1de5675a and 8e7e608062c5 are preserved for independent review against this contract. Claim: unassigned.
