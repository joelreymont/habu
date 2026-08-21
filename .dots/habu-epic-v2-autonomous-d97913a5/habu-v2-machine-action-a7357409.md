---
title: V2 machine action registry
status: closed
priority: 1
issue-type: task
created-at: "2026-07-11T12:25:27.021937+02:00"
closed-at: "2026-07-17T23:56:27.789755+02:00"
close-reason: "Action-schema registry landed (34dc03d6): typed declarations, gate-only DISPATCH with honest implemented/declared staging, all four acceptance rules test-proven, action-id minted per precedent."
---

Implement MODEL-CAD-V2-PLAN.md:1939-1953 action-schema registry. Each action declares checked input/output artifact kinds, preconditions, effects, capabilities, deterministic/cacheable flags, budget dimensions, obligations, verifier, diagnostics, and invalidation. Seed SCHEMA:LIST, ARTIFACT:GET, REVISION:DIFF, TX:BEGIN/APPLY/VALIDATE/COMMIT/ABORT, and PASS:RUN. Acceptance: missing declaration fields reject registration, wrong input kind cannot dispatch, unauthorized effects reject before execution, registry enumeration is canonical and replayable.

Claim: agent=maction workspace=.jj-ws/fable-maction (owns new maki/db machine-action files + tests)

RESOLVED 2026-07-17 (maction lane, commit 34dc03d6): ACCEPTANCE MET.
Package ACTION (maki/db/action.f): name-interned declarations with typed
fields (art-kind ENUM I/O, precondition/effect/budget/invalidation
bitmasks, capability CODES held opaque per the user-gated CAP dot -
seeded actions declare empty caps, mechanism tested with abstract codes;
obligations/diagnostics/verifier reuse the landed OBLIG/DIAG enums);
DISPATCH is a protocol GATE never an executor (wrong-kind / unauthorized
/ unsupported / unknown / accepted); staged honestly: implemented =
SCHEMA:LIST ARTIFACT:GET TX:BEGIN/APPLY/VALIDATE/COMMIT, declared-
unsupported = REVISION:DIFF TX:ABORT PASS:RUN (no faked dispatch). All
four acceptance rules test-proven incl. static kind verdicts and
both-order replayable digests. CAD-KIND:action-id minted per precedent
(+2 audited refinement rows). Flag recorded: maki/producer.f's header
still SAYS it is the action registry - wording reconciliation rides
whoever touches producer.f next.
