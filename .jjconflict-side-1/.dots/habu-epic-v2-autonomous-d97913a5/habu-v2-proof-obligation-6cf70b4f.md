---
title: V2 proof obligation schema
status: closed
priority: 1
issue-type: task
created-at: "2026-07-11T12:25:27.203645+02:00"
closed-at: "2026-07-17T21:49:24.160421+02:00"
close-reason: "Obligation schema landed (8df2320f): all four plan rules test-proven, NPOL bridged not forked, codec on house conventions. Txn repoint folded into the evidence-applicability chain link."
---

Edge note 2026-07-17: blocker habu-v2-canonical-artifact-ee5121b4 SATISFIED
and removed - the envelope program closed (three slices + wire migration);
the subject-digest and identity surfaces this schema consumes are landed.

Implement MODEL-CAD-V2-PLAN.md:1851-1869 typed obligations naming subject digest, claimed relation, proof domain, policy, verifier class, and required environment. Exact, approximate, empirical, device, safety, and performance domains are distinct families. Acceptance: wrong-domain evidence cannot discharge, subject or environment mismatch rejects, producer cannot be sole verifier when policy requires independence, and changed dependency invalidates only affected obligations.

Claim: agent=oblig workspace=.jj-ws/fable-oblig (owns new maki obligation schema files + focused tests)

RESOLVED 2026-07-17 (oblig lane, commit 8df2320f): ACCEPTANCE MET. Package
OBLIG (maki/db/obligation.f + test): immutable obligation over subject
artifact-id / relation / domain / independence policy / verifier class /
environment config-id / producer + dependency cone; evidence PRODUCT;
DISCHARGE gate; INVALIDATED-BY? minimal-invalidation predicate; canonical
codec on the house conventions with cross-process content keys. Domain
design: six-domain closed DERIVE-eq ENUM with EQUALITY-only coercion -
deliberately NOT a lattice - bridged one-way from NPOL:dom (exact/
approximate/empirical projections; device/safety/performance have no
numeric source); documented at the definition site, no forked lattice.
All four acceptance rules test-proven incl. the static wrong-domain leg
(CHECK-CANDIDATE! verdict fixtures) and unrelated-obligation survival.
TXN REPOINT DEFERRED honestly: needs CAD-KIND:obligation-id (missing
from frozen cad-kinds) + an owner interning registry - folded into the
evidence-applicability dot (the obligation-closure chain link), not
minted separately per drain-plan rule 3.
