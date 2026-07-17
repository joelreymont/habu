---
title: V2 transaction journal schema
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-07-11T12:25:26.896307+02:00\\\"\""
closed-at: "2026-07-17T18:16:33.413325+02:00"
close-reason: "Transaction model + JOURNAL/REV codecs landed (16de285f): all four acceptance items test-proven, conservative readings documented, +4 audited refinement rows per precedent. Envelope field wiring noted as follow-up on the envelope dot."
---

Edge note 2026-07-17: blocker habu-v2-canonical-artifact-ee5121b4 removed
- the envelope slices this dot consumes are LANDED (slice 1 dcbe61f9/aa83..,
slice 2 469f1e15: schema/wire/digest/VALIDATE + five foreign-id codecs); the
envelope dot stays open only for fields that THIS dot provides (rev-id +
audit-event-id codecs per its 23.9 note) plus the user-gated cap field -
keeping the edge would be circular. Build transaction.f on the landed
ARTIFACT/owner-codec surfaces.

Implement MODEL-CAD-V2-PLAN.md:1832-1849 transaction data model. Define checked immutable transaction id, base revision, idempotency key, read set including negative lookups, write object set, dependency edges, capability set, budget ledger, obligations, and commit proposal. Acceptance: canonical round-trip, duplicate/conflicting writes reject, omitted read dependency rejects validation, and retry identity is stable. Files: new maki/db/transaction.f and focused tests.

NOTE 2026-07-17 (artifact contract round 2, 676d5a7b): this dot also owns
the rev-id family leg of the plan-23.9 foreign-id contract (owner REV/TX,
content-addressed at commit: constructor + refinements + wire codec pair),
and the JOURNAL package for audit-event-id (monotonic append sequence,
8-byte wire) rides the journal/object-store leg. Land them with the
transaction data model per the 23.9 surface spec.

Claim: agent=txn workspace=.jj-ws/fable-txn (owns maki/db/transaction.f (new) + JOURNAL/REV codec legs per the 23.9 note)

RESOLVED 2026-07-17 (txn lane, commit 16de285f): ACCEPTANCE MET. Landed in
one commit (the three legs are interdependent): (1) package JOURNAL -
audit-event-id, monotonic append-sequence origin (occurrence-identified,
deliberately NOT content-addressed), APPEND/DESC$/SEQ/EQUAL?/VALIDATE/
COUNT + 8B LE codec; (2) package REV - rev-id, content-addressed COMMIT
intern + codec; both with audited refinement pairs rowed per precedent
(+4 rows, epic-owned) and refine-lint seeds; (3) maki/db/transaction.f
(package TX) - txn id, base rev, polarity read set (negative lookups),
write set, dependency edges, capability codes (CAP vocabulary is the
user-gated dot), budget ledger, obligation codes (model owned by
habu-v2-proof-obligation-6cf70b4f), idempotency key, commit proposal.
All four acceptance items test-proven (round-trip + order independence,
duplicate-write reject, omitted-read reject, idempotency stability with
polarity/base discrimination). Conservative readings documented at
definition sites: idem-key = SHA-256 over the FULL canonical action
(never falsely dedups); PROPOSE digests base+writes and interns via
REV:COMMIT. FOLLOW-UP noted on the envelope dot: wire the event/rev
fields into maki/db/artifact.f now that the codecs exist.
