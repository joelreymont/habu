---
title: V2 evidence applicability checker
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-07-11T12:25:27.265401+02:00\\\"\""
closed-at: "2026-07-17T22:35:23.553437+02:00"
close-reason: "Applicability checker + identity legs landed (7f9fd877/d60c0389): mutation matrix complete, refusal rules proven with controls, cache==uncached closure over 7 change-sets, EVIDENCE registry landed. Txn repoint folded into the atomic-txn dot."
---

Edge note 2026-07-17: blocker habu-v2-proof-obligation-6cf70b4f SATISFIED
and removed - the obligation schema landed (8df2320f).

Implement obligation closure and evidence applicability over exact subject/dependency/schema/target/numeric/verifier/environment digests. Produce typed stale/missing/inapplicable results and the minimal invalidation set. Acceptance: mutation matrix pins each key component, static proof cannot satisfy required device execution, performance evidence cannot satisfy equivalence, and cache-hit closure equals uncached closure.

NOTE 2026-07-17 (diag landing 6b19cda8): when this dot (or the promotion
sibling) lands, mint the EVIDENCE owner package per the plan-23.9 codec
mechanism (evidence-id constructor + refinement pair + ID>WIRE/WIRE>ID) -
the diagnostic IR's invalidated-evidence[] field is waiting on it to be
promoted from string[] to nominal ids (see habu-diag-nominal-ids dot).

NOTE 2026-07-17 (obligation landing 8df2320f): this dot's implementation
now also owns the obligation-identity leg the txn repoint needs: mint
CAD-KIND:obligation-id (report-first since cad-kinds is frozen - the
integrator will sanction the one-line kind addition with the claim),
an OBLIG interning registry + KEY>WIRE/WIRE>KEY per the plan-23.9
mechanism, and the mechanical maki/db/transaction.f obligation-code
repoint - plus the EVIDENCE owner registry per the earlier note. The
obligation schema (package OBLIG, DISCHARGE/INVALIDATED-BY?) is landed
and is this dot's substrate.

Claim: agent=evid workspace=.jj-ws/fable-evid (owns new evidence/closure files + the folded legs; SANCTIONED: one-line CAD-KIND additions for obligation-id + evidence-id in maki/cad-kinds.f with verdict fixtures)

RESOLVED 2026-07-17 (evid lane, commits 7f9fd877 + d60c0389): ACCEPTANCE
MET. Identity legs: CAD-KIND:obligation-id minted with verdict fixtures
(evidence-id already existed); OBLIG content-addressed INTERN registry +
both wire codecs; new package EVIDENCE (maki/db/evidence.f) per the
owner-registry template - the owner the diagnostic invalidated-evidence
promotion waits on. +4 audited refinement mints rowed/seeded, strict +
baseline green. Core: package APPLIC VERDICT composing the landed
DISCHARGE/INVALIDATED-BY? into the typed applicable/stale/inapplicable/
missing sum; mutation matrix pins EVERY key component with named
verdicts; static-proof-cannot-satisfy-device and performance-cannot-
satisfy-equivalence proven with positive controls; cached vs uncached
invalidation closure proven EQUAL over 7 change-sets with exact minimal
masks pinned. Conservative digest-axis mapping documented at the
definition site. The txn obligation repoint (now mechanically enabled)
is folded into habu-v2-atomic-txn-a3c26066 with the lane's precise spec.
