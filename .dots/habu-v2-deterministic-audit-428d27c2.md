---
title: V2 deterministic audit replay
status: closed
priority: 1
issue-type: task
created-at: "2026-07-11T12:25:27.630504+02:00"
closed-at: "2026-07-18T04:40:15.769168+02:00"
close-reason: Content-chained audit log + LLM-free byte-stable replay + the DAUTH commit threading landed complete (703b3f94); all rejects typed, all downstream suites green.
---

Implement append-only canonical events for action requests/results, transaction commits, verifier runs, evidence decisions, promotion, activation, and rollback. Replay from an empty store must reproduce revision/artifact/evidence/state digests without invoking the LLM. Acceptance: event omission/reorder/tamper rejects, nondeterministic action is marked and must carry captured output evidence, and replay is byte-stable across fresh processes.

NOTE 2026-07-18 (promotion landing ecc1a806): this dot also owns wiring
DAUTH:AUTHORIZED-DISCHARGE into CSTORE:COMMIT-AUTHORIZED (the third
validate leg) - needs obligation/evidence context threaded through
transaction/commit-store parameters; the landed DAUTH gate is the
reusable surface.

Claim: agent=detaudit workspace=.jj-ws/fable-detaudit (owns new maki/db audit-replay files + the folded commit-store threading)

RESOLVED 2026-07-18 (detaudit lane, commit 703b3f94): COMPLETE, no
deferrals. Package AUDIT: eight typed event kinds as fixed 131-byte
canonical content-chained records over the landed cross-process keys
(own store - journal.f would interleave with promotion's appends);
omission/reorder/tamper/truncation all reject typed (chain + HEAD +
malformed); nondeterministic events REQUIRE a captured evidence-id
(statically unconstructible without it) and replay from the capture,
never re-execution; STATE-DIGEST is a pure chooser-free fold proven
BYTE-STABLE in a decoy-shifted fresh process. THE FOLDED THREADING
LANDED: CSTORE:COMMIT-DISCHARGED sibling entry folds
DAUTH:AUTHORIZED-DISCHARGE first (refusals before any publish/charge,
HEAD unchanged, no event), records the decision as an audit event, then
delegates to the factored shared publish - COMMIT-AUTHORIZED's arity
unchanged, all existing callers byte-identical green. Three new
LESSONS (sum-arity signature vs MATCH selector spelling; MATCH-arm
payload locals; sibling-entry threading pattern).
