---
title: Finalize provisional KV batch
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T00:45:46.556699+02:00"
blocks:
  - habu-infer-kv-atomic-cdfb00cb
  - habu-own-device-completion-9aed0a22
---

Why: a provisional KV ready owner must publish or roll back only after the exact device stream is quiescent. Interface: package KV owns the terminal operations over the public unconstructible KV:ready carrier. FINALIZE-DONE consumes cache, ready, session, DONE, and outcome; matching success returns finalized(cache,session) after committing every provisional length, matching operation failure returns finalized(cache,session) after cancelling every row, and refusal returns refused(cache,ready,session,DONE,outcome,finalize-error) without mutation. FINALIZE-QUIESCED consumes cache, ready, and QUIESCED; success returns cancelled(cache) after restoring every row, while refusal returns refused(cache,ready,QUIESCED,finalize-error) without mutation. Each DEVRT proof must carry the exact session generation stored by ready. One pending batch per cache makes a separate batch identifier unnecessary. Delete COMMIT-BATCH, ABORT-BATCH, failed-DONE, boolean synchronization, and any device completion knowledge from the provisional builder. Owner: the two typed KV ready terminal transitions only. Dependencies: provisional KV batch and device completion. Production red: no typed quiescence proof owns the sole all-row publish or rollback. Acceptance: success publishes all rows; operation failure and quiesced loss restore byte-identical committed state; every result has the exact stated owners; stale, reused, cross-session, wrong-generation, and double terminal proofs reject before mutation; no partial commit or rollback exists. Forbidden: ready constructor, proof constructor, retry state, partial terminal, DEVRT batch identifier, error-as-state, snapshot, lease, or compatibility path. Smallest owning check: bin/hb --load maki/infer/kv-cache-test.f with the DEVRT completion provider.
