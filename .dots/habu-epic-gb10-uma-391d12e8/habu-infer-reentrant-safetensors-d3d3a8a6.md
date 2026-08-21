---
title: "Infer: reentrant safetensors load sessions"
status: closed
priority: 1
issue-type: task
created-at: "\"2026-07-21T22:49:17.866125+02:00\""
closed-at: "2026-07-26T08:59:47.668651+02:00"
close-reason: "Implemented under the SAFET re-freeze, reviewed, merged: leaf A landed as 4f3741f10e7f (Replace safetensors registry with load sessions) and leaf B as 5d7a741bb288 (Detach safetensors mappings out of a census), both ancestors of master@origin. Explicit linear SAFET sessions replace the ambient parser registry; DETACH-MAPPING transfers the mapping into a linear SAFET:mapping owner with WITH-MAPPING and UNMAP-MAPPING; a detached census fails closed on byte access; live counters ruled in-contract as leak accounting. The rulings and ratified judgment calls are recorded in the amended dot text; the remaining linear-owner-on-throw checker gap is the linear-scope WITH-owner capability dot minted in this wave."
blocks:
  - habu-add-generic-bounded-359c0944
---

Claim: released.

Stale claim reconciled (2026-07-25): the peer orchestrator confirmed this lane dead in blackboard message 20260724-190033.997-codex-30ac on channel general, which states "I confirm the four old claims are stale: no live worker owns safetensors d3d3a8a6, normalized config 84fc05fa, manifest 27c1030c, or GPT-2 binding f2ed655d", and undertook to release them in the next metadata wave. The former safet_reentrant_impl workspace .jj-ws/habu-infer-reentrant-safetensors-d3d3a8a6 is evidence only. The contract below is being superseded by the rev-4 inference leaf redesign posted as 20260724-191041.846-claude-7d24 on channel general; do not implement from this description until that redesign has replaced or re-frozen it.

Problem: maki/infer/safetensors.f parses through one process-global staging registry. The explicit GPT-2 model can detach a completed parse into an independent published model, but two callers cannot load concurrently or compose nested loads, and the transient LOAD-CONTEXT remains a named unchecked ownership boundary. Acceptance: replace the ambient parser registry with an explicit package-owned SAFET load-session value whose mapping, header cursor, staged tensor metadata, capacity, and error cleanup belong to the caller. OPEN begins one isolated transaction; PARSE and tensor lookup take that session explicitly; DETACH consumes a fully validated session and returns one immutable mapping-backed tensor-census owner; CLOSE consumes any unfinished session. No public word or forward path reads ambient current-load state. Two sessions parse interleaved without cross-talk; a failed session leaves another live session byte-identical; detach and close are exactly once; mappings cannot leak, double-unmap, or outlive their owner; every success and failure leaves no hidden registry. Preserve zero-copy tensor spans, complete pre-publication validation, normalized dtype/shape/name/offset checks, Conv1D orientation, real GPT-2 census, and the copy-once residency handoff. Add checked negative fixtures for duplicate/drop/reuse of sessions and detached owners where the checker can express them, plus hermetic interleaving, nested catch, capacity rollback, malformed header, close-after-failure, detach-after-close, and two-model lifetime tests. If linear session ownership needs a missing checker capability, reduce it, add the exact compiler dot, and keep only one named tested boundary. Files: safetensors loader ownership module, focused loader tests, model consumers, FILEMAP.md, TRUSTED.md, and docs describing load ownership. Verify typed-local diff, package/trust/host/filemap/dot lints, safetensors synthetic and real-artifact gates, GPT-2 two-instance forward, Maki, PTX standard library, fixpoint, and full native gate. This is follow-up hardening; it does not block the current GPT-2 forward once that lane proves DETACH empties the staging parser on every path and published models never depend on it.

Amended at closure (2026-07-26): the SAFET re-freeze (2026-07-25) superseded
this contract and was implemented as two leaves on current master. Leaf A
re-derived the session and census core (OPEN/PARSE/DETACH/CLOSE plus census
readers, unified types; the abandoned ada6cd51 lane stayed reference-only).
Leaf B added the mapping seam: a DEFLINEAR SAFET:mapping owner with
WITH-MAPPING and UNMAP-MAPPING, DETACH-MAPPING as an atomic transfer, and a
census that fails closed on byte access after detach. Ratified leaf-B
judgment calls: the three-cell mapping record is allocated before any census
write (census byte-identical on out-of-memory), double-detach is structurally
inert, and MAP-LEN answering 0 after detach is in-contract. The live counters
SAFET-MAP:LIVE and SAFET:LIVE-OWNERS are ruled in-contract as count-only leak
accounting. The linear-owner-leaks-on-throw gap this dot anticipated is real
and is now tracked by the linear-scope WITH-owner combinator capability dot
minted in this metadata wave.

Claim: agent=claude-solo workspaces=.jj-ws/habu-safet-core (leaf A) and
.jj-ws/habu-safet-seam-b (leaf B). Recorded retroactively at closure;
implemented during the solo-orchestrator shift.
