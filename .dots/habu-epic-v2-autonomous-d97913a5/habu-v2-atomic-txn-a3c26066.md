---
title: V2 atomic transaction commit
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-07-11T12:25:26.960293+02:00\\\"\""
closed-at: "2026-07-17T23:17:47.009849+02:00"
close-reason: "Crash-safe commit landed (bde39aea/c1cb38c8): all four acceptance proofs incl. cross-process crash children at every durability boundary; obligations interned; honest process-crash-vs-power-loss boundary recorded with the fsync capability minted."
blocks:
  - habu-v2-txn-journal-d0bc644f
---

Implement the smallest crash-safe commit slice from MODEL-CAD-V2-PLAN.md:1832-1849 over the V2 object store: validate head/base, complete read set, capabilities, budget, digests, and obligation closure; write objects and commit marker atomically; recover old or complete new revision only. Add failpoints before every durability boundary. Acceptance: injected crashes never expose partial revision, idempotent retry returns original result, stale head returns typed conflict, and deterministic replay yields equal revision digest. Depends on transaction journal and V2 persistent object-store owner.

NOTE 2026-07-17 (evidence landing d60c0389): this dot also owns the
mechanical obligation-id repoint in maki/db/transaction.f, now enabled:
B-OBL/S-OBL -> TYPED-BUFFER CAD-KIND:obligation-id; OBLIG+ takes the
nominal; EMIT-OBLIG/DEC-OBLIG switch to the 32B OBLIG:KEY>WIRE/WIRE>KEY
content key (base-rev precedent); OBL-SORT/OBL-DEDUP order by content-key
BYTES (KEY</BYTES< precedent) so the idempotency key / Merkle chain stays
cross-process-deterministic; fixtures become interned obligation-ids.

Claim: agent=atxn workspace=.jj-ws/fable-atxn (owns maki/db/transaction.f + new commit-machinery files + tests)

RESOLVED 2026-07-17 (atxn lane, commits bde39aea + c1cb38c8): the four
acceptance proofs are test-proven. Repoint: TX obligations are interned
CAD-KIND:obligation-id with content-key-byte ordering (idempotency key
cross-process-deterministic). Commit slice: minimal file-backed store
(maki/db/commit-store.f - one file per revision + HEAD marker + idem
record; the pre-existing maki/store.f is a different concern, verified),
three ordered temp+rename boundaries with ADVANCE-HEAD as the atomic
linearization point, recovery = read HEAD (old or complete new, never
torn), production COMMIT byte-wise knob-free (injection = the public
step words + child prefixes). Proofs: T-CRASH-* + cross-process
crash-child at every boundary; T-IDEM; T-CONFLICT typed; T-REPLAY equal
digests. HONEST BOUNDARIES: (1) no fsync/dir-sync primitive exists in
checked Habu - the guarantee is PROCESS-CRASH durability (rename-atomic),
not power-loss; minted as habu-native-fsync-durability. (2) the deeper
validate composition (capabilities + budget + obligation-discharge
authority) is deferred to its owners - folded into
habu-v2-capability-and-0970a96d.
