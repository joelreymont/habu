---
title: Append native dictionary publications without a full rebuild
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-14T00:01:46.985086+03:00\""
---

Own native expected-index append primitive, NPUB consumer and focused publication/index tests. Reuse LHIDXADD with pending native parent/DOES ownership and task/seal checks; preserve general ndict! restore/rebuild and rollback. Measured rebuild cost 231–235 us at 15485 records, one rebuild per native ordinary definition; startup address membership scans remain a separate issue. Require actual emitted product behavior and zero ordinary full rebuilds, plus independent Astra review.

Implemented in `d084cd18` (primitive) and `1d7b2b71` (NPUB consumer and tests),
with reviewed field-state prerequisite `165f2e32` composed between them by the
empty merge `14e01c88`. Workspace: `.jj-ws/cedar-native-publication`.
`ndict-append ( expected-index -- )` is DNAME-INT and trusted-only. It checks
expected count/capacity, active native pending parent or its one DOES companion,
task state and seal bounds before count/index writes, then calls existing
LHIDXADD. NPUB retains emission, pending-record and DOES validation. Parent
facts still publish between the two DOES appends. No layout state was added.
General ndict! raise/restore still rebuilds; index churn compaction is unchanged.

Actual native products, both complete builds with rc 0:

- A: `/tmp/cedar-family-stage-abi/hb-ndict-append-A`, source `d084cd18`, SHA
  `8aea29ea47553d2ce8428da636cc5d2cde30ba778db873df2f0d39d93011b1f1`.
  Built from indexed B. E-hosted emission first refused the separately fixed
  FIELD-PROJ-A boundary; no append failure was involved.
- B: `/tmp/cedar-family-stage-abi/hb-ndict-append-B`, source `1d7b2b71`, SHA
  `7e715bcb19486a0fd4c21631fe9d6b95ea82b1fe38e9bcbffde3de588edb79d7`.
  Built from A with the field-state prerequisite. Logs are
  `/tmp/cedar-family-stage-abi/native-build-ndict-append-{A,B}.log`.

B passes `native-dictionary-append.f` at both tiers, the new native
`native-dictionary-publish.f`, `ndict-binding.f` at both tiers, and native
`native-create-does.f`. These cover immediate ordinary/DOES lookup and exact
record identity, public/private/qualified/trusted scope, folded collisions,
retirement, rollback/regrowth, general restore, hidden raw access, missing owner,
wrong/negative expected index, repeated parent/third companion append and tasks.

Warmed operation probe `/tmp/cedar-ndict-publish-count.f` compiles an ordinary
word, a three-op word and a DOES definer (four records). On A: four rebuilds,
62,246 visited records, zero LHIDXADD calls. On B: zero rebuilds/visited records,
four LHIDXADD calls. GDB script `/tmp/cedar-ndict-publish-count.gdb` gates at the
probe's one-shot breakpoint before these definitions; startup and cleanup are
outside the count. GDB wall time is not timing evidence. The earlier whole-process
address-registration scan attribution concerned startup, not this compile path.

Two emitted-product refusal probes compare all 524,288 index bytes before/after:
wrong expected index during actual native pending compilation leaves count and
claims at 15,563 (`/tmp/cedar-ndict-append-refusal.{f,gdb,log}`); marking the pending
record retired at actual NPUB:PENDING-CK rejects with E-NPUB-PENDING, count/claims
stay 15,709 (`/tmp/cedar-ndict-malformed.{f,gdb,log}`). Both index pairs are
byte-identical. The second probe changes only record[idx].wordlist via GDB before
the existing validation, then catches its expected refusal; it does not bypass
validation or add a production testing hook.

Three quiet A/B floor pairs, no other hb running, one-minute load 0.75 to 0.69:
trivial 1,241–1,245 to 995–1,000 us; three-op 953–958 to 707–713 us; tier 0 stays
29–30 us. Every run confirms 200 optimizing compiles. Logs:
`/tmp/cedar-ndict-floor-quiet-{A,B}{1,2,3}.log`. The standard tool times compilation
only, excluding source generation and cleanup. Earlier overlapping-load pairs
are separate and are not quiet acceptance. A/B also includes the reviewed
field-state prerequisite; the explicit operation counts identify the removed
dictionary work. About 245–250 us is removed; the 500-us target remains open.

Root's independent source review approved the primitive. Consumer review and
the combined full native gate remain root-owned before integration closes this
leaf. Recovery's native-only append surface refuses honestly because stage0 has
no native pending owner; the recovery chain was not rerun for this change.
