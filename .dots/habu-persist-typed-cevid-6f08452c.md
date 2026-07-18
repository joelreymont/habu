---
title: Persist typed cevid evidence rows
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-18T13:22:54.298423+02:00\""
---

Follow-on from habu-v2-competitive-evidence-5d07d471 (closed): add the durable typed store codec for cevid/v1 evidence rows, mirroring how the closed habu-persist-typed-bench-2d15efa2 persisted bench/v1 after the schema landed (maki/competitive-store.f pattern: typed encode/decode, byte-stable round-trip, crash-safe write). The schema + canonical render + byte goldens live in maki/competitive-evidence.f (package CEVID, codes -5417..-5421). Optional second item, decide at claim: wire the sealed unit enum into run-metric MEASURE only if a consumer needs per-metric units at run time (deliberately kept out to avoid resignaturing MEASURE and every caller). Verify: cevid round-trip suite + maki/test.f.

Claim: agent=cevstore workspace=.jj-ws/fable-cevstore (durable cevid/v1 codec per the bench/v1 precedent; units-into-MEASURE only if a consumer needs it)

PROGRESS 2026-07-18 (cevstore lane, implementation complete; NOT closed - awaiting
review + merge by the orchestrator):

NEW FILES (no src/* or fixpoint-lib changes; reopens package CEVID in a sibling one-concern file):
  maki/competitive-evidence-store.f       durable typed codec (owns -5422..-5428)
  maki/competitive-evidence-store-test.f  acceptance suite (package CEVIDSTORE-TEST)
  registered in maki/test.f next to competitive-evidence-test.f; FILEMAP entries added.

CODEC DESIGN. Blends the two landed store idioms:
  - competitive-store.f (bench/v1) for the KEY: ENCODE derives KEY = the single canonical
    CEVID:RENDER (every field participates), wrapped in a "|schema=cevid/v1" versioned
    envelope. Row = <canonical-render>|schema=cevid/v1. The render is REUSED (no wire
    duplication) - this file owns no second render.
  - diff-case-store.f (CASESTORE) for the durable surface: file-per-row content-addressed by
    SHA-256(render) at <root>/rows/<hex>, crash-safe ATOMIC-WRITE-FILE (temp+rename,
    idempotent re-PUT), and typed load-result verdicts. Root = HABU_CEVID_STORE or tmp/cevid-store.
  NO embedded content digest: unlike competitive-store's shared append-only file (key not in
  the filename), here the SHA-256 FILENAME already commits to the content, so the canonical
  re-render IS the integrity axis - a redundant checksum would add nothing (the architecturally
  correct omission, not a shortcut).

  DECODE ( bytes -- load-result ) is the untrusted-bytes inverse (the diff-suite.f DECODE
  idiom): a throwing DECODE-RUN rebuilds the typed evidence via CEVID:ROW + the six category-
  pinned reading setters (so a wrong-category forged unit is the schema's own E-CEVID-UNIT, an
  over-cap forged value E-CEVID-CAP), then proves the stored render IS the canonical re-render
  (E-CEVIDST-CANON). The catch boundary maps schema/fields/label/token/canon + E-CEVID-UNIT/-CAP
  to `malformed` and RE-THROWS any other code (`code throw`) - IO/width/root never masquerade as
  malformed (non-masking; forth.md checked-catch discipline). LOAD ( evidence -- load-result )
  adds absent (no file) + a content-path identity check (the file at SHA-256(query-render) must
  decode to the SAME render, else malformed). Verdicts: ok<evidence>/absent/malformed (arity-0
  sum, concrete evidence payload - the diff-suite decode-result shape).

KEY COMPOSITION. store-key = SHA-256(CEVID:RENDER); file = <root>/rows/<hex(store-key)>. Every
render field participates INCLUDING cache-state, so cold vs warm is a DISTINCT durable file
(pinned by T-PATH-DISTINCT). A fresh process that rebuilds the identical evidence derives a
byte-identical render, the same SHA-256, and finds the record (T-HAS-YES / RT-PUT-* prove
fresh-handle lookup + rehydration).

ROUND-TRIP PROOFS (maki/competitive-evidence-store-test.f, all green):
  - encode golden: ENCODE(GEMM)=GEMM-GOLD, ENCODE(SAXPY)=SAXPY-GOLD (render golden from the
    sibling competitive-evidence-test.f + the fixed "|schema=cevid/v1" suffix; non-vacuous -
    a mutated 3026577->3026578 golden fails, verified in scratch).
  - in-memory round-trip: ENCODE -> DECODE -> re-ENCODE is byte-for-byte the golden (both rows).
  - DURABLE round-trip (the acceptance tying the codec to reality): PUT -> fresh-handle LOAD ->
    re-ENCODE is byte-for-byte the golden, for the migrated flagship GEMM (MMM-WIDE-B-M4-S1
    3026577 milli-GFLOP/s = 1.601x Triton) and SAXPY (SAXPY-V4 64209 milli-GB/s).
  - LOAD verdicts: absent (no file); malformed (a foreign VALID row planted at the key ->
    content-path mismatch; and structural garbage). One `malformed` forgery per DECODE axis
    (schema tag, meta schema, bad token, field count, bad label, wrong-category unit,
    over-capacity value, non-canonical spelling), each resolving against the clean base row (0).

UNITS-INTO-MEASURE DECISION (optional item 2): NOT implemented - no runtime consumer exists.
maki/experiment/run-metric.f MEASURE is ( n direction aggregation population -- report-metric )
with NO unit axis, and that file explicitly documents units as an open-vocabulary follow-up
"NOT modelled here". A repo grep found no consumer of the CEVID `unit` enum outside
maki/competitive-evidence.f (the schema). Per the schema-landing decision, the sealed enum stays
in the CEVID schema; re-signaturing MEASURE + every caller speculatively is unjustified. This
remains a documented follow-up on run-metric's own units owner, not this dot's work.

GATE TABLE (all in .jj-ws/fable-cevstore, HB_TMP=/tmp/hbtmp-cevstore, native bin/hb):
  competitive-evidence-store-test.f      EXIT 0  test: ok
  competitive-evidence/report/store + eval-triton  EXIT 0  test: ok (siblings unbroken)
  maki/test.f                            EXIT 0  test: ok
  test/gate-stdlib.f                     EXIT 0  native lint/stdlib phase PASS
  typed-local-diff-lint (jj diff --git)  EXIT 0
  dot-dep-lint                           EXIT 0  0 finding(s)
  stale-status-lint                      EXIT 0  0 finding(s)
  host-lint                              EXIT 0  0 finding(s)
  filemap-lint                           EXIT 0  0 finding(s)
  error-code-lint                        EXIT 0  0 finding(s)
  trust-lint                             EXIT 0  0 finding(s)  (zero new TRUSTED/TRUST)
  refine-lint                            EXIT 0  0 finding(s)
  trusted-inventory -- strict            EXIT 0
No src/* or fixpoint-lib changes.

REMAINING OPEN ON THE DOT: nothing substantive. Both stated deliverables are met (durable
typed cevid/v1 codec + flagship byte-equal durable round-trip). The units-into-MEASURE item is
explicitly declined for lack of a consumer (recorded above). Shared with the sibling stores:
the fsync/dir-sync power-loss durability primitive is the only remaining store-wide capability
(dotted on maki/db/commit-store.f, out of this dot's process-crash-safe scope). Dot stays active
through review + merge; do not close until the reviewed commit is merged and verified.
