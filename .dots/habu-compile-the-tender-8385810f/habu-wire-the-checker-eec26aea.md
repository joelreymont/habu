---
title: "Capture and restore the source owner's checker payload"
status: open
priority: 1
issue-type: task
created-at: "2026-09-13T10:53:21.380871+03:00"
blocks:
  - habu-build-engine-layout-abdd0188
  - habu-preserve-complete-addr-258c0288
  - habu-keep-a-row-f2c4f3d4
---

Plan: [PLAN.md](../../PLAN.md). Design reconciled 2026-09-13; replaces stale diagnosis/claim. Claim: review_completion_plan.

Own aot-arm.f payload arm/mark/high-waters, aot-capture.f signature/type collection, checker ASIG and type-family registry mark/restore; exclude call-row recording. Payload is required for restored checked REPL. Arm source owner and freeze membership before persistence/writer tools; restore correct registry base, close on refusal. Do not retire empty sections to hide unarmed producer. Verify explicit family/signature content after cleared-buffer read, source-free checked call/wrong-type refusal and second capture, with writer-only types excluded. Consume layout leaf's distinct membership/DATA boundaries.

Verification: focused real-load cases above; rebuild and run `bin/hb --load test/run.f` for compiler/runtime integration. Speed acceptance uses the all-AOT campaign pair; functional/count evidence can be developed in parallel.

Sequencing constraint confirmed while separating the target writer:
`CHECKER-CAPTURE-SCRATCH-PREPARE` disarms and clears ASIG stores. Freeze/export
the source owner's required payload before that preparation, then persist its
runtime registry before the final DATA bound. Current `ACAP-AUDIT-SIGS` is not
called by CAPTURE and is lexically bound to the host checker; inserting it after
preparation would still lose the source owner's rows. The owned section transfer
now preserves supplied payload bytes, but does not establish this producer seam.

Confirmed payload correctness gaps (2026-09-13):
- `CHECKER-USIG-CERT-PARSED` stores the verified graph but ASIG retained the
  original declaration text. Arming ASIG, compiling `ROW-ADD ( R -- R ) 1 +`,
  then replaying its collected `R -- R` through the ordinary declaration intake
  lets `WRONG ( ptr u8 -- ptr u8 ) ROW-ADD` compile. The reduction executes no
  underflowing call. The current renderer also drops row tails/return clauses;
  text cannot represent individual RAW kinds or inferred quotation metadata.
- `REG-AOT-LOAD` previously published each store before validating later bases
  and spans. Whole-table validation and capacity reservation must precede any
  publication.

Approved repair retains the 17-section container and 16-byte signature rows.
Each signature offset names a versioned verified graph in SIGSTR, preserving
variable identity, row kinds/quantification, quotation/return metadata and
canonical constructors. Old incompatible artifact versions must refuse. The
full native runtime instead explicitly selects its complete persisted USIG/TFAM
stores in DATA; that mode requires the captured owner and completed prefix.

The first bounded slice adds the immutable owner descriptor and explicit full
payload mode. Partial capture deliberately refuses until the verified-graph
callbacks and restore path are complete; it must never fall back to text.

Focused first-slice evidence: malformed/truncated descriptor checks pass at
both tiers. The real replacement-owner fixture passes at both tiers after
source loading and handoff, two owner CAPTURE callbacks, preserved numeric-call
acceptance/wrong-pointer-call refusal, and reopening. Full artifact restore
remains the driver integration boundary; partial graph roundtrip is unfinished.

Review handoff, 2026-09-13: the partial v8 producer, graph validator/importer,
exact registry prefix/delta identity, and owner-relative schema validation are
implemented and frozen for independent review. This is not an accepted release
milestone. Tests through a fresh source owner now cover:
- Every registry store populated by real declarations: products, wide/nested
  fields, parameters, pointers, compact/named variants, packed layout and quoted
  schemas. A/A is idempotent; same-count A/B, mixed states, late byte/count and
  semantic-reference corruption refuse with every published byte/count and both
  lookup indexes unchanged.
- A rebuilt foreign prefix with equal counts refuses, including an empty delta;
  an independently rebuilt identical prefix accepts. Per-store wire bytes carry
  the entire closed prefix plus delta; only the delta is published. No extra
  artifact section or hashing provider was added.
- A 17-section file roundtrip clears its capture buffers, rolls back the source
  registry and zeroes the retired registry rows and USIG bytes before importing. Numeric,
  nominal, quantified quotation and return-row checks retain their meanings,
  including wrong-type refusals. Returned quotations execute under JIT and
  consumed quotations compile and execute at tier 1. Six malformed graph cases
  refuse by name.
- All 46 registry exporter/validator/importer bodies compile at tier 1 and pass
  the same state tests in a fresh-owner probe. The probe uses typed JIT adapters
  for 85 pre-hook dependencies; it is not a wholly native prefix or image build.

Focused commands: `bin/hb --load test/aot-registry-identity.f`,
`bin/hb --load test/aot-payload-graph.f`,
`bin/hb --load test/aot-payload-unsupported.f`, and
`bin/hb --load test/aot-payload-admission.f`. Native probe generation and source
are `build/payload/make-registry-native-probe.py` and
`build/payload/registry-identity-native.f` in cedar-owner-payload; its log says
`registry identity and refusal atomicity: ok`, `window: 0`.

Open boundaries are explicit: exceptional quotation contracts and source catch
unsoundness (89902bde), canonical dynamic schema constructors (8262d7f3), and
native cross-call return-row effects (608449fe). Partial export/import refuse
exception-bearing quotations and non-builtin SCH-CON identities rather than
silently dropping them. The existing native compiler still refuses an ordinary
return-moving callee at -8286 and an unconsumed returned quotation at -8651.
Fresh-process native partial-artifact execution, full rebuild and full test/run.f
remain integration gates. The full persistent-owner route is separate and is
being integrated by Cedar; these partial boundaries must not be reported closed.
