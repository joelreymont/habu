---
title: Migrate maki raw family-handle pools to converters
status: closed
priority: 2
issue-type: task
created-at: "\"2026-07-15T13:04:11.275326+02:00\""
close-reason: "Landed as 488a4937b5a7."
---

PREREQUISITE for the native RAW-definer seal (habu-register-native-repl-f12807aa), found by the engmirror lane 2026-07-15: maki idiomatically stores nominal family handles (tensor<>, mir:ref-pos<>, node ids, ...) directly into raw create pools (P-INS/P-OUT, MI-INS/MI-INOFF, and recurring across artifact/async-dag/fusion-plan/typestate/model-ir files), bypassing each family's sanctioned TRUSTED: raw<->family authority (RAW>TENSOR/TENSOR>RAW etc.). The checker's RAW-BLOCK? rejects family<->raw-cell binding in both directions, so the proven engine seal (patch preserved by the lane; design: LSIGRAWDEF label + s" sig-raw-definer!" baked string + C-EMIT-SIG-RAW-CALL bracketing C-CALL-TRUST-LASTC-PTR-A/-A inside the HOOK-CELL guard, mirroring verify-source RAW-TRUST-NEXT) breaks maki/test.f pervasively (tensor-value test 3, model-ir test 26, onward). verify-source applies the identical seal, so maki is latently incompatible there too - masked only because maki is checked via --load. Fix: route EVERY maki create-pool store/read of a family handle through the family's >RAW/RAW> converters (proven shape: tensor-value 2->25 tests passing), file by file; overlaps habu-nominal-storage-migrate-47ee0f93's TYPED-VARIABLE migration for its four files (coordinate - the typed-definer route is strictly better where applicable; converters for the rest). Acceptance: maki/test.f green WITH the seal patch applied on a scratch engine; then f12807aa re-applies its patch and lands. Files: maki/tensor-value.f, model-ir.f, artifact.f, async-dag.f, fusion-plan.f, typestate.f + siblings with raw pools, their tests. Verify: maki suite under the sealed scratch engine, full run.f. Ownership: maki storage idiom. NOTE: maki/lower-launch.f + fusion-plan may touch sol's region territory - check claims at dispatch.


UNFENCED MIGRATION LANDED 2026-07-15 (makipools worker, "Migrate maki
family-handle pools to typed storage", merged 488a4937): 8 files migrated
(tensor-value P-OUT/P-INS -> TYPED-BUFFER; model-ir MI-INOFF/MI-INS + rows/
cols + pend-off; traffic TRF-SRC; target TGT-SM87; backward BW-CT/ISG/SEED;
onnx/import 5 pools; checkpoint-test slot lifting; sched-key-test KT-STALE-R);
artifact/async-dag/typestate audited CLEAN. Seal-compatibility PROVEN: a
sealed scratch engine (re-derived f12807aa design, byte-identical fixpoint)
runs maki/test.f 108/0 with a throwaway-migrated overlay of the fenced files;
the workspace tree blocks exactly at the first fenced site (fp-sp-node!).
REMAINING (why this dot stays open, gated on sol's region-lower lane): six
fenced files with single-family pools, exact TYPED-BUFFER fixes derived and
proven: fusion-plan FP-SP-NODE (CAD-KIND:node-id), lower-launch LLA-IN-REF +
lower-ew LEW-INS + lower-red LRED-INS + lower-mm LMM-INS + lower-move LMV-INS
(all MIR:operand-ref). Hand to the region-lower owner or reassign when that
claim releases; then f12807aa re-applies its engine patch.
