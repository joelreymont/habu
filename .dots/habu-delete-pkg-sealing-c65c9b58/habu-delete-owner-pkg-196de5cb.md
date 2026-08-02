---
title: Delete owner package seal emitter
status: closed
priority: 1
issue-type: task
created-at: "2026-07-30T22:10:39.927508+02:00"
closed-at: "2026-08-02T15:17:47.105146+02:00"
close-reason: "Completed by the a8c716c5 hard cut: OWNER-WID package/seal machinery and its load/inventory rows were deleted; no owner mechanism remains."
---

Delete obsolete src/habu/owner-wid-emit-seal.f and remove only its exact load or inventory rows from tools/bootstrap.sh, tools/build-fixpoint.f, tools/hb-build-lib.f, tools/srclist.f, tools/image-bytes-test.f, tools/bootstrap-codegen-test.f, and test/run-files.f. In tools/image-bytes-test.f, the removed assertion is the final row of a legacy global definition; hard-cut only that changed boundary into package IMAGE-BYTES-TEST with private SOURCE-SHAPE, public RUN, and the top-level IMAGE-BYTES-TEST:RUN call. Leave every unchanged legacy helper global untouched and add no alias. Retain adjacent layout-buffer-seal.f and lower-cert-seal.f rows unchanged. Acceptance before M17: exact source/load census has no owner-wid-emit-seal reference, every edited owning file still loads through bin/hb where applicable, and exact typed-local/package/trust diff gates pass. No suite, generated artifact, broad load-list rewrite, host logic, compatibility, lint, docs, or unrelated seal deletion. Claim: agent=seal_emitter_impl workspace=.jj-ws/habu-delete-owner-pkg-196de5cb.
