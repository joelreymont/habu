---
title: Delete owner package seal emitter
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T22:10:39.927508+02:00"
---

Delete obsolete src/habu/owner-wid-emit-seal.f and remove only its exact load or inventory rows from tools/bootstrap.sh, tools/build-fixpoint.f, tools/hb-build-lib.f, tools/srclist.f, tools/image-bytes-test.f, tools/bootstrap-codegen-test.f, and test/run-files.f. Retain adjacent layout-buffer-seal.f and lower-cert-seal.f rows unchanged. Acceptance before M17: exact source/load census has no owner-wid-emit-seal reference, every edited owning file still loads through bin/hb where applicable, and exact typed-local/package/trust diff gates pass. No suite, generated artifact, broad load-list rewrite, host logic, compatibility, lint, docs, or unrelated seal deletion.
