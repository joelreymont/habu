---
title: Make driver-io.f and aot-closure.f require what they use
status: open
priority: 2
issue-type: task
created-at: "2026-11-03T19:59:26.362176+02:00"
---

Problem: src/habu/driver-io.f calls FDIO:WALL (:19) without requiring src/habu/fdio.f; its header and fdio.f's say every builder loads fdio.f just before it (tools/bootstrap.sh:140, tools/build-fixpoint.f:629 BF-APPEND-FDIO, src/habu/aot-link-arm.f:43). On 2026-11-03 dropping aot-link-arm.f's fdio require turned stdlib-process-fixtures red (tools/standalone-load-test.f, src/habu/aot-lib.f standalone: E-UNDEFINED FDIO:WALL at src/habu/driver-io.f:19), so the require was restored. src/habu/aot-closure.f uses LE:U32@ (SCAN-X64-SITE) and gets LE only through sites.f's require of src/lib/le.f. The card (docs/forth-card.md section 7) says every file requires its own dependencies. Acceptance: driver-io.f requires fdio.f and aot-closure.f requires src/lib/le.f; the builders that place fdio.f by hand (bootstrap.sh's list, build-fixpoint BF-APPEND-FDIO, aot-link-arm.f:43) either keep working with the require or drop their own placement; the two headers stop saying 'loaded just before'. Files: src/habu/driver-io.f, src/habu/fdio.f, src/habu/aot-closure.f, src/habu/aot-link-arm.f, tools/build-fixpoint.f, tools/bootstrap.sh. Verify: bin/hb --load tools/standalone-load-test.f; the stdlib-process-fixtures row; tools/two-generation-build.f; a Gforth recovery run (tools/bootstrap.sh), since the recovery list places these files. Depends: none. Ownership: src/habu/driver-io.f. Claim: unassigned.
