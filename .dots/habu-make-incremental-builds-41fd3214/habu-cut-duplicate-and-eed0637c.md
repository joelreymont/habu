---
title: Cut duplicate and mirror build-cache tests
status: open
priority: 2
issue-type: task
created-at: "2026-10-22T12:17:10.246770+03:00"
---

Problem: (a) test/source-root-exe-test.f:623-668 spawns two children that each compile src/habu/app-image.f from source (SAVE-PUSHED and CAPTURE are one boundary; one child can do both). (b) Format mirrors: test/native-file-types-e2e.f PARTIAL-PROJECTION :30-101 (hand-assembled rows), test/native-file-auto-e2e.f:532 HBFDEP02 magic scan, tools/hb-build-test-lib.f CHECK-REPORT :527-529 version pin. (c) Duplicates: tools/hb-build-retain-test.f:252-291 vs src/lib/build-cache-retain-test.f:352-370 basic rules; test/native-file-stripped-path-e2e.f:48-53 cold/warm pair; three 1 ms deadline cases (test/compiler/native-build-edit-e2e.f:936-942, test/whitebox-engine-key-test.f:142-146, tools/hb-build-timeout-test.f; keep the CLI one). Acceptance: each removed or merged with its distinct claim kept; affected rows pass; row wall/CPU before and after. Depends: none. Claim: unassigned.
