---
title: Run or delete unrun tests
status: closed
priority: 2
issue-type: task
created-at: "2026-10-15T17:16:31.190136+03:00"
closed-at: "2026-10-15T20:49:15.560649+03:00"
close-reason: "Landed: unrun tests registered as gate rows (lsp-prefix, native-file-unit, native-file-ffi, aot-owned, engine-id-replace, native-branch-reloc, c2-loan-checker, c2-memory-capture, c2-memory-runtime, c2-view-record-mut, c2-unique-policy). Integrated full suite 678/678."
---

Problem: about 1,700 lines of tests that the gate never runs: c2-loan-checker-{positive,refusal,region}, c2-memory-{capture,image-build,tier1,runtime}, c2-unique-policy, c2-view-record-mut, engine-id-replace-e2e (and child), native-source-view-e2e (and children), zip-temp-source/consumer, aot-owned, compiler/native-fetch-subject, catch-success-shadow (which has no assertion), pointer-views, native-branch-reloc, native-file-forms-e2e, src/lib/unicode/alphabetic-test.f and src/lib/unicode/uppercase-test.f (both pass). The runner for gate-lint-tools-package-child.f is missing; docs/native-file-object.md:92, 96 cite native-file-unit-e2e.f (418 lines) and native-file-ffi-e2e.f (76 lines), which do not run. Acceptance: each file is registered (if it tests live behavior and passes) or deleted (if obsolete, or redundant with a registered test); docs corrected; tier-1 compiler tests deleted rather than revived. Files: those named, test/gate-stdlib-cases.f, docs/native-file-object.md. Verify: each newly registered row. Depends: none. Ownership: listed files. Claim: unassigned.
