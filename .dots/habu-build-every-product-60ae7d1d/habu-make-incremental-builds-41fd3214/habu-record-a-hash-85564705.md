---
title: Record a hash for each file built into hb
status: closed
priority: 2
issue-type: task
created-at: "2026-10-31T22:17:32.203474+02:00"
closed-at: "2026-11-03T10:05:00.677669+02:00"
close-reason: "landed in master 92594f9e (stack 32e8348b..92594f9e) and installed; full suite on the stack's two-stage hb: 516 of 517 rows green, native-build-recompile PASS 1896 s, native-file-dynamic-load PASS; the one red, lsp-boundary, was tools/lsp-entry-test.f's 30 s child deadline at load 138 and passes alone (13 s on the installed and the batch-2 hb alike); two-generation check converged (gen 5 = gen 4), rc 0"
---

Problem: an app build cannot tell which files built into hb changed (the require registry keeps paths only; SOURCE-PRODUCER clears read history at capture), so any hb change recompiles every app file. Joel 2026-10-31 (LESSONS 'files built into hb'): record each built-in file's path and SHA-256 in the image, word SOURCE-PRODUCER:FILE-HASH; hashes everywhere; track dependencies. Acceptance: the engine build records, per file compiled through the file cache, a SHA-256 over its own bytes, the recorded hashes of the built-in files it required (loaded or already loaded) and other files it read; the image carries the table (no stored address, no cap); FILE-HASH ( ptr u8 n -- ptr u8 n bool ) answers it; a debug-watch.f comment edit changes debug-watch.f's, stepper.f's and debug.f's hashes and not memory.f's. Files: src/core/source-producer.f, tools/native-definition-cache.f, tools/native-build-core.f. Verify: brief carl-dots/.out/carl-file/brief-file-hash.md Proof. Depends: none. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-fhash.
