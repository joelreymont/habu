---
title: Record a hash for each file built into hb
status: active
priority: 2
issue-type: task
created-at: "2026-10-31T22:17:32.203474+02:00"
---

Problem: an app build cannot tell which files built into hb changed (the require registry keeps paths only; SOURCE-PRODUCER clears read history at capture), so any hb change recompiles every app file. Joel 2026-10-31 (LESSONS 'files built into hb'): record each built-in file's path and SHA-256 in the image, word SOURCE-PRODUCER:FILE-HASH; hashes everywhere; track dependencies. Acceptance: the engine build records, per file compiled through the file cache, a SHA-256 over its own bytes, the recorded hashes of the built-in files it required (loaded or already loaded) and other files it read; the image carries the table (no stored address, no cap); FILE-HASH ( ptr u8 n -- ptr u8 n bool ) answers it; a debug-watch.f comment edit changes debug-watch.f's, stepper.f's and debug.f's hashes and not memory.f's. Files: src/core/source-producer.f, tools/native-definition-cache.f, tools/native-build-core.f. Verify: brief carl-dots/.out/carl-file/brief-file-hash.md Proof. Depends: none. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-fhash.
