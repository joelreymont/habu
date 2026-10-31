---
title: Measure, then drop, the change-time shortcut
status: open
priority: 2
issue-type: task
created-at: "2026-10-31T22:17:38.403958+02:00"
---

Problem: tools/native-file-inputs.f vouches for a file's own source by its change time and size (SOURCE-ROW stamp, STORE-OWN, STAMP-FILE, STORE-CTIME, ADD-CTIME; native-definition-cache.f FILE-INPUT) so a build re-hashes only files whose stamp moved. Joel 2026-10-31: 'use hashes everywhere if that's best for long term'. Acceptance: measure a no-edit warm engine build's wall and CPU with the stamp and with every source hashed; delete the stamp machinery so a source row holds only its SHA-256, unless hashing every source measurably slows that build; record both figures here either way. Files: tools/native-file-inputs.f, tools/native-definition-cache.f. Verify: no-edit warm build before/after, test/native-file-replay-e2e.f, test/native-file-storage-e2e.f. Depends: habu-record-a-hash-85564705 lands first (same files). Claim: unassigned.
