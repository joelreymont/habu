---
title: Measure, then drop, the change-time shortcut
status: closed
priority: 2
issue-type: task
created-at: "2026-10-31T22:17:38.403958+02:00"
closed-at: "2026-11-03T12:30:38.700465+02:00"
---

Problem: tools/native-file-inputs.f vouches for a file's own source by its change time and size (SOURCE-ROW stamp, STORE-OWN, STAMP-FILE, STORE-CTIME, ADD-CTIME; native-definition-cache.f FILE-INPUT) so a build re-hashes only files whose stamp moved. Joel 2026-10-31: 'use hashes everywhere if that's best for long term'. Acceptance: measure a no-edit warm engine build's wall and CPU with the stamp and with every source hashed; delete the stamp machinery so a source row holds only its SHA-256, unless hashing every source measurably slows that build; record both figures here either way. Files: tools/native-file-inputs.f, tools/native-definition-cache.f. Verify: no-edit warm build before/after, test/native-file-replay-e2e.f, test/native-file-storage-e2e.f. Depends: habu-record-a-hash-85564705 lands first (same files). Claim: agent=carl worker workspace=.jj-ws.noindex/carl-ctime brief=carl-dots/.out/carl-file/brief-hash-inputs.md (critic first; removing NFILE-INPUTS:ADD-CTIME needs Joel's approval with the measurement before landing).

Result (spark, Linux arm64, idle; same engine per side; base = stat shortcut, chg = every input hashed):
- no-edit warm engine build, 3 runs: base 16.52/16.43/16.44 s wall, 15.62/15.53/15.49 s user; chg 16.70/17.03/17.00 s wall, 15.82/16.01/16.05 s user (169 reused, 0 compiled on both).
- cold engine build: base 122.39 s, chg 120.64 s.
- no-edit warm `hb-build --repl hello.f`, 3 runs: base 3.37/3.40/3.37 s; chg 3.90/3.89/3.84 s. Cold: 13.00 vs 13.42 s.
- repeated CHECK-VERIFY-IMAGE:PATH$ in one process (the LSP's call): base 0.45 ms per call; chg 108 ms (re-hashes the 6.8 MB engine and the verifier closure).
Hashing every input measurably slows the build, so the stat shortcut stays (the dot's acceptance rule); NFILE-INPUTS:ADD-CTIME and the row format are unchanged. The reason is a comment at FILE-ROW in tools/native-file-inputs.f.
