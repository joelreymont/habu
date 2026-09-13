---
title: Retire the S-PWID buffer readers left by the bitmap capture
status: open
priority: 2
issue-type: task
created-at: "2026-09-13T09:13:20.984822+03:00"
---

Problem: the artifact format's S-PWID section has no buffer since the protected-WID capture became a live bitmap (3e29a730), and the section is already SKIP-SECTION on merge (src/habu/aot-file.f:1099), yet src/habu/aot-file.f:244 (SEC-PTR) and tools/aot-chain-capture.f:335 still call AOT-PWID-BUF@, which no longer exists, and tools/aot-chain-capture.f also dies earlier on E-UNDEFINED: AOT-ARM:SIG-CLOSE; measured 2026-09-12 by the protected-WID fixture lane. Neither path is in test/run.f, which is why the residue survived. Acceptance: the format owner's decision recorded (drop S-PWID from the format with a version note, or give it a live producer) and implemented; every reader resolves; tools/aot-chain-capture.f runs end to end the way its header says on a cold-built engine and its assertions state the current capture (the capture-window lane changes which below-window rows travel); a check that loads aot-file.f and aot-chain-capture.f through the real load path so an undefined word there fails a suite. Files: src/habu/aot-file.f, tools/aot-chain-capture.f, docs/. Verify: the tool end to end, test/run.f. Depends: habu-keep-declared-addr-dbd7d8d9. Ownership: hazel. Claim: unassigned.
