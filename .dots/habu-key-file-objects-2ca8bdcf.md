---
title: "Key file objects by hb's bytes before its signature"
status: open
priority: 2
issue-type: task
created-at: "2026-11-09T20:06:09.212641+02:00"
---

Problem: src/lib/engine-id.f KEY$ (HASH-RUNNING, ~142) hashes the running hb's whole file, its code signature included, and src/habu/linker.f KEY-SUM (~369) puts it in every file object's key. codesign writes the signing time into the signature, so two builds of the same program (gen 1 and gen 2, 0 bytes differing before the signature) share no object: on 2026-11-09 gen 2 building over gen 1's object store (/tmp/cfix.m9Uc/oB) compiled every file, 12.5 s user, as a cold build. DECISIONS 'comparing signed builds' already compares images skipping only the signature (CODESIGN:SAME-IMAGE?). Acceptance: an hb that differs from another only in its signature links every file from the other's objects; any byte before the signature still changes the key. Files: src/lib/engine-id.f (expected), src/os/macos/sign2.f for the signature's extent. Verify: build gen 2 from gen 1, then build the tree with gen 2 over gen 1's HABU_OUT and see every file link and the output equal byte for byte before the signature. Depends: none (after the arm64 landing). Ownership: carl. Claim: unassigned.
