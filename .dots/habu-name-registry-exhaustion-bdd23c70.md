---
title: Name registry exhaustion in the enum diagnostic
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-01T16:06:24.410731+02:00\""
---

E-PROTECTION-CAP (7169) has no entry in src/core/generated-declaration.f:180's reason table, so protected-registry exhaustion reports the generic 'declaration failed' naming an innocent enum - the maki red was misattributed for two days because of this. Add a protection reason group mapping 7169 to 'the protected-wordlist registry is full'. Baked into the cold prefix (bootstrap/cg/forth.fs:1983) so it rides the same rebuild as the bitmap dot. Depends on habu-replace-the-protected (rides the same seed rebuild).

Claim: agent=makilane workspace=.jj-ws/habu-fix-maki-competitive-7dc29ec2
