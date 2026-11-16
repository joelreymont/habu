---
title: Write x86 dictionary and policy rejections
status: active
priority: 2
issue-type: task
created-at: "2026-11-17T10:44:57.090650+01:00"
---

Problem: in src/habu/kernel-x64.f, POLICY-ADMIT-BODY keeps an x86-only 'keyword check unavailable' exit; NDICT-APPEND-BODY's bad exit and RECORD-FACTS-BODY's SEAL-TRAP-LBL jumps exit without the die line arm64 writes for the row; the index exits, DOES-UNCREATED and REPLAY-RECORD still print 'E-X habu:' texts; test/x86-64-kernel-policy.f's header names the old first lines. Acceptance: no x86 exit prints an 'hb: ' or 'E-X habu:' line arm64 does not, and the suite tests that reach them pass on x86. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-xr6.
