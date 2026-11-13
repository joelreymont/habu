---
title: Drop the constructor-wordlist protection
status: closed
priority: 2
issue-type: task
created-at: "2026-11-06T13:19:27.285684+02:00"
closed-at: "2026-11-06T17:03:25.805795+02:00"
close-reason: "Landed as 97ee2eea on A1 7b497647, under A2a 3bfcbf1c, master 7bafa920. Proof: the stack's chain run (fixpoint at generation 2, rc 0) and full suite (396 of 397; the red check-cli-boundary is a load timeout that passes alone)."
---

Problem: Joel's ruling of 2026-11-03 deletes everything that only builds or reads family namespaces, E-PROTECTION-CAP included; the protection owner (src/core/generated-declaration-protection.f:8-142), the TDECL-PROT-WID-XT hook (src/core/sumtype.f:1047-1067, :1758-1766) and xref.f's PROT-WID-CTOR-ADD (:396-408) still protect a public family's constructor wordlist with prot-wid-add, which the sealing decision (2026-11-06) also retires. Acceptance: those go; the dictionary preflight and the snapshot-reset chain move into DECLARATION-TRANSACTION in the same file; the tests of the protection go. Files: src/core/generated-declaration-protection.f, src/core/sumtype.f, src/habu/xref.f, test/seal.f, test/type-ctor-suite.f, test/bootstrap-wide-{tick,memory,interpret}-src.f. Verify: workspace candidate build, then rows friend-arena-seal type-ctor and the declaration suites. Depends: none. Ownership: those files. Claim: agent=worker workspace=.jj-ws.noindex/jerry-ctor
