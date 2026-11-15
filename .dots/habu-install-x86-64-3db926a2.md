---
title: Install x86-64 shadow addresses only on an x86-64 host
status: open
priority: 2
issue-type: task
created-at: "2026-11-05T14:55:33.750466+02:00"
---

Problem: X64PASS:INSTALL (src/arch/x86-64/passes.f:289, the store at :285) installs SHADOW-ADDRS as NSHADOW:NATIVE-ADDRS-XT on any host. On arm64 with the x86-64 passes loaded, a tier-1 compile under a native (arm64) shadow runs COPY's x86 E8/E9 call rewrite (src/compiler/native/shadow.f NATIVE-CALLS) over arm64 bytes. Found in the S1a review (dot habu-give-a-native-87179989), older than it. Acceptance: the installer is chosen by the host the emission is placed for, so arm64 placed emissions never meet the x86 rewrite; a tier-1 compile under an arm64 native shadow with the x86-64 passes loaded files bytes equal to one without them. Waits for Joel's ruling that x86-64 gets no work until arm64-to-arm64 builds (2026-11-05), and for tier 1's fate: tier 1 changes only to fix a defect on a path it still owns. Files: src/arch/x86-64/passes.f, src/compiler/native/shadow.f. Verify: compiler-shadow. Depends: none. Ownership: those files. Claim: unassigned.
