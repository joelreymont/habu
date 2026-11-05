---
title: "Give a native shadow T0's unplaced emission"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-05T14:13:30.233125+02:00"
closed-at: "2026-11-05T15:38:03.043161+02:00"
close-reason: "Landed in 'Give a native shadow T0's unplaced emission': under a native shadow HOSTED seals the host walk unplaced into the shadow; COPY rewrites only placed (tier-1 x86-64) emissions; real CALLBACK: cases under both shadows in test/compiler/shadow.f and shadow-kinds.f. Candidate gen1 = gen2 bar the signature; compiler-shadow, aot-shadow-capture, x86-64-link-records PASS on gen2 at master 94bc4127."
---

Problem: OPEN-NATIVE (src/compiler/native/shadow.f:311) throws unless a backend installed NATIVE-ADDRS-XT, which only x86-64 tier 1 does (src/arch/x86-64/passes.f:285); COPY's NATIVE-CALLS (:164) rewrites x86 E8/E9 only, so arm64 has no native shadow (step 5 S1, design r2 3.5). Approach (lead): under a native shadow T0's HOSTED (src/habu/t0.f:1929) seals the host walk unplaced into the shadow before EMIT-PLACE (private COMP word EMIT-SEAL-UNPLACED in t0-emit.f), the form a non-native shadow already takes, so no machine needs an unplacer; COPY runs NATIVE-CALLS/NATIVE-ADDRS-XT only for a placed emission (tier 1 x86-64) and refuses one without the installer; SOURCE-FUNS pairs the shadow's functions with the host's less the callback stub NCBSTUB appends. Acceptance: on arm64 under the engine's own binding a tier-0 definition files one record and one emission whose bytes equal a non-native AArch64 shadow's; a CALLBACK: definition publishes under the native shadow. Not here: fixed/cast/does> bodies (S1b), a stub in the shadow routine (the product linker). Files: src/habu/t0.f, src/habu/t0-emit.f, src/compiler/native/shadow.f, test/compiler/shadow.f. Verify: arm64 build, probe, suites compiler-shadow aot-shadow-capture x86-64-link-records (baseline green on master e77bb07d). Brief: /tmp/jerry-big/brief-s1a.md. Depends: none. Ownership: those files. Claim: agent=jerry worker workspace=.jj-ws.noindex/jerry-s1a
