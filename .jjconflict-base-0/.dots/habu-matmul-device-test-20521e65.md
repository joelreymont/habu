---
title: matmul-device-test undefined ED-* + uniform golden
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T11:09:34.639255+02:00"
---

tools/ptx/matmul-device-test.f references ED-LIB/ED-H/ED-SYM which are DEFINED NOWHERE in the tree (grep: only used here + a comment in maki/device-smoke.f). Loading it gives E-UNDEFINED: ED-LIB (exit 70) - the file is pre-existing broken and cannot run. Fix: define the ED-* launch idiom it needs (dlopen handle ED-H/ED-LIB + ED-SYM resolver + CALL* - the same shape as cuda-launch.f LL-* / acc-device-test AD-*) either inline or via a shared checked launch helper. SEPARATELY, its golden is uniform: A=B=all-ones memset -> C[0][0]=K=64.0, which cannot catch an A/B transpose or a wrong-stride index bug (all-ones is transpose-invariant). Make A nonuniform via HtoD of a computed 64x64 host array (e.g. A[i][k]=k+1, B=ones -> C[0][0]=sum_{k=0..63}(k+1)=2080.0=0x45020000; a transpose reading A[k][0]=1 gives 64.0 != 2080.0). Sentinel readback (MM-RB FILL/GUARD) already added but unverifiable until ED-* is fixed. Device-blocked to run (Orin).
