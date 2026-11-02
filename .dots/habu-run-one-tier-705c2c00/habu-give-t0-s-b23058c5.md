---
title: "Give T0's compile lifecycle typed states"
status: open
priority: 2
issue-type: task
created-at: "2026-11-02T18:27:22.663341+02:00"
---

Problem: one T0 compile passes through lifecycle flags held by five owners: COMP's M-OPEN (src/habu/t0.f); the emitter seam's CUR (-1 = none selected) and PLACED-AT (-1 = unplaced, else the address) in src/habu/t0-emit.f; NEMIT's rows (open, sealed, cleared; src/compiler/session/emission.f); NCBSTUB's ARMED (src/compiler/native/callback-stub.f); NSHADOW's OPENED, PENDING, PEND-DOES (src/compiler/native/shadow.f). The legal order lives in the call sequence of SHADOWED and HOSTED and in comments ('EMIT-PLACE is legal once between EMIT-OPEN and EMIT-SEAL'); nine E-NCOMP-STATE checks guard parts of it. The Fable review of C1+C2 found one guard unreachable (PLACED? in EMIT-CB-STUB) while EMIT-PLACE twice or before EMIT-OPEN is not refused. Joel 2026-11-02: 'you would benefit from using state machines, prove me wrong'. Plan: each owner keeps its lifecycle in one typed state (an ENUM, as prof.f's phase and tape.f's mode), written only by its transition words, each refusing a call from the wrong state; values with a payload split into state plus data (PLACED-AT's -1 sentinel goes); modes that are not phases (NSHADOW's NATIVE) stay flags; counts stay data. One machine per owner, not one across them: NEMIT, NSHADOW and NCBSTUB are the session both compilers share. Order: after C1+C2 land, before C3 (C3 adds SHADOW-FIXED, another entry through the same lifecycle). Public words: none expected; any needs Joel's approval. Verify: the C2 proof rows, a refused out-of-order call per machine through the real load path, full arm64 suite and chain at landing.
