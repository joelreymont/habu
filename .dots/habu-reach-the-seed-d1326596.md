---
title: "Reach the seed's payloads past the ADR field"
status: open
priority: 2
issue-type: task
created-at: "2026-08-14T16:38:14.182403+02:00"
---

PRECONDITION of the chain bake (e98b03d4 ruling): EM-AOT-REGISTER-RECS/PATCH-SITES/VALIDATE reach LAOTDICT/SITES/NAMES/DATA with ADR, (+-1MB, icode.f:246), and EMIT-AOT-SEED lays the blob BEFORE them - at chain scale (1.2MB blob + 1.5MB window DATA = 2.7MB of payloads) the image writer refuses icode: adr out of reach at ~1MB. The 2MB AOT-BLOB-CAP is DEAD behind this - the stated cap was never the binding bound. Fix: reorder the section (payloads last) AND a far-address form for whatever still lands past 1MB (ADRP+ADD is the architectural answer; if a new instruction form enters the emitter it needs Rocq rows in formal/Common/Insn.v per the CG-02 discipline, enc/wf/roundtrip, before the emitter uses it). Two-sided reach fixtures per the derived-bound discipline. Files: src/arch/arm64/icode.f, src/habu/habu2.f (the seed emitters). Depends: none; blocks e98b03d4 implementation.
