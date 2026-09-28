---
title: Measure remaining ARM64 selection gaps
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-09-28T17:42:50.994148+02:00\\\"\""
closed-at: "2026-09-28T18:14:03.225345+02:00"
close-reason: Completed exact owned-code census and independent source design. 4449 STP+move sites/17796 gross bytes;595mixedloads/2380;124unsigned addressedloads/496;2FPRpairs/8. Counts overlap and do not prove transient IR provenance or netproduct savings. Lead reviewed receipt/source design and verified final log hash. Implementation commissioned separately as habu-fuse-paired-stack-c1800302. Receipt ~/.cache/tmp/habu-selection-gap-census-completion-20260928-01.md. No dependency edges require removal.
---

User asks which similar easy optimizations were missed, while pursuing 1 MB or more additional engine savings. Current source confirms ordinary addressed memory uses offset zero, new GPR64 pairs have no writeback, and floating pairs are absent. Scalar writeback, frame fusion and MADD already exist; prior constant-shift and boolean candidates grew payload. Measure owned-code instances on exact paired engine with control-target/liveness/provenance limits stated; establish safe selection seams and complete compiler cost before implementing a winner. No opcode census is itself proof of eligibility or achieved savings. Acceptance: repeatable ranked census and independent source design, identifying already-supported forms and semantic blockers.
