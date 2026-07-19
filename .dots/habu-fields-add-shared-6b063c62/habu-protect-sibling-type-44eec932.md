---
title: Protect sibling type-registry states via seal
status: active
priority: 2
issue-type: task
created-at: "2026-07-18T18:12:28.426954+02:00"
---

Protect the six sibling type registries that are as exposed as PF-COMMIT-N was before the TYPE-FIELD seal landed: TFAM-N and SUMV-N (type-family.f:1342 TFAM-RESET writes; arenas), TF-STR-U / TF-PK-N (string + param pools), LAY-N (type-family.f:1257-1260 LAY-A-P/LAY-A-BOOT), SCH-N / SCH-ROOT-N (src/core/type-schema.f). Each has the same live bare-write vector as the confirmed `99 PF-COMMIT-N !` exploit and the same cross-file consumer pins (TDECL-MARK/RESTORE snapshots all of them at sumtype.f:62-70; checker.f rollback frames).

Mechanism (corrected 2026-07-19 by the orchestrator; the memory-band design this dot originally referenced was abandoned with the closed parent dot habu-protect-type-field-04d91409): apply the pattern that actually landed for the TYPE-FIELD registry in src/core/type-family.f:784-806. (1) Tag each sibling registry cell set with REG-PROTECT at its definition site and let IMK-SEAL-REGISTRY (internal-mark) seal them, so bare interpret-mode writes and bare ticks fail closed with rc 70. (2) Keep declaration-time writes as in-package colon builders; where a cross-package site must legitimately write, use the same narrow explicit package-reopen wrapper the TYPE-FIELD landing used rather than exporting the cell. (3) Add per-registry bare-write negatives in test/internal-word-gate.f mirroring the existing PF-COMMIT-N case. (4) The seal only has to hold against checked Habu; 0 set-check / patch32 forges are out of the threat model. Full fixpoint x2 plus the gate matrix per registry batch (seed-affecting: the seal list changes the engine image).

Claim: agent=sibreg-opus workspace=.jj-ws/habu-protect-sibling-type-44eec932
