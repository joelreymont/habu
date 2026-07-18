---
title: Protect sibling type-registry states via band
status: open
priority: 2
issue-type: task
created-at: "2026-07-18T18:12:28.426954+02:00"
blocks:
  - habu-protect-type-field-04d91409
---

Apply the re-blessed TYPE-FIELD friend-band protection (habu-protect-type-field-04d91409 Design C) to the six equally-exposed sibling registries: TFAM-N and SUMV-N (type-family.f:1342 TFAM-RESET writes; arenas), TF-STR-U / TF-PK-N (string + param pools), LAY-N (type-family.f:1257-1260 LAY-A-P/LAY-A-BOOT), SCH-N / SCH-ROOT-N (src/core/type-schema.f). Each has the same live bare-write vector as 99 PF-COMMIT-N ! and the same cross-file consumer pins (TDECL-MARK/RESTORE snapshots all of them at sumtype.f:62-70; checker.f rollback frames). Place each registry cell set in the PROT-GUARD band and route its declaration-time writes through the same internal-marked bracket; add per-registry bare-write negatives in test/internal-word-gate.f; full fixpoint + gate matrix per registry batch. Blocked by the TYPE-FIELD band prototype landing (this dot inherits its mechanism; do not start before the prototype is reviewed).
