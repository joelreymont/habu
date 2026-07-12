---
title: "Checker: seal owner packages"
status: open
priority: 2
issue-type: task
created-at: "\"2026-07-12T15:23:17.613639+02:00\""
blocks:
  - habu-seal-owners-migrate-2dda16df
---

Static invariant: an owner package that contains private nominal refinements or mutable registry state must be permanently closed after assembly; later user source must not reopen it, publish qualified tails, tick/postpone private words, or mutate private cells. Current docs/forth.md intentionally permits ordinary package reopening, while only reserved system packages and generated constructor WIDs are protected; hostile probes reopen TARGET/TOOLCHAIN/MAKI and expose RAW>ID/RAW>TC/RAW>REGION. Root fix: add a checked declarative package-seal capability (for example ;SEALED-PACKAGE or SEAL-PACKAGE) that registers both public/private owner WIDs in the protected registry transactionally, persists through AOT/snapshot/fixpoint, rejects reopen/qualified definition/undefine/tick/bracket-tick/postpone/export with E-SEAL-PACKAGE, and cannot be invoked after authority sealing except by the package being assembled. Acceptance: ordinary packages remain reopenable; sealed owner positive APIs remain callable; hostile reopen and all qualified sinks reject on --load and stdin paths; rollback cannot leak protected WIDs; duplicate/capacity failures are transactional; TARGET, TOOLCHAIN, fusion region, artifact/evidence/store owners adopt the seal; docs and exact native/bootstrap/AOT tests updated. Files: package compiler/protected-WID registry, checker/package declaration grammar, AOT capture/restore, test/seal-package.f, docs/forth.md, owner files. Verify native fixpoint, seal/package/export suites, bootstrap parity, maki/test.f, full test/run.f.
