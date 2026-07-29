---
title: Record owner construction flag
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T20:53:42.492055+02:00"
---

Problem: every public product currently publishes MAKE, so a structure cannot be publicly readable while construction remains package-owned. Result: reserve value 4 as DRV-CONSTRUCT-OWNER in the existing TF.DERIVE word at offset 18 of the unchanged 19-cell family row. Add protected declaration-time TFAM-CONSTRUCT-OWNER! ( fam -- ) and committed read-only TFAM-CONSTRUCT-OWNER? ( fam -- bool ). Absence retains ordinary public construction; presence means only the original declaring package may construct the product. Change TFAM-DERIVE-ANY? to mask only DRV-EQ or DRV-HASH so the owner flag cannot enter sum/enum derived-operation validation or generation. Keep every row offset, ordinary image format, and existing declaration rollback unchanged. Add no public/owner pair, generic flag setter, setter after commit, side table, owner identifier, version, migration record, or runtime state. Owner: src/core/type-family.f existing derive metadata only. Production red: MDLCFG:mcfg must carry a private proof field because its public MAKE cannot otherwise be suppressed. Acceptance: the flag commits/replays and rolls back through the existing family row; owner-only products generate no equality/hash operations unless those exact flags are also set; unknown and post-publication mutations reject; all prior unflagged products remain byte-identical; type-family, type-family rollback, generated-declaration transaction, native fixpoint, and exact diff gates pass. Claim: unassigned.
