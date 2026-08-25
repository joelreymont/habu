---
title: Fix snapshot trailer corruption
status: closed
priority: 1
issue-type: task
created-at: "2026-07-27T18:16:53.142846+02:00"
closed-at: "2026-08-02T15:01:22.921601+02:00"
close-reason: "Completed by reviewed hard-cut ancestor a8c716c53cda322729f8e7d5c92a406f095dc094: header-owned fixed trailer replaced the corrupt scanned-trailer path, and the obsolete OWNER-WID restore fixtures are deleted."
---

Problem: exact master c2339043 deterministically fails the standalone `owner-wid-internal` suite because the freshly generated snapshot exits 79 before restore. GDB proves the writer's trailer is internally consistent and its out-of-line dictionary-name pointers are correctly canonicalized to `RBASE-VA`; `EM-SNAPSHOT-VALIDATE-WIDS` in `src/habu/habu2.f` wrongly interprets those two pointer roles relative to live `DBASE`, rebases them outside the snapshot dictionary source, and reports `hb: snapshot trailer corrupt`.

Owned result: on exact owner-closure tip `9bb45e3f446e`, reopen its existing `ENGINE-BUILD` package once around the snapshot validator through startup, factor the current monolithic WID validator into private phases plus private `SNAP-VALIDATE-WIDS`, and keep every caller private until the chain terminates at the already-owned `EMIT-MAIN`; do not duplicate the owner stack on c233 or export a new API. Within the owner phase, invert the writer's `RBASE-VA` canonicalization at both external-name validation sites, reject decoded offsets beyond the serialized region, and require translated names to begin after `DICT-SIZE` and end within the region. Import no newer namespace-validation behavior from divergent history. Keep the injected owner-package name longer than `DNAME-INL` and make that fact load-bearing so the real snapshot child path always exercises the repaired branch. Add no compatibility path, forwarding shim, or new package.

Acceptance: the unchanged parent fails through `HABU_OWNER_WID_HARNESS=1 bin/hb --load test/owner-wid-child.f`; the candidate passes that exact production build/boot path, including an external-name pointer forged into the dictionary-record table that must exit 79 with `hb: snapshot trailer corrupt`, plus standalone `owner-wid-internal`, the full stdlib gate, native fixpoint, Maki, touched PTX slices, typed-local/package diff checks, and host-lint. The fix must preserve corruption refusals and introduce no heuristic, fallback, version, alias, or new mechanism.

Files: `src/habu/habu2.f`, `test/owner-wid-emitter.f`, `test/owner-wid-doctor.f`, `test/owner-wid-child.f` only. Depends: exact owner-closure tip `9bb45e3f446e`; publish atomically with its trust-deletion prerequisite. Ownership: engine-build snapshot restore/startup, loader coordinate inversion, and its DNAME-EXT production fixtures.

Claim: agent=snapshot_trailer_fix workspace=.jj-ws/habu-fix-snapshot-trailer-9e9d70db
