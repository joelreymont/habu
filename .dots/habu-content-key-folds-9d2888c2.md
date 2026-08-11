---
title: CONTENT-KEY folds share one accumulator
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-10T23:27:23.707035+02:00\""
---

CONTENT-KEY:FILE+ and friends fold into a single shared accumulator (src/core/content-key.f); two interleaved folds silently mix their bytes into one key. The keyfix lane worked around it by completing its whole fold before any other key work, but nothing enforces that - a structural fix is a fold handle (accumulator value on the stack or a keyed instance) so interleaving is impossible by construction. First consumer: tools/build-fixpoint.f STAMP-KEY plus any future second CONTENT-KEY user. Files: src/core/content-key.f, tools/build-fixpoint.f. Depends: habu-pkg-the-hb-a0ca2229 (BLOCKING - the handle must be threaded, which changes ~12 helper signatures in tools/hb-build-lib.f, and that file is unpackaged so package-diff-lint rejects every body edit).

GROUNDWORK DONE (fixpkg lane 2026-08-11, all measured on master): (1) the interleaving hazard is REAL and demonstrated through the real CONTENT-KEY entry - sequential folds give keys 88a3e7f2.../bd797e8a..., the same folds interleaved give aca3706e... for BOTH; that is the must-fail-first fixture. (2) Design ruled in-lane: a THREADED handle (RESET ( -- fold ), TEXT+ ( fold ptr u8 n -- fold ), ...) - the only shape where interleaving is impossible by construction; straight-line call sites stay textually unchanged. (3) lib/content-key.f is NOT in the migrate.f chain closure (53 entries via EC:BUILD), so the refactor cannot move the stamp key by itself; still prove key byte-identical on an unchanged tree.

Claim: agent=fixpkg workspace=.jj-ws/habu-fixpoint-pkg
