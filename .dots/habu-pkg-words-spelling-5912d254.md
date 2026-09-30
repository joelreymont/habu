---
title: Package words spelling op rows differ by tier
status: open
priority: 2
issue-type: task
created-at: "2026-09-30T18:14:00.939171+02:00"
---

Found by the Fable review of the sealed vocabulary (habu-load-authored-src-4ef714a3), 2026-09-30, as: a package word spelled like a JIT op row means the op at tier 0 and the package word at tier 1.

Re-scoped on evidence (heron, 2026-10-01), after "Bind shadowed spellings to their scope":
- The package-word case is fixed. The JIT's C-SCOPED-SKIP keeps op rows off scoped records, and test/reopen-binding.f gives the same result at both tiers: RE-DUP 6, RE-SUM 2, RE-SUM-LOCALS 2, a private swap `0 103`, MR:2dup 6.
- `min` is not an op row, and a package MIN was already consistent.
- A global `: dup` is refused as `duplicate definition: dup` (rc 78) on both engines.
- The review's recommendation, reserving op-row spellings engine-wide, is rejected: docs/forth.md states that package operator spellings are legal and bind to their scope.

Remaining defect: redefining an op-row word globally after `undefine`. For `undefine dup`, `: dup ( n -- n ) 100 + ;` and `: T ( -- n ) 5 dup ;`, the checker certifies T against the user's dup. Tier 0 still inlines the engine op: `T . depth .` prints `5 1`, a certified stack leak. Tier 1 refuses with an unnamed -8303 ("ncomp: cannot compile T", rc 67). With `: dup ( -- n ) 5 ;`, `1 dup` prints `1 1` at both tiers, against the certificate. Both results are the same on the engines before and after the scope fix.

Acceptance: an op row applies only when the shared lookup returns that op's own engine record, at both tiers. The two programs above print `105 0` and `5 1` at both tiers, pinned by rows that are red on the old engine. This is part of the single-resolver work under habu-share-reopen-name-92885254 (change xonwltsq).
