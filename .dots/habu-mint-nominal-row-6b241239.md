---
title: Mint nominal row-index types for owner/prot WID registries
status: closed
priority: 3
issue-type: task
created-at: "2026-07-19T02:33:56.747382+02:00"
closed-at: "2026-08-02T15:01:23.614168+02:00"
close-reason: Wholly obsolete after reviewed hard-cut ancestor a8c716c53cda322729f8e7d5c92a406f095dc094 deleted OWNER-WID row storage and indices; no nominal row type is needed.
---

Problem: src/habu/aot-capture.f addresses owner-WID and protected-WID registry rows with bare n indices (idx OWNER-WID-ROW * at :360, idx 4 * at :339); the checker cannot tell an owner-row index from a prot-row index, so a swap is not rejected (same-type semantic-role gap, docs/forth.md 'Same-cell values need nominal roles'). test/owner-wid-role-swap.f is a toy NOMINAL: stand-in (package-scoped OWNER-ROW-IDX / PROT-ROW-IDX) proving the checker mechanism only; the real rows share type n. Acceptance: mint NOMINAL: OWNER-ROW-IDX and PROT-ROW-IDX (deftype was retired onto NOMINAL: by habu-retire-deftype-onto-07227854), thread them through the registry accessors, and add a negative checked regression using the REAL row types (dropping the toy fixture's stand-in note). Files: src/habu/aot-capture.f, src/habu/habu1.f row math, test/owner-wid-role-swap.f. Verify: the negative fixture rejects rc 70 through the owner-wid suite; test/gate-stdlib.f. Depends: none. Ownership: src/habu/aot-capture.f owner-WID accessors. Claim: unassigned.
