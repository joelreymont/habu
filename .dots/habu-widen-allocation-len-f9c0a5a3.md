---
title: Widen allocation length to byte length
status: open
priority: 1
issue-type: task
created-at: "2026-07-28T03:47:04.573876+02:00"
blocks:
  - habu-scan-public-cast-95c07c25
---

Problem: guard and bounded-memory code receive CAD-NUM:alloc-byte-len from the allocator but may need the same extent as the weaker CAD-NUM:byte-len role. Current checked code cannot call MINT-BYTE-LEN with alloc-byte-len; the exact native probe fails at the nominal mismatch with exit 70, which has driven consumer casts or raw length reconstruction. Owner and interface: package CAD-NUM in lib/cad-num-types.f owns public AS-BYTE-LEN ( CAD-NUM:alloc-byte-len -- CAD-NUM:byte-len ) as one owner-declared identity CAST. This is total subset widening: every positive allocation byte length is already a valid ordinary byte length, and the exact cell value is preserved. It adds no raw n input or output, reverse conversion, validation, result arm, throw, new role, public projection, TRUSTED word, MEM, guard, GPT-2 code, alias, or compatibility surface. Checkpoint: retain the failing exact checked candidate and prove the CAST form certifies before implementation. Tests in lib/cad-num-types-test.f prove exact values 1 and maximum through owner-test-only readers, certify the exact public signature, reject raw n, byte-len, alloc-cell-count, reversed, and raw-output candidates, and prove the representative whole-allocation to byte-length composition without exposing a projection. Files: lib/cad-num-types.f and lib/cad-num-types-test.f only. Acceptance: the focused CAD-NUM type suite and exact consumer candidate pass; typed-local, package, declaration, public-signature, and owning standard-library gates pass. Mutations using MINT-BYTE-LEN directly, reversing the conversion, accepting raw n, exposing n, changing the value, or adding trust fail. Smallest owning-path check: checked code can pass the allocator-issued extent to a byte-len consumer without a consumer-private cast or raw reconstruction.
