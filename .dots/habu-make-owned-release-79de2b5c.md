---
title: Make owned release uncatchably fatal
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T22:16:26.896717+02:00"
blocks:
  - habu-add-forked-mem-423076fc
  - habu-rename-owned-release-5736ed92
---

Why: the frozen disposal contract (pillar A, txn-v2 plan) classifies a failed whole-range release of owned bytes as a violated owned invariant with no sound continuation; today MEM:RELEASE (post-rename) throws E-MEM-UNMAP catchably, so callers can resume after ownership is gone - the false-recoverability hybrid two reviews rejected, and the explicit blocker for the codex transactional loader leaf.

Behavior, the semantic flip ONLY (the rename and provenance audit land first in habu-rename-owned-release-5736ed92): same stack effect ( ptr u8 CAD-NUM:alloc-byte-len -- ); success returns having released; a negative munmap emits ONE allocation-free diagnostic (fixed byte string plus base and length rendered from stack scalars, single write to stderr, no allocator, no formatting machinery) then terminates through the direct exit path bypassing throw and catch, with a named exit-code constant minted beside the MEM error block, distinct from engine compile-fault codes, gate codes, and every E-* value. No third outcome. This leaf EXPLICITLY removes WB-COMBINE and the caught cleanup leg from MEM:WITH-BYTES - a direct fatal release cannot retain or combine a cleanup error, so the caught leg and its error-combination logic are deleted, not guarded, and WITH-BYTES becomes straight-line around a total cleanup. Forbidden: catchable wrappers, result shapes, cleanup coordinators, retained-state guards at callers.

Owner: lib/memory.f (MEM). Dependencies: habu-rename-owned-release-5736ed92 (mechanical rename plus provenance audit) and habu-add-forked-mem-423076fc (the proof machinery). Acceptance: fork-based fixture through WITH-RELEASE-FAULT asserts the child exit code and exact stderr diagnostic; catch-bypass proof - the parent wraps the disposal in catch and measures it cannot intercept; WITH-BYTES fixtures prove the cleanup leg is gone (no combined error path remains, straight-line total cleanup); focused memory suite green; both diff lints clean. Real pre-change failure: a caught E-MEM-UNMAP after BUF-FREE leaves mapped bytes with no owner and execution continuing - measured in the landed disposal review.
