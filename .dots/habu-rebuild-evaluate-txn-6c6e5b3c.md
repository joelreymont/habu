---
title: Rebuild evaluate transaction rollback on master
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T00:42:48.621870+02:00"
blocks:
  - habu-load-registry-rollback-d955db59
---

Verified engine-integrity defect: evaluate recovery restores pointer watermarks but not in-place XREF retirement or checker signature mutations, so a caught failure can destroy an older word and leak a replacement. Owner: the existing composite evaluate savepoint and CHECKER-TXN coordinator. Capture the XREF retirement journal and checker registry scope at each evaluate frame; reserve every fallible restore before mutation; on failure restore checker registries and package mode/name, undo retired-record content, then restore CP, NDICT, DP, and engine package cells in one defined order. On success finalize every participant once. Reuse the landed registry rollback composer; do not copy type, field, event, or owner state into the engine frame. Production red: after `s" FZ-A" FORGET-DEFS-FROM TRUSTED: FZ-OV ... ; -713 throw` inside caught evaluate, FZ-A is retired and FZ-OV remains live. Acceptance: the real test/eval-xref.f path preserves both words and checker signatures after every injected failure; nested frames restore only their own mutations; a failed evaluate and REPL line that closes a directly flagged owner package restore the XREF marker count, TFAM owner query, and checker/native package mode together; success persists all three. No mutation begins until all restore readiness checks pass. Files: the composite checker transaction, native/bootstrap evaluate frames, XREF journal owner, and focused production tests only. Forbidden: pointer-only repair, owner-specific rollback table, copied registry state, error swallowing, heuristic resync, compatibility path, or lint. Smallest owning check: the existing caught FORGET reproducer plus one owner-package close followed by throw through real evaluate. The preserved July design commits are evidence only, not code to merge. Claim: unassigned.
