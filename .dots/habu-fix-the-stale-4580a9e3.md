---
title: Fix the stale text the comment sweep left
status: open
priority: 3
issue-type: task
created-at: "2026-11-09T17:59:10.999511+02:00"
---

Problem: comments still name capture-era machinery the arm64 build no longer has. Sites on master: src/habu/address-cells.f:156 'Native-build owns the explicit lifetime cut through its retired source heap' on KEEP-BELOW (:158, which src/habu/interpreter.f:2725 calls, so the code stays); src/lib/error.f:10 gives src/core/util.f's ERROR-TEXT as the reason error.f does not require util.f, which may no longer hold; src/lib/fs-mutate.f:11 requires src/lib/fd.f, which requires src/lib/task.f (fd.f:17), so the reason src/habu/layout.f gives for the FS-ABI and FS-MUT-ABI bands (:990-1049) may be gone.
Acceptance: each site states what the code does now (docs/forth-card.md section 9) or goes.
Files: those named.
Verify: read the diff; rg the quoted phrases over src tools docs; hb rebuilds itself byte for byte.
Depends: none (lands after arm64). Ownership: those files. Claim: unassigned.
