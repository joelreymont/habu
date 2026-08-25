---
title: Re-measure the Linux size decomposition at a byte fixpoint
status: open
priority: 2
issue-type: task
created-at: "2026-08-12T09:05:04.970710+02:00"
---

Scope discovered by the zedgate lane (2026-08-12): the Linux rows in test/gate-size-attribution-test.f are DELIBERATELY ZEROED and fail closed (LINUX-CODE-TEXT 0, LINUX-FLOOR-DIST 0, the 2026-08-05 note at lines 678-703 explains: merge cd7bf8eb left them describing a binary that no longer exists, and PAGE-UP(CODE-OFF+CODE-TEXT)+LINUX-RW is one equation with two unknowns - a third invented number would be page rounding wearing an attribution's clothes). GE-CODELEN-ENFORCE takes the base-0 branch on Linux = guaranteed candidate red by design until measured. The work is NOT a row nudge: on a linux-arm64 host, install --force twice to a byte-identical bin/hb, HABU_ENGINE_SIZE_MAP=1 through the stdin metabuild host, commit LINUX-CODE-TEXT + LINUX-FLOOR-DIST + all 44 per-region budget rows (lines 734-778, currently unenforced behind HOST-REGION-BUDGETS-MEASURED? false) with reconciliation to zero residue, macOS-row idiom for the derivations. BLOCKED: zed (the only linux-arm64 host) offline since 2026-08-09 with spark - site event, user informed; probe reachability before dispatch. Files: test/gate-size-attribution-test.f. Depends: device availability; do together with habu-confirm-the-pty-4eb65fbf on the same visit.
