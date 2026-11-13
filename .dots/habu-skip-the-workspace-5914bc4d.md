---
title: Skip the workspace directory by its present name
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T10:07:09.659155+02:00"
---

Problem: src/lib/fs.f SKIP-DIR? (:699-705) names the workspace directory .jj-ws, and tools/lsp-test-lib.f (:412, :4507-4530) tests that name; workspaces now live under .jj-ws.noindex, so FS:WALK-FILES from a repository root descends into every workspace. Acceptance: a walk from a repository root visits no file under .jj-ws.noindex; no source names .jj-ws alone (hard cutover). Files: src/lib/fs.f, tools/lsp-test-lib.f. Verify: src/lib/fs-test.f's walk cases and the lsp-test case at :4507 with the new name. Depends: none. Claim: unassigned.
