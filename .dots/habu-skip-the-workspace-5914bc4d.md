---
title: Skip the workspace directory by its present name
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T10:07:09.659155+02:00"
---

Problem: src/lib/fs.f SKIP-DIR? (:643-649) and tools/install.f:113 name the workspace directory .jj-ws, and tools/lsp-test-lib.f (:360, :4789-4812) tests that name; workspaces now live under .jj-ws.noindex, so FS:WALK-FILES from a repository root descends into every workspace and install's check does not see them. Found 2026-11-10 by reading, while root-causing a test/file-objects.f failure under a TMPDIR inside .out. Acceptance: a walk from a repository root visits no file under .jj-ws.noindex; install.f treats .jj-ws.noindex as it treated .jj-ws; no source names .jj-ws alone (hard cutover). Files: src/lib/fs.f, tools/install.f, tools/lsp-test-lib.f. Verify: src/lib/fs-test.f's walk cases and the lsp-test case at :4789 with the new name. Depends: none. Ownership: carl. Claim: unassigned.
