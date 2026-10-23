---
title: Refresh LSP source locations on edits
status: closed
priority: 2
issue-type: task
created-at: "2026-10-16T11:17:41.570747+03:00"
closed-at: "2026-10-22T12:36:35.173776+03:00"
close-reason: "Triage 2026-10-22, DONE: tools/lsp-references.f:20-33 checks against \"one consistent editor version or disk stamp\" instead of a whole-file SHA. engine-origin.f no longer keeps hashed source copies."
---

tools/lsp-core.f already receives document versions, but lsp-references.f uses whole-file SHA refusal; lsp-test-lib.f REFS-DIGEST-TURNS requires an unrelated comment append to fail until the requester changes. Use document versions and disk size/mtime to refresh affected semantic results. Replace engine-origin early per-definition source copies/hashes with source-unit origin metadata, coordinating current reader/origin hooks with Carl. Preserve definition/reference/rename correctness for moved, removed, shadowed and unsaved declarations; existing LSP E2Es plus required source-matched build and navigation audit. Mark owns; includes pending navigation work, no blanket cache-key replacement.
