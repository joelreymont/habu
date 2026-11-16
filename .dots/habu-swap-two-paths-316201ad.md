---
title: Swap two paths atomically
status: active
priority: 2
issue-type: task
created-at: "2026-11-16T18:29:47.247685+01:00"
---

Problem: Omabox's installer replaces its binary and plugin directory with renameat2(RENAME_EXCHANGE) so a crash leaves either tree whole (Omabox install.rs:1046, bob's /tmp/bob-habu-lib-request.md); FS has RENAME-FILE, RENAME-NOREPLACE and RENAME-SYNCED (src/lib/fs-mutate.f:144-160, :747) but no exchange. Acceptance: FS:SWAP-PATHS (Joel 2026-11-17) swaps two existing paths in one step (renameat2 RENAME_EXCHANGE on Linux, renamex_np RENAME_SWAP on macOS), leaves the errno in FS:ERRNO and throws on failure; a test swaps a file and a directory. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-swap.
