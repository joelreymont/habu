---
title: "Store type-family.f's object in no-diag cold builds"
status: closed
priority: 2
issue-type: task
created-at: "2026-10-31T09:59:38.340311+02:00"
closed-at: "2026-10-31T20:23:16.875393+02:00"
close-reason: "landed: lane 11 as 4b54ed21 Reuse compiled files in incremental builds, 9a25b36f, a8bdf2af on master; gate at the lane tip: native-build rc 0, registry rc 0 1164 s, P0 cold 164 compiled, warm 162 reused/2 compiled, warm == cold, two-generation rc 0; after rebase over ef2bf183 the overlapping rows (check, check-verify, gate-diagnostics, check-all-errors, repair-packet, load-reject-diag, runtime-regression, native-file-frame) rc 0"
---

After the hook fix (pxpzvlxw), a cold engine build without HABU_NATIVE_CACHE_DIAG stores 146 objects; type-family.f's is silently refused (stored under diag, and at the parent). Root-cause and fix. Brief carl-dots/.out/carl-file/brief-tfam.md. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-tfam.

Resolved by lane 11's unmrmrst (stale process map): at the lane 11 tip type-family.f is refused every build (mapped-compile-time-read TF-A-P) in both modes, so no object is stored either way (dot habu-key-a-file-b9c53392). Closes at the lane 11 landing; reuse of the file is dot habu-reuse-checker-f-5bd58a27.
