---
title: Decide what a build rebuilds from timestamps, without hashing
status: closed
priority: 2
issue-type: task
created-at: "2026-11-09T22:19:16.287727+02:00"
closed-at: "2026-11-10T09:22:38.884870+02:00"
close-reason: landed; a build decides from stamps and times and hashes nothing; engine cold 13.2 s, warm 1.9 s, 0 of 339 rewritten, warm hb = cold hb; suite 270 of 270
---

Problem: a warm build decides a file is unchanged by SHA-256 (src/habu/linker.f INPUT! :664, WRITE-OBJECT :1481, HEADER-CK :1871, KEY-SUM :369 over src/lib/engine-id.f). Joel 2026-11-09: 'just checking the timestamp is sufficient, why did we introduce sha256?', 'make does NO hashing'. Acceptance: a design Joel approves, then lanes. Files: design first (read-only Plan, Fable). Verify: Joel's approval. Depends: none. Ownership: jerry. Claim: agent=jerry Plan (read-only).

Lanes 2026-11-10 (jerry): reached and hbid merged into .jj-ws.noindex/jerry-inc (68274dd1, 14bcaa40); lane keys running, workspace .jj-ws.noindex/jerry-keys, brief .jj-ws.noindex/jerry-inc/.out/briefs/keys.md; then removals, tests, the edit-case proof, the review.
