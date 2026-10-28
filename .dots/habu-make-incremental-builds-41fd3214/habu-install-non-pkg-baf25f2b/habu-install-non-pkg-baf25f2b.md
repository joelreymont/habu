---
title: Install non-package wordlists from file objects
status: closed
priority: 2
issue-type: task
created-at: "2026-10-25T03:52:46.545920+01:00"
closed-at: "2026-10-28T17:13:32.627311+02:00"
close-reason: landed 1e8fb3db, 517e5b6a; unsupported-wordlists gone
---

Blocks landing: auto e2e GENERATED-CASE warm and third builds miss app.f reason=unsupported-wordlists (ENUM/STRUCTURE; TFAIL 214 215 221 222 at carl-int); the case is also restated per file (generated declarations in a required file). Fable Plan design running; then a worker lane on the carl-int tip.

Lead ruling 2026-10-25 (design-wl-identity.md §10): the language refuses definitions into bare wordlists (WORDLIST-BARE), so only empty bare wordlists and their file identity are carried. wlid1 lane in .jj-ws.noindex/carl-wl; wlid2 (bare-wid file key) follows.
