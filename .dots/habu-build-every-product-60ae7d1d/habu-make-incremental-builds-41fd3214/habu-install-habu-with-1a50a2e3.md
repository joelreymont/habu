---
title: Install Habu with a Habu command, not rsync
status: active
priority: 2
issue-type: task
created-at: "2026-11-09T22:19:16.299037+02:00"
---

Problem: Habu has no install step; agents mirror a workspace over ~/.local/lib/habu with rsync -a --delete, which keeps the workspace's file times (memory one-public-hb). Joel 2026-11-09: 'why are you using rsync and introducing a dependency on another tool?', 'always fix the cause, not the symptom!'. Acceptance: a design Joel approves, then a lane. Files: design first (read-only Plan). Verify: Joel's approval. Depends: none. Ownership: jerry. Claim: agent=jerry Plan (read-only).

Landed 2026-11-10 (jerry): tools/install.f, commit "Install Habu with tools/install.f" (DECISIONS "the install command"). Tried with HOME set to a scratch directory: a first install, a rerun that leaves equal files untouched, strays and a symlink removed, a changed file replaced, .out kept. Open until the Mac and spark installs have run with it.
