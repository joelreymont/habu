---
title: Install Habu with a Habu command, not rsync
status: closed
priority: 2
issue-type: task
created-at: "2026-11-09T22:19:16.299037+02:00"
closed-at: "2026-11-10T21:56:45.399791+02:00"
close-reason: "landed: tools/install.f is on master and the Mac install ran with it; the spark install is an acceptance case of habu-cross-build-a-cb630896 (dots audit 2026-11-10)"
---

Problem: Habu has no install step; agents mirror a workspace over ~/.local/lib/habu with rsync -a --delete, which keeps the workspace's file times (memory one-public-hb). Joel 2026-11-09: 'why are you using rsync and introducing a dependency on another tool?', 'always fix the cause, not the symptom!'. Acceptance: a design Joel approves, then a lane. Files: design first (read-only Plan). Verify: Joel's approval. Depends: none. Ownership: jerry. Claim: agent=jerry Plan (read-only).

Landed 2026-11-10 (jerry): tools/install.f, commit "Install Habu with tools/install.f" (DECISIONS "the install command"). Tried with HOME set to a scratch directory: a first install, a rerun that leaves equal files untouched, strays and a symlink removed, a changed file replaced, .out kept. Open until the Mac and spark installs have run with it.

Mac install (jerry, 2026-11-10, tree a5b8d2a9, `bin/hb --load tools/install.f` from .jj-ws.noindex/jerry-inc, 0.44 s): ~/.local/lib/habu holds bin, src, tools, test and its .out; the entries the tree does not carry (docs, .dots, the top-level files and the rest) are gone. Installed hb equals the tree's hb before the signature, hb.map is equal, `codesign -v` passes, src/tools/test differ from the tree only by the symlinks the install does not carry, and `~/.local/bin/hb --load` of a one-line file prints 3.

spark waits: the install needs a Linux arm64 hb and map built from the installed tree, and nothing builds one (habu-cross-build-a-cb630896: hb-build has no --target, and no hb on spark loads this tree). This dot closes with the spark install.
