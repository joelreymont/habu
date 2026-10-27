---
title: "Build into the tree's own .out"
status: open
priority: 2
issue-type: task
created-at: "2026-10-27T13:24:03.122005+02:00"
---

Problem: the shared ~/.cache/habu-build root (src/lib/build-cache.f) collects every tree's outputs in one flat directory; it reached 3,524,234 entries and froze amfid for 18 s-4 min per executable launch, and a run of the installed hb wrote 23 GB into ~/.local/lib/habu/8216dd63/.out. Ruling (Joel 2026-10-27): no shared cache; each tree builds into <cwd>/.out/<engine-key>/{obj,img}; the installed release is never written; one compiled-object kind (file objects); builder and test images are ordinary outputs; bin/hb stays; hard cutover, no versions. Design and rulings: /Users/joel/.cache/tmp/carl-file/design-build-out.md (copy into this dot's lanes as they are briefed). Acceptance: lanes 1-4 below landed; rg -n 'HABU_BUILD_CACHE|BUILD-CACHE:|FS-LOCK:' src tools test docs empty; full suite green from a fresh workspace using only its own .out. Depends: KEYS1 nupyzutv and KEYS2 tlzprqrx landed (same file, tools/native-definition-cache.f). Ownership: carl. Claim: unassigned.
