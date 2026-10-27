---
title: Derive the recovery stage family files from the prefix list
status: open
priority: 2
issue-type: task
created-at: "2026-10-27T12:38:12.500153+02:00"
---

tools/bootstrap.sh SRC_COMMON and tools/build-fixpoint.f BF-APPEND-FMT each restate, by hand, which stdlib family files the boot prefix loads and in what order (src/habu/habu2.f PFX-LOAD-STDLIB-FILES). The seed refuses a captured type registry that does not start where the prefix's does (src/core/type-family.f REG-AOT-BASE-BAD), so every new family file in the prefix (num-types.f, then memory.f's frame STRUCTURE) broke Gforth recovery until both lists were edited (lane TFAM, change wqzvxowz). Derive both lists from the prefix's own list so they cannot drift; bootstrap.sh may only invoke, so the derivation lives in Habu or in a file both read. Workspace: none yet.
