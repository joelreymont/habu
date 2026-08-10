---
title: Measure the chain-compiled REPL against the AOT blob cap
status: open
priority: 2
issue-type: task
created-at: "2026-08-10T18:38:00.881854+02:00"
---

AOT-BLOB-CAP is 64KB with u16 site offsets - a hard ceiling. The chain emits more bytes than the engine for the same source; whether the chain-compiled REPL capture window still fits is UNMEASURED and is a cut prerequisite (aotsite 2026-08-11). Measure it (chain-compile the four REPL sources, sum the emissions + site rows) BEFORE the cut lands; if it does not fit, the ceiling's growth is its own design (u16 offsets). Files: measurement first. Depends: habu-per-site-relocation-bb9b6d70 stage 1. Blocks: habu-cut-colon-compilation-a5aa3f1f.
