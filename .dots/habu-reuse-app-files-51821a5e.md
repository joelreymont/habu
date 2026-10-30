---
title: Reuse app files built at tier 0
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T22:40:57.502786+02:00"
---

Cold-load worker 3 (carl-fcold/.out/cold/coldroles-handoff.md): hb-build's maker runs at the caller's tier, 0 for hb, and every tier-0 file is refused reason=tier (a tier-0 object holds no definitions; census A ruled the refusal real). So an app built through hb-build never reuses a file: tools/hb-build-producer-cache-test.f's unchanged rebuild reuses 0 of 2 on master and with the fix; with 1 set-tier the maker reuses both. Engine builds are unaffected (P0 reuses 147/155). Decide whether app builds run the maker at tier 1 or tier-0 objects carry definitions.
