---
title: "Reduce bin/hb size: dedup cold-prefix loader"
status: open
priority: 1
issue-type: task
created-at: "2026-07-03T10:02:37.718688+02:00"
---

RCA in docs/size-rca.md (MEASURED byte-exact __text map, 2026-07-03). Root cause: EMIT-SOURCE (habu2.f:779) emits the cold-prefix trio (EMIT-COLD-PREFIX + PFX-LOAD-SCRIPT-ARGV-COLD + PFX-PROVIDE-FILES) INLINE at 4 source-entry points (C-SOURCE-PIPE :650, C-SOURCE-FILE-PREFIX :681 & :686, C-SOURCE-FAIL-REPL-DONE :731). Each trio = 9600 bytes (PFX-PROVIDE-FILES alone 9424, emitting 's" PATH " provided' char-by-char at ~36 bytes/char for ~19 files). Total 39568 bytes = 35% of the 113448-byte __text. Fix A (dedup, low risk, escape-decoder precedent): factor the trio into ONE BL-callable routine, save/restore x30, keep x9/x11 contract; -28800 bytes -> file ~116KB. Fix B (densify): bake constant s"/provided wrappers as data + copy loop; further -8774 -> file ~99.7KB (hits 90-100k target). NOT the lever: dict-record schema (only 148 baked records = 7156 bytes, 2099 is runtime arena; falsified). NOT valid: tree-shake (SHAKE?=0 required for self-hosting/REPL). Each fix: byte-for-byte fixpoint + full gate + lower GB-SIZE-BASELINE-MACOS in same commit.
