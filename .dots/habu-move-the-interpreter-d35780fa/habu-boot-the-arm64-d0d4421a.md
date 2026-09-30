---
title: Boot the ARM64 product through the Habu MAIN
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.697835+03:00"
blocks:
  - habu-route-stdin-repl-d7e12895
  - habu-hook-tier-0-96e33c29
---

Problem: the seeded product's `EM-STARTUP` (`src/habu/habu2.f:7428-7451`) never walks `LAOTBOOTRUN`, which is emitted only for cold engines (`habu2.f:10016-10023`), and `APP-ENTRY:XT-CELL` cannot carry the engine's `MAIN` because it also flips the argv and stdin conventions (`src/habu/aot-owned-cells.f:171-179`, `habu2.f:1305,1747,1869`); the product's REPL installs at window time (`src/habu/repl.f:237-240`, `src/habu/native-runtime.f:130`). This is the one leaf that changes the kernel boot on ARM64.
Acceptance: `ENGINE-MAIN:XT-CELL` (declared in `src/habu/layout.f` by K2) is stored by `main.f` at window time and relocated by the seed as an address row; `main.f` a manifest row; `EM-ENGINE-MAIN` (the five-line shape of `EM-APPLICATION-START`, `habu2.f:7422-7426`) emitted in the seeded branch after `EM-SNAPSHOT-RX-FLUSH`; `EM-APPLICATION-START` and `EMIT-SOURCE` skipped with `SEEDED-RUNTIME? if exit then` (the pattern of `EMIT-COLD-PREFIX`, `habu2.f:1721-1722`); a zero cell dies with `ENGINE-ERROR:AOT-SEED` like `NCOMP-EMIT:UNSET`; `MAIN` honours `APP-ENTRY:XT-CELL` (executes it, then the application argv/stdin conventions `src/os/script-argv.f` keys on that cell); cold engines (`SEEDED-RUNTIME?` false) untouched, so `bootstrap/cg/forth.fs` needs no mirror; no `.names` or coverage proof; `docs/x86-64.md` gains the Habu interpreter section.
Files: `src/habu/habu2.f`, `src/habu/main.f`, `src/habu/native-runtime.f`, `docs/x86-64.md`.
Verify: spark: rebuild; chain to gen 5 (a builder change is in gen1: gen1==gen2 expected); full gate; the periodic no-binary check (`docs/bootstrap.md:206-220`).
Depends: habu-route-stdin-repl-d7e12895 (I10b), habu-hook-tier-0-96e33c29 (I6), habu-add-the-x86-a8bf9973 (K2: declares `ENGINE-MAIN:XT-CELL`). Serialise on `habu2.f` (K4, I6, X7).
Route: Alder (shared: src/habu/habu2.f, src/habu/main.f, src/habu/native-runtime.f).
Ownership: krait (Intel lane).
Claim: unassigned.
