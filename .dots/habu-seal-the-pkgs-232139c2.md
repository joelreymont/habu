---
title: Seal the packages the engine bakes
status: active
priority: 2
issue-type: task
created-at: "2026-11-12T17:00:54.623059+02:00"
---

Problem: docs/design.md:160-165 says the native build seals every package a capture ships, so no baked package can be reopened, but bin/hb lets a loaded source reopen a baked package and resolve its privates: `package MACHO ' MH-MAGIC64` resolves a private of src/os/macos/macho.f, and tests reopen package LINKER (test/compiler/arm64-link-data.f:42, arm64-place.f:40, arm64-place-unsafe-bad.f:24, arm64-link-data-bad.f:22). docs/forth.md:199-200 says only seal-package seals. A reopened package can republish a private word, the hole design.md names. Acceptance: a source loaded on a product engine that opens a baked package is refused, the tests that reached LINKER privates reach them another way or move, and docs/design.md and docs/forth-card.md section 6 state what holds. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-seal (census).
