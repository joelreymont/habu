---
title: Close the reserved-cell definer to checked code
status: closed
priority: 2
issue-type: task
created-at: "2026-11-13T23:00:56.897774+01:00"
closed-at: "2026-11-18T15:56:21.148104+01:00"
---

Problem: RESERVED-PTR-U8-CELL (src/core/pointer-storage.f) is public and takes any offset, so a checked program mints a ptr ptr u8 cell anywhere in DATA: `$3800 RESERVED-PTR-U8-CELL N : FORGE ( -- ) s" x" drop N ! ;` overwrites the engine's permanently-zero NULL-PTR cell, after which NULL-PTR ( -- ptr a ) hands that byte pointer back as a pointer of any pointee; `V NULL-PTR - data-base NULL-PTR - - RESERVED-PTR-U8-CELL X : AS-PTR ( -- ptr u8 ) X @ ;` returns an integer stored in variable V as a ptr u8. Both certify, rc 0 on the installed hb. The public POINTER-STORAGE:DATA-CELL ( n -- ^^u8 ) (same file) is the same door without a definer: `: P16 ( ^u8 -- ) $360 POINTER-STORAGE:DATA-CELL ! ;` overwrites the checker's declaration owner cell and passes tools/check.f --verify-only, rc 0. Acceptance: checked code outside the engine cannot declare a reserved cell or take DATA-CELL; the engine's own reserved cells (src/habu/layout.f, src/core/include.f, src/core/check-hook.f, the engine-origin files) keep working; the forge probes are refusal cases in test/compiler/raw-cell-pointer-refusals.f. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-rc.
