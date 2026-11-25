---
title: Give a persistent breakpoint out-of-line forms on arm64
status: active
priority: 2
issue-type: task
created-at: "2026-11-25T16:05:33.207735+01:00"
---

Problem: src/habu/debug.f's arm64 TRAMP! takes every instruction, but the handler degrades one that reads or sets PC (b, bl, adr, cbz) to one-shot when it hits (debug.f's comment above TRAMP@), so DEBUG:BP* and BPN on a word that begins with a tail branch or an adr fire once. Acceptance: a word entered by b, adr or adrp, the forms T0 begins a unit with besides plain instructions, breaks on every call, as on x86; a form with no trampoline is removed by its first call and reported. Depends: habu-run-x86-persistent-1a090bfb. Claim: agent=worker workspace=.jj-ws.noindex/jerry-bp-arm.
