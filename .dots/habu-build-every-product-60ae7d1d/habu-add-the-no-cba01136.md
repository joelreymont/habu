---
title: Add the no NATIVE- or N- prefix rule to the forth card
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T22:34:02.964301+02:00"
---

Problem: Joel's naming rule, quoted from habu-name-the-build-abed309a: "no NATIVE- or N- prefixes; there is no non-native build", was to go into docs/forth-card.md section 1 (Names and packages, :8) when the first rename landed. The build is package BUILD (src/lib/build.f:12) and the compiler package COMP (src/habu/t0-arm64.f:22, t0-x64.f:20, t0-wasm.f:41, t0-label.f:14, src/compiler/fixed-body.f:10), and no NATIVE-BUILD- name is left, but section 1 does not state the rule. Tier 1's N packages (NELAB, NTAPE, NABI and the rest under src/compiler/) go with habu-delete-tier-1-f5c44fb4, not by rename. Acceptance: docs/forth-card.md section 1 says a name carries no NATIVE- or N- prefix, since every build is native. Files: docs/forth-card.md. Verify: read section 1. Depends: none. Ownership: unassigned. Claim: unassigned.
