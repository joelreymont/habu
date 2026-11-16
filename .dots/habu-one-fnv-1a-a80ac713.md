---
title: One FNV-1a fold per width
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T16:26:06.292148+01:00"
---

Problem: the FNV-1a byte fold is written out in tools/lsp-workspace.f (MIX), src/core/compile-reads.f (FNV$+), src/habu/linker.f (NAME-SUM, BYTES-SUM) and src/habu/intern-table.f (FNV), and its constants are named three times (src/core/checker.f HIDX-FNV-BASIS and HIDX-FNV-PRIME, tools/lsp-workspace.f FNV-BASIS and FNV-PRIME, src/core/sumtype.f DRV-FNV-BASIS and DRV-FNV-PRIME) and spelled as literals in src/habu/habu1.f's emitted hash. Acceptance: each width's fold is one word and its constants one pair, which every site above calls or names; src/habu/kernel-x64.f's copy follows when x86-64 returns. Depends: none. Claim: unassigned.
