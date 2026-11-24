---
title: Delete error.f's unused code-range markers
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T11:50:32.951981+02:00"
---

Problem: src/lib/error.f keeps FIRST/LAST markers for code ranges no constant is minted in: E-CID (-6700..-6799, :151), E-CIE (-6900..-6919, :160) and E-CRL (-6880..-6899, :167), each with its comment; by name, E-DIAG, E-JRP, E-ARITH, E-TR, E-IR and E-WS-CONN have no code outside their markers either. Habu keeps no legacy codes. Acceptance: every marker whose range holds no minted constant (checked by value, not by name) goes with its comment, and nothing under src, tools, test or docs names it. Depends: none. Claim: unassigned.
