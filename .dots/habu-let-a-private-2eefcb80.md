---
title: "Let a private cast name another package's type"
status: active
priority: 1
issue-type: task
created-at: "2026-11-01T02:24:36.099924+02:00"
---

Problem: CAST: refuses (E-CAST-OWNER 7135) a destination that introduces another package's NEWTYPE or OPAQUE structure, through pointers and fields. It cannot keep its promise that only the owner makes such values: the reverse projection ( ptr JR:cursor -- ptr n ) rewrites a real reader (/tmp/carl-cast/p1.f, JR:SPAN$ returns 64 bytes of a 3-byte document), the same for a NEWTYPE cell (p4.f), a private ( n -- ptr u8 ) cast plus c! rewrites one with no cast naming JR (/tmp/carl-cast-fable/raw.f), and a quotation source forges one (habu-check-a-cast-86464052). It refuses only honest code: Maki src/project-ops.f:176 SCRATCH-CELL, Zen compare.f:39, structure.f:138/:430, xlsx.f:93. Fable and Astra reached this independently (2026-11-01). Ruling (lead, on Joel's 'use your best judgement' relayed by phil): drop the foreign-owner question; keep E-CAST-MINT and the arena-head rule, which E-CAST-OWNER now names alone. Acceptance: brief .jj-ws.noindex/carl-dots/.out/carl-file/brief-cast-owner.md. Files: src/core/checker.f, type-family.f, xref.f, test/cast-negative-suite.f, docs. Verify: cast-negative, nominal-pointer, c2-memory-source, arena, native-window-owner, full suite. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-cown.
