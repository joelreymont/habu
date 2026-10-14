---
title: Reduce XML streaming allocation churn
status: open
priority: 1
issue-type: task
created-at: "2026-10-15T02:16:38.318282+03:00"
---

Bob isolated a public Habu library performance defect on b685c051, excluding Tender: XML:INIT-INPUT plus XML:NEXT over unchanged866645-byte worksheet toEOF yields118781tokens,454987minorfaults,0.47suser/0.90ssystem, while merelyloadingbytes yields348faults/negligibleCPU. Kaizen no-opSCAN yields1825295faults vs archive-open-only457. Repro held at /Users/joel/.cache/tmp/xlsx-reader-churn-repro/{xml.f,xml-entry.f,build.f,sheet.xml}; sheetSHA a14adb071c70b17fc3ac9b65a0e78c31ae3636f95d4e03f65157e4e3511901b3. Build from publicrelease with hb --load <repro>/build.f -- <repro>/xml-entry.f -o <repro>/xml-repro; run /usr/bin/time -l <repro>/xml-repro <repro>/sheet.xml xml. Own the demonstrated XML/library allocation boundary with existing owner-scoped reusable/chunk storage where appropriate, preserving lexical/token/source lifetime, streaming bounds, correctness, partial-failure cleanup and source owners. Measure identical input/output and actual faults/systemCPU before/after; do not optimize unrelated allocation targets or add an allocator framework. Inspect existing streaming/source/native E2E before test changes; no caller/private-engine workaround. Bob owns isolated Habu library implementation with his existing Astra diagnosis, required independent review and focused source/native consumers. Core changes or material design questions return to Carl; no duplicate core lane. Qualify immutable public library release on a matching qualified engine if engine unchanged, preserve current core source work and symlinks. Do not claim improvement without observed unchanged workload evidence.
