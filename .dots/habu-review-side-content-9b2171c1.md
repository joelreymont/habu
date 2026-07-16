---
title: Review side-content fixes
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-17T03:09:29.563949+02:00\""
---

Independent destruction review of revision 0b375dfe after the prior side-content findings. Read the five claimed dot specs and code only; verify each extra file is required, SHA state is owned and interleaving-safe, SIDE commits only after full binary validation, empty binary is rejected while empty file content remains valid, growth exceeds 128 KiB, ROW-NEXT? and public SIDE are covered, errors are package-owned, and no scanner/compiler/filesystem scope leaked in. Read-only review in an isolated jj workspace; report prioritized file:line findings and unnecessary complexity. Do not edit implementation or close implementation dots.
