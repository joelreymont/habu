---
title: Map the occurrence table without zeroing it
status: open
priority: 2
issue-type: task
created-at: "2026-11-26T19:08:36.691598+01:00"
---

Problem: every ARM64 boot maps a fresh, already-zero occurrence table (src/habu/habu1.f EM-DEF-OCC LINIT) and LRESET then stores zero into all DICT-CAP slots, so every process holds about 1 MB of resident zero pages; x86-64's OCC-INIT (src/habu/boot-x64.f) writes only the slots it issues. Acceptance: an ARM64 boot writes only the slots it issues, and an empty program's resident size drops by the table's unused pages. Depends: none.
