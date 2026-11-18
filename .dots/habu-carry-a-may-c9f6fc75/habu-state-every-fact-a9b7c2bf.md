---
title: "State every fact in the program entry's type"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-18T09:36:01.892669+01:00"
closed-at: "2026-11-18T09:50:36.299564+01:00"
---

Problem: src/habu/layout.f APP-ENTRY:XT! keeps the entry as [ -- ], so a program whose MAIN parses or creates is rejected inside src/habu/app-end.f:17 with a help to edit engine code, before link-band.f's E-CANNOT-RUN-STRIPPED (wasm-device case 71). Acceptance: the entry's type states every fact; case 71 passes. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c9e2.
