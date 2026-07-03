---
title: "TFAM 8: generated constructors without trust"
status: open
priority: 2
issue-type: task
created-at: "2026-07-03T23:36:48.942711+02:00"
---

PLAN.md item 8. Checked constructor effects+runtime words from SUMV metadata (payload cells, zero padding, tag); no TRUST/TRUSTED:/set-check emitted; package name via pinned Package Shape escape/hash derivation, reserved + non-reopenable, collision-checked; private families export nothing (metadata only until TFAM 9 construct form); bodies lower through existing checked paths (Gforth parity proven at TFAM 10); linear payload constructors reject until TFAM 11; undefine of generated entries rejects. Gate 17i. Depends: TFAM 7, TFAM 12.
