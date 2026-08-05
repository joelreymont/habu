---
title: Measure the Linux size decomposition on spark
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T11:01:50.407985+02:00"
---

test/gate-size-attribution-test.f now carries LINUX-CODE-TEXT 0 and LINUX-FLOOR-DIST 0 — the manifest's spelling of 'unmeasured, fail closed' — because merge cd7bf8eb's 118420/3732 described a binary that no longer exists and a page-rounded total (127168) does not determine its decomposition (one equation, two unknowns). On spark, build the integrated tree to fixpoint, run the candidate gate, commit the real CODELEN/FLOOR-DIST and refreshed per-region rows (the old rows still carry master's pre-deletion shape: no aot-owner row, protected-wid 120 not 1540). Also confirm LINUX-RW 192 still describes the integrated binary — it was carried over unexamined.
