---
title: Route the maki suite into the gate
status: closed
priority: 1
issue-type: task
created-at: "\"2026-07-18T23:36:09.301637+02:00\""
closed-at: "2026-07-19T01:17:27.177362+02:00"
---

The gate (printf '' | bin/hb --load test/run.f) does not load or run ANY maki code; maki/test.f is a separate manual entry point (bin/hb --load maki/test.f). Proven consequence (2026-07-18, deftype-retirement probe): deleting a word that maki/report.f load-bears on gates GREEN while breaking the entire Model CAD pipeline - the flagship. Every 'gate green' merge verdict to date proved nothing about maki. Fix: run maki/test.f as a routed gate case - a spawned child engine like the other child batteries, in its own progress subject/group so its wall time is budgeted and visible. First measure its runtime on spark solo; if it fits the existing external-battery pattern (test/run-lib.f TR-EARLY-EXTERNAL-START), route it there; otherwise give it its own group. The merge protocol keeps requiring one command: test/run.f must be the single tree-proving entry.
