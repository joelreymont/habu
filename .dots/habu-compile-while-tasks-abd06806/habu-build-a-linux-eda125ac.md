---
title: Build a Linux arm64 product engine from current source
status: closed
priority: 2
issue-type: task
created-at: "2026-10-29T20:53:13.790758+02:00"
closed-at: "2026-10-30T01:46:50.999672+02:00"
close-reason: "Landed as master b4c96c08 'Take seeded rows at the interpret row's step' (carl-linux): the interpret row's step takes seeded rows through TOP-INTAKE/TOP-TAKE, CK-AOT-INTAKE! dies when a served miss is not there to take. Gates: gen1 from the public hb and gen2 from gen1 byte-equal (hb and hb.map); aot-payload-graph, top-row-storage-test, top-row-hook-test, outer-interpret rc 0; full suite 498/499 with the one red, lsp-boundary, a 30 s timeout under load that reproduces on the public engine (dotted separately); spark: bootstrap, native-build, self-rebuild and aot-payload-graph rc 0."
---

No route builds one today. Cross-build from the Mac stops at native-build-core.f OUTPUT-LAUNCH-CK (rc 74, by design); spark's installed Habu is stale (E-A-FIRST); tools/bootstrap.sh on spark (gforth 0.7.9) builds a recovery engine whose hb-build --engine fails rc 70, E-UNDEFINED NATIVE-RUNTIME:CAPTURE-PREPARE in save. W3L's snapshot-writer, debugger-resume and self-rebuild rows cannot run on Linux until this is fixed. Root-cause lane in .jj-ws.noindex/carl-linux, brief .out/brief-linux-build.md; proofs on spark.
