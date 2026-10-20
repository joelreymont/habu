---
title: Chunk pool reach source filler
status: closed
priority: 2
issue-type: task
created-at: "2026-10-20T14:56:27.486862+03:00"
closed-at: "2026-10-20T15:07:48.708974+03:00"
close-reason: "Replaced 27000 branch-word definitions with 2048 linked chunks of 128 straight-line increments. Existing stripped-pool-reach E2E passes using a02380d3/map1607423e: 83.25 s wall, 81.79 s user, 1.13 s sys. Retained ordinary-source artifact has 1109924 code bytes (>1048576), eight SHARED DATA loads, 262144 reachable increments and exact result 262168/reach=ok. Generated bodies are at most 407 source bytes. Source, image, map and logs retained in external scratch for integrator acceptance and independent review. No compiler, deadline or capacity changes."
---

Own test/stripped-pool-reach.f. Replace 27000 tiny linked definitions and per-step file appends with 2048 bounded linked chunks of 128 straight-line increments. Preserve ordinary source and native linking, eight MAIN DATA loads, code range greater than 1048576 bytes, exact result 262168 and reach=ok runtime output, deadlines and cleanup. Measure existing E2E with the matching a02380d3/map1607423e engine, external scratch/cache, stdin EOF and file-descriptor limit 512; retain generated source/image evidence until integration. No compiler, capacity, deadline or raw-code changes.
