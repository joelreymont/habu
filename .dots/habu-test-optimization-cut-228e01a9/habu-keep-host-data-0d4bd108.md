---
title: Keep host data out of the built image
status: open
priority: 2
issue-type: task
created-at: "2026-10-01T11:32:11.928163+02:00"
---

Problem: docs/bootstrap.md:322-331 says a change outside codegen gives gen1 == gen2, and that the unmodified tree built by a new engine returns the shipped engine byte for byte. Measured in round 4: r4-tbuf's g1 (a checker-only change: storage refusals) built the unmodified base tree into an engine 17,041 bytes different from g0, .names equal, every difference in image data after the code; r4-tbuf g1 vs g2 differ by 23,054 bytes and r4-lexrec commit 2 (VREC field refusal) g1 vs g2 by 12,153 bytes from offset 1716879, past the code end (code 143252 + 1392116). Some captured data comes from the host. Acceptance: reduce to the smallest host change that moves the output (master's engine plus one variable, one record, one checker row); name the captured bytes that come from the host and the capture rule that copies them; fix the capture so master's tree built by every such host is byte-identical to master's engine with an identical .names; a fixture through the real build path (the two-generation-fixtures row or a new one) seen to fail before; docs/bootstrap.md states the measured case beside the wid and layout-constant ones. Files: tools/native-emit.f, src/habu/aot-*.f, whatever the reduction names. Verify: the reduction before and after, two-generation build, the fixtures row.
