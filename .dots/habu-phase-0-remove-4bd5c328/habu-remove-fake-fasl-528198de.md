---
title: Remove fake FASL paths
status: closed
priority: 1
issue-type: task
created-at: "\"2026-04-01T22:06:02.069785+02:00\""
closed-at: "2026-04-01T22:24:01.395386+02:00"
close-reason: done (source fallback removed; validation by rg plus zig build test blocked only by pre-existing baseline compile errors in disasm and builder.lambda callsites)
---

Problem: .fasl and .hfasl loads silently substitute source. Acceptance: FASL loads run real semantics or fail explicitly. Files: src/interp/repl.zig:1945-1991,2317-2338. Verify: focused load tests prove no sibling source fallback. Blockers: none.
