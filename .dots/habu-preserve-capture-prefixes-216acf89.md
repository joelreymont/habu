---
title: Preserve capture prefixes on overflow
status: open
priority: 2
issue-type: task
created-at: "2026-10-15T22:44:01.668784+03:00"
---

Jorge reports PROC-CMD:RUN-OUTCOME resets output lengths, then the lower process capture loop throws E-PROC-TRUNCATED before STORE-RUN. ERR$/OUT$ are therefore empty although their buffers contain the captured prefixes; timeout already preserves its capture. Maki src/serve.f catches the overflow while building a design and cannot relay the child diagnostics. Preserve the actual stdout/stderr prefix lengths on overflow at the responsible process-command/capture boundary, including cleanup and the original throw. Inspect existing process command/capture behavior tests first and use the real child-output path; verify separate stdout/stderr and simultaneous caps, with no fabricated lengths. Existing Maki test/serve.f and test/loud-exit.f provide downstream acceptance by the owner. Queue after current incremental-build fixes; send the qualified public prefix when ready.
