---
title: Preserve capture prefixes on overflow
status: active
priority: 1
issue-type: task
created-at: "2026-10-15T22:44:01.668784+03:00"
---

Problem: when a child writes more than a capture holds, PROCESS throws E-PROC-TRUNCATED (src/lib/process.f:700-720) and the caller of CMD:RUN-OUTCOME cannot relay what the child wrote before the limit. Maki meets it: src/project.f:515-517 and src/step-source.f:2533 run a build child through CMD:RUN-OUTCOME and read CMD:ERR$ only after it returns, so a build that floods stderr loses its error message. Verify first, since the code has moved: src/lib/process-command.f RUN-CAPTURE (:497-512) now copies PROCESS:OUT-LEN and ERR-LEN into the command before RUN-DONE (:465) throws the code again, so ERR$ and OUT$ may already hold the prefixes after the throw; run a child that overflows one stream, catch the throw and read both. Acceptance: after E-PROC-TRUNCATED, CMD:OUT$ and CMD:ERR$ answer the bytes captured before the limit, each stream's own length, for an overflow of stdout, of stderr and of both; the header of process-command.f says so; a test through the real child path pins it. Maki's own change (catch the code, read ERR$) is Maki's. Files: src/lib/process-command.f, src/lib/process.f, the process-command test. Verify: that test through bin/hb --load test/run.f -- <its name>. Depends: none. Ownership: unassigned. Claim: agent=bob workspace=.jj-ws.noindex/bob-capture (verify-first hunt).
