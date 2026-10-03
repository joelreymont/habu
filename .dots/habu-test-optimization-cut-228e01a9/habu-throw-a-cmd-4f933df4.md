---
title: Throw a CMD deadline from RUN-RC
status: closed
priority: 2
issue-type: task
created-at: "\"2026-10-01T11:07:52.671962+02:00\""
closed-at: "2026-10-01T13:07:37.530437+02:00"
close-reason: Fixed by xquusqom bb01a7cd (reviews ACCEPT)
---

Problem (r4-wblabel commit 2 report): lib/process-command.f RUN-RC and RC@ (:365-371, :477-486) give err(137) for a deadline, the SIGKILL that reaped the child, so tools/chain-run.f:44 and tools/chain-plan.f:106 turn an expired deadline into E-BUILD-STATUS, lib/process-command-test.f PCMDT-PROC-RUN-RC/PCMDT-H-RUN-RC read it as an assertion, and only tools/zed-run-lib.f (TIMED-OUT? :112, :169, :173) tells it apart. This breaks the rule commit 2 sets: a reader that turns an outcome into a status throws E-PROC-TIMEOUT. Acceptance: RUN-RC and RC@ throw E-PROC-TIMEOUT for a deadline; RUN-OUTCOME and OUTCOME@ keep it as data; zed-run maps it at its own boundary or drops E-ZED-TIMEOUT if it means the same; process-command-test.f:112 asserts the throw; docs/stdlib.md states it. A forced 1 ms deadline through chain-plan shows E-PROC-TIMEOUT, before and after. Files: lib/process-command.f, lib/process-command-test.f, tools/zed-run-lib.f, tools/chain-run.f, tools/chain-plan.f, docs/stdlib.md. Verify: lib/process-command-test.f, lib/process-task-test.f, the zed-run and chain tests.
