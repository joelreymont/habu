---
title: Make the plain-FPRIM internals private
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T23:00:56.862441+01:00"
---

Problem: five engine-internal primitives are registered with plain FPRIM into WL:PRIMITIVES (code-publish, xref-retarget, wide-mark and run-rc in src/habu/habu1.f, does-record in src/habu/habu2.f), so ordinary source finds them: after `s" code-publish" s" ptr u8 n n --" trust` a checked caller outside NPUB compiles code-publish, rc 0, and xref-retarget, does-record and run-rc likewise; `['] code-publish` takes the trusted effect; wide-mark has a global row, so `: QA ( -- ) wide-mark ;` certifies anywhere. wide-mark's checker caller REC-WIDE-PUBLISH (src/core/checker.f) is in no package. Acceptance: each is private to the packages that call it (run-rc's home is PROCESS; src/lib/process-command.f and src/lib/process-task-test.f call PROCESS's public word); each probe answers E-UNDEFINED; `package QS : code-publish ( -- n ) 5 ; : QB code-publish ;` still compiles. Depends: none. Claim: unassigned.
