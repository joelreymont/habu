---
title: "Find why a program build's require check missed sys.f's require of icode.f"
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T10:46:14.042736+01:00"
---

Problem: with the require check (jerry-reqcheck, engine /tmp/jerry-p1/reqcheck/g1), engine-stack-machine's 'stripped application build' failed once in a parallel suite run: E-UNREQUIRED LIT64, at src/os/linux/sys.f:94, though sys.f:6 requires src/arch/arm64/icode.f (log /tmp/jerry-p1/reqcheck/suite3.log:5-29); it passed on rerun. Acceptance: the cause named with a reproducer that fails every time, and the check fixed there. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-reqhunt.
