---
title: Name the operation and errno in a staged cleanup line
status: closed
priority: 2
issue-type: task
created-at: "2026-11-12T18:13:25.408467+02:00"
closed-at: "2026-11-12T18:46:14.836946+02:00"
---

Problem: src/lib/fs-mutate.f STAGED-REPORT (:562) prints 'hb: staged replacement <label> threw <n> at <path>'. ATOMIC-ABANDON (:655) and REPLACE-STAGED (:1046) both pass the label 'cleanup', and ATOMIC-ABANDON is no staged replacement, so the line cannot say which operation failed. It prints the code as a bare number with no name and no errno. The deliberate case FMT-TEST-STAGED-PRIMARY (src/lib/fs-mutate-test.f:626-641) lets the line out unchecked. Acceptance: each caller's line names its own operation and gives the code's name where ERROR:CODE declares it, else its number (habu-convert-every-err-7e7824a1 declares the FS codes), and the errno; the case asserts its line. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/staged-line.
