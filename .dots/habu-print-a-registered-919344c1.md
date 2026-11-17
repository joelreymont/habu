---
title: Print a registered code on an uncaught throw
status: active
priority: 2
issue-type: task
created-at: "2026-11-17T10:48:16.986443+01:00"
---

Problem: an uncaught throw of a code registered with ERROR:CODE prints its record from main.f RUN-SOURCE, but three roots still print only 'hb: uncaught throw code N' (mark, spark:/tmp/mark-habu-uncaught.f): a forked child (src/lib/process-fork.f CHILD-MAIN), an exit action (src/lib/at-exit.f RUN, reached through LUNCAUGHT) and RUN-NOW's REPORT (at-exit.f). Acceptance: each prints the code's record as RUN-SOURCE does, with the same exit status. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-mu2.
