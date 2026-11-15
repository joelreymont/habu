---
title: Make SIG-COUNTED a stated outcome
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T12:50:22.228768+01:00"
closed-at: "2026-11-16T04:07:35.548266+01:00"
---

Problem: checker.f SIG-COUNTED (~:11089) is a flag set in one place and read in another, through the native compiler's interface. Acceptance: the outcome it carries is one named state the reader receives, no flag. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-w40sig.
