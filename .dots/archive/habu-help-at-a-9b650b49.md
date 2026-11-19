---
title: Help at a mismatched constant
status: closed
priority: 2
issue-type: task
created-at: "2026-11-19T13:53:39.789595+01:00"
closed-at: "2026-11-19T14:44:07.747601+01:00"
---

Problem: a nominal value stored by `constant`, `!` or `,` into an undeclared cell (`QF:MK constant C`, QF:MK leaving a NEWTYPE) is a bare E-MISMATCH whose help says only to give `constant` a, though a rejection says how to fix the issue (docs/forth.md diagnostics). Acceptance: the rejection names what `constant` takes and what to do instead. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-rawf.
