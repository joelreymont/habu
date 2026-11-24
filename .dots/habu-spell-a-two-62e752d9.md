---
title: "Spell a two-colon FUNCTION: name as written"
status: active
priority: 2
issue-type: task
created-at: "2026-11-24T19:06:20.587752+01:00"
---

Problem: PROCESS-SYMBOLS, FUNCTION: A:B:C getpid ( -- n ), ;FUNCTION: the check's E-BAD-QUALIFIED-RECORD names the word a:b:c, folded, where the load names A:B:C as written. Acceptance: the check's record spells the name as the source wrote it, as the load does; a test holds it. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-qspell.
