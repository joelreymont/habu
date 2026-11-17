---
title: "Show expected as the word's row in a mismatch"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-17T23:03:37.952459+01:00"
closed-at: "2026-11-18T07:34:58.640228+01:00"
---

Problem: an E-MISMATCH's expected row borrows the found row's extra items: /tmp/jerry-str/probe/p2-typedvar.f prints '`!` takes ^u8 str ^str; the stack holds ^u8 n ^str' and p9-unmarked-caller.f prints '`PK:LEN` takes ^u8 str' for a word declared ( str -- n ); neither word takes a ^u8. Acceptance: the record's takes/expected line shows the word's own input row (str ^str, str), with found aligned to it; a program-diagnostics test fails first. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-mmrow.
