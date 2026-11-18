---
title: Never split a character at a cut
status: active
priority: 2
issue-type: task
created-at: "2026-11-18T13:16:26.480091+01:00"
---

Problem: src/lib/utf8-scalar.f FIT (:252-259) tests whether the byte at the cap is a continuation byte, so a valid 4-byte character ending exactly at the cap and followed by a stray continuation byte is split: 28 A, F0 9F 98 80, 80, B with cap 32 answers 29. Acceptance: FIT cuts at the start of the character that crosses the cap; a test in the utf8-scalar tests pins that input and fails first. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-fit.
