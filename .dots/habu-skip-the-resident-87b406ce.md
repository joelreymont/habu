---
title: Skip the resident library when check.f runs it as a program
status: open
priority: 3
issue-type: task
created-at: "2026-09-23T15:20:00.396308+03:00"
---

tools/check.f lib/string.f (engine fa2fe032 on 548facd2) preverifies and then runs a child whose run.f requires lib/string.f a second time: duplicate definition: STR-TAB at HB_TMP/habu-check-*/run.f:20, rc 78. tools/check-core.f loads lib/string.f itself, and in --source-list mode it already knows the resident inputs (check-core.f:632 all source-list inputs are already provided). Acceptance: a program that is one of check.f's own resident sources gets the same resident treatment in the prog.f mode (preverified, no duplicate child load) or a named refusal that says so; a row in the suite that owns check.f rows. Depends: none. Ownership: hazel. Claim: unassigned.
