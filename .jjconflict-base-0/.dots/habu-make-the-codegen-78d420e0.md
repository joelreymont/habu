---
title: Make the codegen cost comparison survive a loaded host
status: open
priority: 2
issue-type: task
created-at: "2026-07-31T20:43:26.461154+02:00"
---

tools/codegen-compare-baseline.f compares a measured cost with a number recorded on an idle machine, inside a tolerance band of 8x (CODEGEN-COMPARE:COST-BAND). Measured on a 12-core host: with 8 competing busy processes per core CODEGEN-CORPUS:BYTE-FIND came out 7.93x its recorded cost - 2 per cent of margin - and with 16 per core four rows and the pass budget were reported, none of them a compiler change. The scheduled entry therefore runs CODEGEN-COMPARE-CLI:CHECK-EXACT and leaves the cost column and the pass budget out (dot habu-schedule-the-codegen-4e1915bc), so no gate checks the timings at all; only a hand-run bin/hb --load tools/codegen-compare.f does. Wanted: a cost comparison a busy host cannot move, so the column can rejoin a gate. The shape to try: stop comparing with a frozen number and compare with a reference measured in the same pass whose body is the same weight as the subject, the way each path is already normalised against its own empty call - the empty call is 2 ns and finds a clean scheduling window that a 70 ns body cannot, which is exactly why normalising does not cancel sustained load today (the note is in tools/codegen-compare-core.f). The run spread the harness already records per row is the other candidate signal: it explodes under load and could say the pass is untrustworthy without being a per-row verdict.
