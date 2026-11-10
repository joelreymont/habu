---
title: "Load the consumers on master's FSM generator"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T08:34:57.021318+02:00"
---

Problem: fd307801 (NAME-STEP) and 9f6510cc (every action held to its context effect) changed what FSM:DEFINE generates for every table, and the installed tree's src/lib/fsm.f (~/.local/lib/habu) is still b7273a0's, so no table in Tender, Zen or Maki has loaded through the new generator. A table whose callback breaks its declared effect now fails to load; nothing has shown whether any does.
Acceptance: with master's src/lib/fsm.f, Tender (tender.f and tenderd/entry.f), Zen and Maki each load. Every refusal is recorded here with its table and callback and goes to its owner as a dot. Once all three load, the installed tree is updated in place to master.
Files: none in src; the installed tree.
Verify: the three loads' output and exit status, recorded here.
Depends: none.
Ownership: the installed tree; a consumer's defect belongs to its owner. Claim: unassigned.

The installed tree's src/lib/fsm.f equals master's and generates the step word, so the premise that the installed tree is old no longer holds. What remains is a recorded load of each consumer.
