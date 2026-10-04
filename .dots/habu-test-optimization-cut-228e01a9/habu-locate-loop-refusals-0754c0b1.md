---
title: Locate loop refusals in a loaded file
status: open
priority: 3
issue-type: task
created-at: "2026-10-04T04:41:56.218105+03:00"
---

Problem (lane 573, on 7c03e870): refusals that end through LDIAGRET in src/habu/habu2.f (undefined word, a closer without its opener, a structure-kind mismatch such as `0 if [: then ;]`, the nesting cap) name only the token, with no file or line, even when the source is a loaded file; 136425ef's tests pin each closer refusal to its token. Dot 7a137417 asked for the quotation's name and location at `;]`; this is that location for every LDIAGRET refusal. Fix: LDIAGRET renders file:line:col for a loaded source, as located refusals elsewhere do, with the definition name where one is open. Acceptance: each listed refusal from a loaded file names file:line:col and the open definition, seen unlocated first; runtime-regression-test GE-CF-KIND and the closer pins updated to the located form; baked: rebuild, g1 == g2 with .names, two-generation build. After: 7a137417.
