---
title: Carry intact-input evidence through a callee locals frame
status: open
priority: 3
issue-type: task
created-at: "2026-09-23T17:09:36.829956+03:00"
---

The callee mask (c2923193) records which declared inputs a throwing word leaves intact, but 18 of the 39 callees migrated for catch-stale bind their inputs to locals first, and a locals frame owns a fresh base row, so their mask is all-stale: conservative, not wrong, and those sites keep their pre-catch locals. Model the frame: a bound input the body never writes back is intact at every throw, so the mask can be computed over the definition's SGIN/SGRIN window before the frame is entered. Measure on the 18 sites in the callee-mask lane's cm/callees.txt.
