---
title: Complete native quotation and control-flow support
status: open
priority: 1
issue-type: task
created-at: "2026-09-10T18:03:13.346260+03:00"
---

Owner: Cedar and nominal_pointer. PRODUCT locals, execute result grouping, generic call instantiation, tail stack placement and nested MATCH payload boundaries are integrated. The combined native-product-locals suite and Maki CORE-DATA, PART-ANGLE, SUM-buffer and small APPLY reducers pass. MATCH now consumes instantiated checker payload metadata rather than guessing from field count.

The full APPLY reducer still fails because a package word CELLS ( -- n ) is lowered as core cells ( n -- n ) despite correct checker binding. Repair native binding without application renames. Maki candidate /home/joel/Work/maki/build/maki-release at15608a7d is the eventual complete application smoke; its build/full-entry.f is the temporary in-process helper. Preserve the native-match suite's post-publication emission-reader failure and cast zero-cost assertion failures for real repair. Acceptance is full consumer control/quotation behavior plus rejected programs, not merely the smaller reducers.
