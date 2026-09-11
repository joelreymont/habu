---
title: Complete native quotation and control-flow support
status: open
priority: 1
issue-type: task
created-at: "2026-09-10T18:03:13.346260+03:00"
---

Owner: Cedar and nominal_pointer. PRODUCT locals, execute result grouping, generic call instantiation, tail stack placement and nested MATCH payload boundaries are integrated. The combined native-product-locals suite and Maki CORE-DATA, PART-ANGLE, SUM-buffer and small APPLY reducers pass. MATCH now consumes instantiated checker payload metadata rather than guessing from field count.

The full APPLY reducer still fails because a package word CELLS ( -- n ) is lowered as core cells ( n -- n ) despite correct checker binding. Repair native binding without application renames. Maki candidate /home/joel/Work/maki/build/maki-release at15608a7d is the eventual complete application smoke; its build/full-entry.f is the temporary in-process helper. Preserve the native-match suite's post-publication emission-reader failure and cast zero-cost assertion failures for real repair. Acceptance is full consumer control/quotation behavior plus rejected programs, not merely the smaller reducers.

Current handoff (2026-09-11): owner Cedar; earlier package CELLS/GEOM/PLACER failures and quoted EXIT are repaired and reviewed. Root integrated RQ/ARG-Q metadataa83fac94 (sourceb169c546): quotation return-stack transport, joins preserving distinct literal bodies, loop replacements and Tender DOCX quote-before-IF shape pass. Frozen KEEP ABI fix931a543c in cedar-return-quotations is NOT integrated; dedicated child habu-fix-generic-keep-7ab33eb0 records current reducers and Rowan review. Its exposed frame-token failure is habu-fix-spill-frame-e429b835, owned compiler_xhigh_review. Keep original complete-consumer/rejection acceptance: reductions alone are not full application or selfbuild acceptance.
