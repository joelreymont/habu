---
title: Delete dead error codes
status: open
priority: 2
issue-type: task
created-at: "2026-10-15T17:16:31.044021+03:00"
---

Problem: src/lib/errors.f:125-156 holds 30 PTX, KABI and KEXPORT constants with no reference (residue of the removed Loom work); 44 dead codes in total. Acceptance: every code with no reference in src, tools, test, bootstrap or ../etch deleted; compiler codes stay where they are (tier 1 is being replaced). Files: src/lib/errors.f. Verify: rg; suites that load errors.f. Depends: carl-final-core chain landed on master (it edits these files); start from master after it lands. Ownership: src/lib/errors.f. Claim: unassigned.
