---
title: Engine words that read a pointer carry no memory class
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T20:13:39.605230+02:00"
---

Problem: these checked-callable rows in src/habu/prims.f take a pointer, carry no REACH class and may read it in the process, so a known null passed to them is not refused and may end the process (tok-imm? and search-wl do, rc 139, and are fixed with habu-checked-src-can-88e8a977): scope-find (:1054), policy-admit (:1031), run-in-stack (:568), evaluate-closed (:573), die (:593), spawn-io (:621), spawn-fds (:630). Read, not probed. Found by the known-null design (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/known-null-design.md, issue 2). Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: each of the seven is probed with NULL-PTR on the engine; each that reads memory at its pointer carries REACH memory and is refused at its own token; each that does not is listed with the run that shows it. Files: src/habu/prims.f, test/prim-parity.f. Verify: one refused case per classed word, failing before. Depends: habu-checked-src-can-88e8a977 (the reach rule lands first). Ownership: the memory class of these rows. Claim: unassigned.
