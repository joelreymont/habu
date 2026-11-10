---
title: Checked source can store into the global wordlist cells
status: open
priority: 1
issue-type: task
created-at: "2026-11-10T16:21:44.149391+02:00"
---

Problem: checked source can overwrite the cells that hold the global, namespace and types wordlist ids: `data-base BOOT:GLOBAL-WL-CELL + !` succeeds on master. GLOBAL-WL-CELL ($7F0) and, by reading, TYPES-WL-CELL ($2D78) lie outside the protected bands that guard CUR-CELL ($28), WIDN-CELL ($30) and the PKG cells ($78-$90). With habu-a-load-accepts-db95cbfb's shortcut (src/habu/packages.f PKG-WID-SEALED? answers false for the build's global and types wordlists without a walk) a sealed package's wid stored into the cell lets a definition into the sealed package: reproducer and outputs in /Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/seal-rereview.md finding 2. Settled by the long-term rule: the shortcut stays (without it every top-level definition of a build walks the dictionary) and the cells get the protection WIDN-CELL has. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: a hunt names as path:line where the protected bands are defined and checked, and whether protecting these cells lies outside the machinery AGENTS.md says not to extend; then a store from checked source into each of the three cells is refused as a store into WIDN-CELL is, the reproducer is refused E-SEALED-PACKAGE or at the store, and test/seal.f holds a negative case per cell. Files: named by the hunt; test/seal.f. Verify: the reproducer under --load and as a build. Depends: none. Ownership: the engine's protected data. Claim: unassigned.
