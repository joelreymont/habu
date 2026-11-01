---
title: "Card: when a file can open a package"
status: open
priority: 3
issue-type: task
created-at: "2026-11-01T10:26:42.448081+02:00"
---

docs/forth-card.md section 1 says a package opens 'in the file or string A is open in'. Measured on the engine (fixtures /tmp/jerry-disc-rv2-tmp/r3/f/): a file can open a part inside a package another file opened (h1b: entry.f opens DEMO:P inside DEMO opened by x.f; P:X works, E-UNDEFINED once DEMO closes; h2 one load deeper), and a file cannot open a package while one that was open when it started loading is still open (h3, g1, g2, i1, i2, e1/e2/e5/e6/e7: 'package at k.f:1', rc 75). State the rule in the card and docs/forth.md at the heading that holds package scope. Workspace: none yet.
