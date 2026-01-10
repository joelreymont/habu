---
title: Add copy-tree function
status: open
priority: 2
issue-type: task
created-at: "2026-01-10T11:24:45.524406+02:00"
---

Missing from stdlib. Add to lib/stdlib.habu.
Pattern: (copy-tree tree) -> deep copy of tree structure
Recursive: (if (consp tree) (cons (copy-tree (car tree)) (copy-tree (cdr tree))) tree)
Test: (copy-tree '((1 2) (3 4))) => ((1 2) (3 4))
