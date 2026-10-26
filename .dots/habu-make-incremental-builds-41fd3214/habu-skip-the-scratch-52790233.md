---
title: Skip the scratch retire for a refused file
status: closed
priority: 2
issue-type: task
created-at: "2026-10-26T19:10:40.289675+02:00"
closed-at: "2026-10-26T19:10:54.014009+02:00"
close-reason: "Not reproduced: probe /Users/joel/.cache/tmp/carl-file/edge-probe/probe.f (child DECL-EVENT:OPEN drop then an undefined word, required by a parent, built at tier 1 with capture) on the roqvyyxu engine gives the child's E-UNDEFINED and no die; the parent is never refused, so LOAD-FINISH with ok false is not reached on this path. Fold dropped."
---

Edge re-review item 4: IMAGE-TAKE's SCRATCH-RETIRE dies on a refused child with a leaked DECL-EVENT:OPEN, hiding the child's error. Workspace carl-edge, change vzuunzpr on roqvyyxu (squash into the edge at landing); brief /Users/joel/.cache/tmp/carl-file/brief-edge-fold.md. Worker running.
