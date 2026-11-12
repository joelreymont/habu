---
title: Bound the objects an out keeps for old engines
status: open
priority: 3
issue-type: task
created-at: "2026-11-11T15:03:31.549160+02:00"
---

Problem: src/lib/build-out.f OBJ$ (:44) keys each out's objects by engine identity (<out>/obj/<ENGINE-ID:KEY$>/), and nothing ever removes another engine's directory, so every out root grows with each engine built or installed (the installed tree's .out held 940 engine directories, 3.6 GB of objects, on 2026-11-11). An old engine's objects are never read by a new one. Acceptance: an out keeps a bounded set of engine directories, and a build never removes a directory another running build is using. Needs a design (concurrency: two engines building into one out at once). Depends: none. Claim: unassigned.
