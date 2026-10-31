---
title: Convert spark to the one public Habu layout
status: open
priority: 2
issue-type: task
created-at: "2026-10-31T17:23:15.250477+02:00"
---

Problem: spark (Linux arm64, ssh spark) runs a public hb from 2026-10-02 under the old current symlink layout, and a Zen lane left a revision directory ~/.local/lib/habu/r20261031 there. The public Habu is ~/.local/lib/habu itself, updated in place (LESSONS 2026-10-28). Acceptance: on spark, ~/.local/lib/habu holds the tree and bin/hb built from the master that the macOS install carries, with no current link and no revision directory; echo bye | hb answers E-UNDEFINED: bye; phil is told. Ownership: carl. Claim: unassigned.
