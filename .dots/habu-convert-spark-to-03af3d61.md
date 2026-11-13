---
title: Convert spark to the one public Habu layout
status: open
priority: 2
issue-type: task
created-at: "2026-10-31T17:23:15.250477+02:00"
---

Problem: spark (Linux arm64, ssh spark) runs a public hb from 2026-10-02 under the old current symlink layout, and a Zen lane left a revision directory ~/.local/lib/habu/r20261031 there. The public Habu is ~/.local/lib/habu itself, updated in place (docs/bootstrap.md "Stable local command"). Acceptance: on spark, ~/.local/lib/habu holds the tree and bin/hb built from the master that the macOS install carries, with no current link and no revision directory; echo bye | hb answers E-UNDEFINED: bye; phil is told. Ownership: carl. Claim: unassigned.


Seen on spark 2026-11-10: ~/.local/lib/habu is the tree itself, with no current link and no revision directory, and ~/.local/bin/hb links to its bin/hb; that hb and tree are from 2026-11-01 (LESSONS.md and INTEL.md still there), so the build from current master is what remains.
