---
title: Find why crew exited 67 in a full suite run
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T22:05:29.959653+02:00"
---

Problem: the full suite at the stop lane's tip (28b4ae9a, 2026-11-06 22:0x, Mac at load ~17, installed hb 83307dd9-tree) went red on one test: crew (src/lib/crew-test.f) kind=exit code=67; its pool logs were removed with the run's TMPDIR. Rerun alone at the same tip: PASS (134 ms). The stop lane does not reach crew (only a comment in src/lib/error.f is shared). Acceptance: reproduce exit 67 (loop crew in the suite's parallel pool, or the full suite with a kept TMPDIR), name the cause from the kept logs, fix the layer at fault. Verify: the reproducer passes repeatedly. Depends: none. Ownership: carl. Claim: unassigned.
