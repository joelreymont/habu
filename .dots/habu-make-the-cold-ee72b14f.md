---
title: Make the cold-engine closure pass source preverify
status: open
priority: 2
issue-type: task
created-at: "2026-09-17T02:57:55.420901+03:00"
---

Problem: `bin/hb --load tools/check.f -- test/cold-argv-separator.f` reports `source preverify failed` on lib/type/deftype.f NG-RESET (declared `--`, inferred `ptr n --`), and the identical failure appears for the untouched test/cold-engine.f, so it belongs to that closure: a checked module in the tree whose declared effect disagrees with its body under preverify (double-load lane, 2026-09-17). Acceptance: NG-RESET's declaration and body agree (fix whichever is wrong, with the reason), tools/check.f passes on test/cold-engine.f and test/cold-argv-separator.f, and tools/check.f over the tree reports no preverify failure. Files: lib/type/deftype.f, test/. Verify: tools/check.f on the two files; lib/type tests. Depends: none. Ownership: lib/type. Claim: unassigned.
