---
title: Fix maki competitive-evidence uncaught 7169
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-30T14:12:00.333003+02:00\""
---

Full context: the maki suite (bin/hb --load maki/test.f) is a required master-merge gate and is red on the proofs branch at commit c9c11d96: 342 phases pass, then maki/competitive-evidence-test.f dies with hb: uncaught throw code 7169 and the suite exits 67. Not yet attributed: run the file standalone, decode 7169 (rg the code and its block owner in lib/errors.f and src/core), and bisect whether it reds on master@origin too (pre-existing) or was introduced by a proofs-branch landing - the checker changes (fail-closed package reject, ptr-elem merge) and the type-family work are the suspects if branch-introduced. Follow the single-site unique-code mutation technique from LESSONS.md to pin the throw site. An uncaught four-digit throw reaching top level is also a diagnostic gap on its own (compare CHECKER-PKG-CONTEXT-REJECT precedent). BLOCKS the master fast-forward.

Claim: agent=makilane workspace=.jj-ws/habu-fix-maki-competitive-7dc29ec2
