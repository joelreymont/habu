---
title: Keep the fixpoint stamp under the out root
status: closed
priority: 2
issue-type: task
created-at: "2026-10-27T14:10:12.827382+02:00"
closed-at: "2026-10-27T19:58:53.273267+02:00"
close-reason: "STAMP ozowtzvo landed on master d81d39e9; integrated proof: engine, two-generation, bootstrap check rc 0, suite 501/502 (engine-stack-machine red on master, dot ed0ee303)"
---

Problem: tools/build-fixpoint.f:1525-1557 BF-STAMP-DIR$ puts the refresh stamp under XDG_CACHE_HOME, else ~/.cache/habu-fixpoint, else TMPDIR, with HABU_FIXPOINT_STAMP as an override. Joel's rule: agents and tools of this repo never write under ~/.cache; build output lives under the out root (HABU_OUT, default ./.out; design ruling in carl-dots/.out/carl-file/design-build-out.md). Acceptance: the stamp default is <out root>/fixpoint/stamp through tools/build-out.f's out-root word; the XDG/HOME/TMPDIR chain and HABU_FIXPOINT_STAMP go (tests set HABU_OUT instead: tools/build-fixpoint-test-lib.f:298, tools/build-fixpoint-test.f:40); docs/bootstrap.md and .gitignore comments that name HABU_FIXPOINT_STAMP updated. Files: tools/build-fixpoint.f, tools/build-fixpoint-test.f, tools/build-fixpoint-test-lib.f, docs. Verify: bin/hb --load tools/build-fixpoint-test.f; rg HABU_FIXPOINT_STAMP empty. Depends: OUT1 (Store file objects under the tree's .out). Ownership: unassigned. Claim: unassigned.
