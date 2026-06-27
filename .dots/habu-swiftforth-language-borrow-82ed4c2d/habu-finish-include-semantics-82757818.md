---
title: Finish include semantics and validation
status: open
priority: 1
issue-type: task
created-at: "2026-06-28T00:10:32.144600+02:00"
---

Files: src/core/include.f, src/habu/habu2.f, bootstrap/cg/forth.fs, tools/bootstrap-codegen-test.f, test/gate-dictionary.f, docs/forth.md. Root cause: include/included exist, but native fixpoint still fails in the bootstrap path and the cold prefix must prove checker/render/hook load before signed target/roles/include sources that publish through trust. Fix: finish the prefix ordering RCA, refresh bin/hb, verify nested include + included + package reopen duplicate failures through the current native gate, and document include as source composition rather than namespace sharing. Why: multi-file packages need include, but include cannot be trusted until bootstrap and package gates pass on the refreshed engine.
