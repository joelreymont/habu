---
title: Adjudicate stranded option-promotion commit
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-18T20:59:31.851501+02:00\""
---

The local bookmark sol-fields-add-shared points at unmerged commit 4208263c (2026-07-16, subject: Promote shared option family). It moves lib/adt/option.f into src/core/option.f, adds test/core-option-suite.f, and touches src/habu/habu1.f, src/habu/habu2.f, bootstrap/cg/forth.fs, and test/boot-pin-test.f - engine and seed surface. None of that landed: src/core/option.f and test/core-option-suite.f do not exist on master, and master's later option work (the option-of-n finder wave, OBJIDX and RX migrations) went a different route. Task: rebase the commit onto current master, review whether promoting the option family into src/core is still the right architecture given how the landed option wave turned out, then either land it through the normal review and gate pipeline (seed-affecting: byte-fixpoint proof required) or retire it with a written reason recorded here. A divergent twin snapshot c606788a with identical content should be abandoned during cleanup; two stale maki-layout-valid snapshots (2114b150, 50f5ca2f - lane content already on master in rebased form) also await abandon. Do not delete the bookmark until the commit is landed or retired.

Claim 2026-07-18: agent=opt-opus workspace=.jj-ws/habu-adjudicate-stranded-option-cace4864 (rebase + adjudication analysis; the landing decision returns to the orchestrator).
