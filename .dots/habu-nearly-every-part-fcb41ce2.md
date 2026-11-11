---
title: Nearly every part records the working directory
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T16:58:41.722546+02:00"
---

Problem: every fresh require reaches src/core/include.f CANDIDATE, ENGINE$ and CWD-RECORD, so each part that ends in a fresh request records `.` -> cwd (COMPILE-READS ROOT-READ), also when hb runs outside a Habu tree and the engine root came from the executable. Two builds of the same tree and absolute HABU_OUT from two working directories recompile every such part; before root reuse they linked. The request and each act are resolved again at check, so the fact adds no soundness for resolution (root-facts feature review, finding 3). Acceptance: a part records the engine root's answer only when its compile used it, or a narrower fact (a new read kind is a saved-format change: hard cutover); building one tree from two directories into one out root links warm. Files: src/core/include.f, src/core/compile-reads.f, src/habu/linker.f. Verify: test/aot-shifted-bytes.f case building from a second directory prints no top-level line. Depends: habu-reuse-the-obj-a450cc5b landed. Ownership: those files. Claim: unassigned.
