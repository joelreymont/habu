---
title: Census tests that launch processes or wait without an end
status: closed
priority: 2
issue-type: task
created-at: "2026-10-30T20:40:26.485185+02:00"
closed-at: "2026-10-30T20:47:21.727122+02:00"
close-reason: Census A /tmp/jerry-big/inproc-census-a.md (child launches, variants, orphans; raw ca-*.txt) and census B /tmp/jerry-big/inproc-census-b.md (waits; raw censusb-*.txt). Findings filed as sibling dots under habu-run-tests-on-f7e9e850 and appended to habu-remove-the-test-6fc078db (orphans across exec, deadline rendezvous).
---

Problem: see parent. Deliverable /tmp/jerry-big/inproc-census.md, read-only (no edits, builds or suite runs). (a) Every test or tool test that runs hb, a built image or a bundle variant as a child (RUN-IMAGE, E2E-TREE, PROC-ARGV with ENGINE-ID:PATH$, COMPILER-IMAGES, C2-IMAGE-PAIR, HB-BUILD), per file:line: what the child proves; whether a process boundary is the subject (image save and restart, exec, exit code, signal, tty/stdin, process state fixtures cannot reset); if not, how the case runs in the suite's process on bin/hb. (b) Every consumer of the compiler-images bundle (hb-whitebox-source, hb-root, hb-ordinary, their maps) and of NATIVE-EMIT:WRITE versus WRITE-C2: what each variant proves that bin/hb does not, and whether anything that ships uses WRITE. (c) Every test wait that ends only on a flag or count a worker that ended early never sets (begin ... while TASK:PAUSE repeat, bare spins, PROCESS:WAIT-FOREVER children whose subject can block). (d) Spawns whose children outlive a killed suite (own process group, no link to the parent's death), cross-referenced with timers lane 1 (dot habu-remove-the-test-6fc078db). Acceptance: the report with file:line evidence, a proposed fix per row, and an order. Files: none owned. Verify: the report. Depends: none. Ownership: none. Claim: agent=jerry workers census-a (child launches, variants, orphans) and census-b (waits), read-only on the root checkout; briefs /tmp/jerry-big/inproc-census-{a,b}-brief.md.
