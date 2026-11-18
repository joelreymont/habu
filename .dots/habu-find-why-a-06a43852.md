---
title: Find why a warm engine build compiles three layout objects again
status: active
priority: 2
issue-type: task
created-at: "2026-11-18T23:34:17.891863+01:00"
---

Problem: an engine build run twice with one TMPDIR (the data-base stack with tuzonynu; hb2 building --engine into hb3, then hb3w) writes 3 of 480 objects again on the second run: src/os/linux/layout.f.2.hfo, src/os/linux-x86-64/target-layout.f.hfo and target-layout.f+1.hfo; the output bytes are the same. Whether master does it is not known. Acceptance: the cause is named with the code that makes those pieces compile again, and a second warm build writes no object. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-wr.
