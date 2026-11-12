---
title: "Give a build's wordlist ids back at its close"
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T03:06:34.155102+02:00"
---

Problem: LINKER:WITH's MOVE-IN takes three fresh wordlists per build (src/habu/linker.f:4714-4715) and a build's packages take more, but the close does not lower WIDN (src/habu/layout.f:222 WIDN-CELL; test/linker.f CLOSE-LEFT?, :63-67). prot-wid-add refuses a wid at or above PROT-WID-MAX 8192 (layout.f:490-503). Measured 2026-11-06: startup-list rows 0-60 and 64-67 loaded inside one build took 227 wordlist ids, none given back, so about 33 such builds fit in one process; the rest of the list was not counted. A long-lived hb that builds repeatedly (agent iteration) is the case. Acceptance: measure the wids one startup-list build takes; then either the build's wordlist ids come from a range the close gives back, or the bound is shown out of reach by the measurement. Depends: none. Claim: unassigned, owner jerry.
