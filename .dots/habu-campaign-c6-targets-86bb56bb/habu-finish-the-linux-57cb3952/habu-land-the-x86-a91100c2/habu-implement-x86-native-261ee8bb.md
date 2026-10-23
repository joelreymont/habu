---
title: Implement x86 native breakpoints
status: open
priority: 2
issue-type: task
created-at: "2026-10-22T13:12:42.383912+03:00"
---

Intel integrated and pushed pwsnnpqn4407e672 through d8984f19. Independent adversarial review found no findings; native build, full unchanged gate-debug and gen5/gen4 byte identity pass. Combined prof-rewind/prof-index/gate-debug pass in one process. Persistent empty/recursive probes passed; no tests changed. Pulsar workspace and outputs retired. Mac ARM check done at intel dfa5151b: ARM engine built twice rc 0, two-gen "ok gen 5 matches gen 4 byte for byte", run.f 501 of 501 with one red, compiler-native-hookless-reject (master 569dc3bf, resolved by the rebase). OPEN: full integrated native census pending image provider. Separate PTY tier1 matcher decision is habu-resolve-pty-tier-5105302d.
