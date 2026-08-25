---
title: Assert the DATA band clear of code regions at boot
status: open
priority: 2
issue-type: task
created-at: "2026-08-11T08:03:13.009964+02:00"
---

habu2.f commit-6 prose argues DATA-VA sits far above RBASE-VA+REGION and above __text+REGION-OFF, so an ADDRMAP-recorded DATA chain is never region-dependent - but the disjointness is argued from constants, not enforced. Today an overlap fails indirectly: EMIT-ADDRS would rewrite a DATA chain and the loader's chain-shape check catches the wreckage. Candidate fix: a boot assertion beside the BL-RANGE-RC one stating the disjointness where it is relied on. SIMPLIFY GATE FIRST: before building it, produce a failing probe through the real gate (a layout-constant mutation that moves DATA into a code region and gets caught only downstream or not at all); if the indirect failure is already loud and prompt on every path, record that as the answer and close without code. Files: src/habu/habu2.f, src/habu/layout.f. Depends: reloc commit 6 merging (dot bb9b6d70).
