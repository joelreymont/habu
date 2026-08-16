---
title: maker.f comment claims an mmap it does not do
status: open
priority: 2
issue-type: task
created-at: "2026-08-17T01:17:37.110282+02:00"
---

bake-chain-20 (2026-08-17): src/habu/maker.f:35 does MK-SOURCE-CAP allot - a DICTIONARY allot - while its comment at maker.f:22 claims the stage2 mmap shape (only stage2.f actually mmaps). The comment is load-bearing misdirection: it hid the Linux DATA-SIZE consequence of the arena doubling. Fix the comment to state the allot and its Linux cost, or convert the allot to the mmap the comment promises - decide by measuring what MK-SOURCE-CAP's consumer actually needs resident.
