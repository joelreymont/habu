---
title: Keep the data stack pointer pending in T0 on x86
status: active
priority: 2
issue-type: task
created-at: "2026-11-25T17:23:50.561343+01:00"
---

Problem: x86-64 has no writeback, so each push or pop T0 emits outside a spill run is a move and a 4-byte lea of rbx; tenderd has 197,887 leas, 4 bytes each, 17% of its code, and x86 code is 13% larger than arm64's (4,722,684 against 4,163,392 bytes). A value evicted one at a time moves the pointer each time, and a pop followed by a push before the next call moves it twice for nothing (25,701 blocks net zero). A count of the leas a block-long pending offset saves is 249 KB in tenderd. Acceptance: T0 keeps the offset between rbx and the stack's top at compile time inside a straight run, writes it once before a call, a branch or a label, the apps' x86 code shrinks by that much, code-bench is no slower on either host, and arm64's code does not grow. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-x-dsp2.
