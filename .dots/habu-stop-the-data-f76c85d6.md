---
title: Stop the DATA text-pointer scan refusing correct images
status: open
priority: 2
issue-type: task
created-at: "2026-10-29T09:58:41.876816+02:00"
---

AOT-DATA-TEXTPTR-CHECK (src/habu/aot-common.f:217) walks DATA in unaligned 8-byte windows. A window that straddles a cell holding 2 (e.g. TCB.EXIT-SLOT) and a following zero cell reads 0x200000000, which it takes for an undeclared text pointer, so it refuses a correct image. Repro: phil-tasknew/tmp/one/subjE.f (found by phil). Find out why that window is not excused (AOT-CELL:NONCELL-WINDOW?, CELL-LITERAL?, and the CELL-TEXTPTR? range: is 0x200000000 inside text?), and fix that layer. Prove it with the repro as an e2e image build. Keep the existing refusal tests red.
