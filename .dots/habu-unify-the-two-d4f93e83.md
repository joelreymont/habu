---
title: Unify the two register-allocation walks
status: open
priority: 2
issue-type: task
created-at: "2026-08-02T10:37:50.386473+02:00"
---

src/compiler/native/regalloc.f and src/compiler/native/regalloc-verify.f each have two walks: a straight-line one for a routine of one block and a general one for the rest, and they number a routine's positions differently - within the block for the first (block arguments at ENTRY, operation i at i) and across the whole routine for the second (block b's arguments at VB-ST[b], operation i at VB-ST[b]+1+i). Because the numbering is part of what the validator re-derives, the two files must send a routine the same way, so the dispatch question CALLS-MB? is written twice, once per file. It is also why the straight-line walk cannot serve a routine that both reaches a frame and reaches the caller's data stack: its frame rule wants the reserve to be the block's first operation and its data-stack rule wants the take to be, which is dot habu-let-a-data-edb3ba26. Landing calls to other words made this live - a calling routine of one block is what ': A ( n -- n ) B 1+ ;' is - and it was handled by sending such a routine to the general walk in both files. The real fix is one walk: give the general one the straight-line one's numbering as the single-block case of its own, delete MEASURE/DSTACK-CK/FRAME-CK and the second dispatch, and let block count stop being a question either file asks. Owners: A64RA, A64RAV.
