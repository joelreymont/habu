---
title: Check no C twin tail-branches to another twin
status: open
priority: 3
issue-type: task
created-at: "2026-08-07T13:57:32.289837+02:00"
---

The codegen comparison's reference column takes each twin's size to be the whole of its program. tools/codegen-compare-macho.f now refuses a non-external __text symbol, so a static helper clang declined to inline cannot hide - that closes the case for twins.c's own helpers. What it does NOT close: a twin that tail-branches to ANOTHER TWIN, which is external and reads normally. hc5_tail_chain calls hc5_tail_mid and clang inlines it today (verified: 40 bytes, no branch), but nothing checks that, and if a future clang tail-branched instead, that row's clang column would understate exactly the way the chain column did before habu-make-the-byte-1de071ba. The check is available with what the harness already has: CODEGEN-CABI:FN gives a twin's mapped address, CODEGEN-MACHO:BYTES its size, so the last instruction is at FN+BYTES-4 and NBR:B? decodes it - refuse, or mark the row, when its target is outside [FN, FN+BYTES). Same shape as NTAILPROBE:TAIL-BRANCH? on the habu side.
