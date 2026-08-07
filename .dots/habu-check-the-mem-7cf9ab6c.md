---
title: Check the memory twins still do their memory traffic
status: open
priority: 3
issue-type: task
created-at: "2026-08-07T14:00:24.711611+02:00"
---

tools/clang/twins.c marks CELL-BUMP's cell and STORE-LOAD's cell volatile so clang keeps the accesses those two rows are NAMED for (habu-make-the-byte-1de071ba: before it, hc1_cell_bump was one str and hc4_store_load was one ldr, one str and x0+3*len - no loop, no loop-carried dependence). Nothing checks it. The clang column is deliberately not pinned (it is a fact about the host toolchain), the recorded outputs are identical with and without volatile, so a qualifier deleted by accident would silently put both rows back to measuring a different program, and the only evidence today is prose plus a disassembly recorded in the file's head. A check is reachable with what the harness already has: CODEGEN-CABI:FN gives the twin's mapped address and CODEGEN-MACHO:BYTES its size, so the twin's instructions can be counted the way tools/codegen-compare-test.f already counts the habu columns' loads and stores (DS-COUNT, LDR-OP/STR-OP masks) - assert hc4_store_load holds a load and a store inside a backward branch, and hc1_cell_bump two stores and a load.
