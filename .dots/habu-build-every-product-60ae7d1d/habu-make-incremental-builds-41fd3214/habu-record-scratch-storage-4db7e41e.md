---
title: Record scratch storage as byte ranges
status: open
priority: 3
issue-type: task
created-at: "2026-11-10T14:03:49.401462+02:00"
blocks:
  - habu-move-every-scratch-4ca80ba0
  - habu-emit-wasm-from-0d5dab47
---

Problem: the engine records scratch as one address-cell row per 8 bytes (src/habu/layout.f:1195-1203, XTCELL-SCRATCH-TAG, bit 62), so a cell with a pointer kind cannot also be scratch (one cell declared with two kinds is refused) and every reader walks a row per cell; the file-end reset costs 4% of a cold engine build. Acceptance (design .jj-ws.noindex/jerry-wasm/.out/scratch-declared-design.md, sections 2 to 4, approved by Joel 2026-11-10): one row per SCRATCH-ALLOT call, offset and byte length; whatever lies inside a range is not carried; the object lists ranges and cell rows lose kind 2 (hard cutover); the object writer, the store check, the link, the file-end reset and the program writer read ranges; scratch-cell-mark, XTCELL-SCRATCH-TAG and the kernel bodies are deleted. Waits for capture's last caller to move to the linker, so no per-cell path is kept beside the ranges; open questions the design lists under 'Not determined' are settled when it is briefed. Depends: habu-move-every-scratch-4ca80ba0; habu-emit-wasm-from-0d5dab47. Claim: unassigned.

Cases waiting for typed scratch storage (no scratch word declares a typed cell, so the owner lanes left them carried): src/lib/genio-core.f SLOT-GEN, SLOT-STATE, SLOT-FAULT and the OP-* rows (read only behind the scratch row status); src/lib/source-lex.f's table set (TABLES, SRC-A, the vector headers; a mapped table's ADDR-V pointer is refused at the file's end); src/compiler/ir/context.f SESSION-CTX; src/lib/task.f:640 TASK-EXIT-SCRATCH (TYPED-VARIABLE of a quotation) keeps the last quotation a registration compared, so a file's top-level AT-EXIT leaves it carried. Private typed flags and counts that became plain scratch cells and want their types back: src/lib/signal.f READY, src/lib/pg.f CONFIGURED, CONN-CAPACITY and RESULT-CAPACITY, src/lib/process-argv.f and process-env.f CONTROL.
