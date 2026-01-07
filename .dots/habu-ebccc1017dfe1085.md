---
title: Wire array primitives to VM opcodes
status: active
priority: 2
issue-type: task
created-at: "\"2026-01-07T22:53:15.927060+02:00\""
---

Location: src/bytecode/opcodes.zig (add opcodes), src/interp/vm.zig (add handlers), src/compiler/compile.zig (add primitive compilation). Primitives ready in vector.zig: makeArray, makeArrayFill, arrayRef, arraySet, arrayRank, arrayDimensions, arrayTotalSize, arrayp. Need to add opcodes (0x60-0x67 range available) and wire to VM dispatch.
