---
title: Implement missing array functions
status: active
priority: 2
issue-type: task
created-at: "\"2026-01-17T21:36:19.820619+02:00\""
---

stdlib.habu/compile.zig: Add ~28 missing array functions (adjust-array, array-dimension, array-dimensions, array-displacement, array-element-type, array-in-bounds-p, array-rank, array-row-major-index, array-total-size, bit, bit-and, bit-andc1, bit-andc2, bit-eqv, bit-ior, bit-nand, bit-nor, bit-not, bit-orc1, bit-orc2, bit-xor, fill-pointer, row-major-aref, sbit, upgraded-array-element-type, vector-pop, vector-push, vector-push-extend). Bit vector ops need runtime.
