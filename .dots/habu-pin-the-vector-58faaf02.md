---
title: Pin the vector-strip edge goldens with clang twins
status: open
priority: 2
issue-type: task
created-at: "2026-08-06T20:16:00.001405+02:00"
blocks:
  - habu-vectorize-byte-sum-f5ac861d
---

The goldens the vectorize leaf demands, which the pinned corpus cannot supply on its own. tools/codegen-compare-cases.f:35-42 pins ONE 21-byte subject ('habu codegen baseline') and an empty span, so the existing rows exercise exactly one strip plus a 5-byte tail and NOTHING else - not a short span below a strip, not an exact multiple, not an unaligned base, not a long span with many strips.

Add corpus rows with clang twins in tools/clang/twins.c (built -O2 -arch arm64 -fno-math-errno by tools/codegen-compare-cc.f) covering every edge the strip loop has: empty; short (fewer than 16 bytes, so the vector body must not run at all); exactly one vector (16); one vector plus a partial tail; unaligned base; and long (many strips). Answers pinned in the baseline tables' outputs column exactly as the existing rows are (test/compiler/codegen-compare-baseline.txt and codegen-chain-baseline.txt).

DIFFERENTIAL against the scalar chain on adversarial spans, bit for bit: all-zero, all-0xFF (the case that would overflow a 16-bit lane accumulator if the reduction were ever widened across strips - 16*255=4080 fits, 17 strips would not), and spans whose length sits one either side of every strip boundary.

MUTATIONS the goldens must catch, and if one is caught by NEITHER the goldens NOR the validator that is a reported gap, not a pass: wrong lane count in the tail; a vector op with the wrong element size; the scalar fallback removed (the short goldens must go red).

A NEW TEST FILE MUST BE REGISTERED IN A SUITE - hand-run green proves nothing about the gate.

Found by agent neon while scoping habu-vectorize-the-byte-a0da35a7.
