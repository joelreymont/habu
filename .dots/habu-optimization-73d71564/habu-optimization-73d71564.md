---
title: Optimization
status: open
priority: 1
issue-type: task
created-at: "2026-09-24T16:53:24.838504+02:00"
---

Problem: this parent collects the work to reduce hb's size, the instructions it generates, its startup storage and the cost of building it. Its goal is stated against an engine that no longer is the product: the user set "1 MB or more of additional reduction" from the qualified 2,625,655-byte paired engine, a target of 1,625,655 bytes or smaller, but that engine was built by capture (loading the product into the running hb and saving its memory), which is machinery being replaced (docs/design.md "Machinery being replaced"). hb is now linked and written as data by `tools/hb-build.f` (docs/compilation.md:88-97), so its size has to be measured afresh. The installed `bin/hb` in the main checkout is 6,814,744 bytes, not confirmed to be built from master. Whether the goal stays the absolute 1,625,655 bytes or becomes 1 MB below the linker-built hb's measured size is the user's choice.

Rules that hold for every child: never compress code or data without the user's explicit approval; dropping zero-filled, unused or unreachable storage, packing allocations tightly and compact table encodings are not compression (AGENTS.md "Changes"). Remeasure any earlier claim on the current product before working on it. Count the whole product cost, new optimizer code included. Never add candidate savings together: rebuild and measure the file, because the Mach-O text segment rounds to a 16 KiB page and a code saving can leave the file unchanged. The user's decisions: no build mode without checks; HR1, the private register ABI, stays parked. Confirmed drops: inline guarded scalar stores; exact-site division helpers and CallOrigin rows; ADRP addressing for DATA; the fragment model, typed relocation algebra and wire schema; the SCC fixed point and capsules. Reverted after landing: census e13ae0a3, terminal-only link save f2071a86, max as select fec184ee. Dropped before landing: f082dbf3, c57b5f1b, db11d4c1, 890d67ea, d65bdc94, 2ddb20af, 2ca2438f and the span-check slice of 22af10b0; (STORE-CELLS) 458c9100 closed below its break-even. Rejected compression experiments, never to land: DATA cbfdab2dff57dafa14ceef8a75639253d19f0d7c, CODE 89686491ae43fb71101918b2340251abba59768f.

Acceptance: the user's decision on the goal is recorded here, with hb's file size measured on master's linker-built engine as its baseline; the parent closes when a qualified hb meets that goal and its open children are closed or moved.

Files: .dots/habu-optimization-73d71564/habu-optimization-73d71564.md.

Verify: `bin/hb --load tools/hb-build.f -- --engine -o $OUT/hb` on master, then `bin/hb --load tools/engine-size.f -- $OUT/hb`; the dot states the size and the user's goal.

Depends: open children habu-declare-the-surface-89e9aed0, habu-delete-the-dead-11e9a4f3, habu-dispatch-do-tok1-e63cab6f, habu-ship-only-the-d7d38629, habu-a-store-s-d7f6c4cb.

Ownership: unassigned. Claim: unassigned.
