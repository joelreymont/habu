---
title: Make the product independent of its host engine
status: closed
priority: 2
issue-type: task
created-at: "2026-09-30T10:12:23.389048+03:00"
closed-at: "2026-10-01T11:46:15.995417+03:00"
close-reason: "The prefix seal kept host record numbers in DATA (73 REG-PROTECT cells, 3 IMK cursors); with them retired, master built under 5c32 and under a one-primitive-larger host is 7f48da9f both ways, and a primitive-adding tree gives gen1 == gen2 (c8f7e24a)."
---

Problem: an ARM64 product's bytes depend on the host engine's dictionary, not only on the source. Found by I4a (2026-09-30): building I4a's tree with a base host gives gen1 `b9a66fb0…`, and gen2 `347cf5b8…` differs from it in 77 bytes. 74 of them sit near file offset 1,776,060 in the captured DATA window values (`AOT-WINDOW:EMIT-VALS`, one LEB128 per cell): about 75 cells, each exactly +1 in gen2. Three more sit near 1,974,533 (212 becomes 213). Building master's unmodified tree with I4a's gen2 as host gives `e8672e2b…`, not master's `506ca7b1…`, again with +1 cells. I4a adds one primitive (`execute-floor`). An independently recovered host reaches `347cf5b8…` too, so the fixpoint is sound, but any change that adds a primitive shows gen1 != gen2, and a product is not a function of its source alone.
Acceptance: identify which captured cells carry a host-dependent count (likely the host's primitive or dictionary count leaking into DATA through the capture) and make the capture record the target's own value, so that building a tree with any correct host of the same source lineage gives the same product (master's tree built by I4a's gen2 gives `506ca7b1…`). Pre-change failing check: that cross-host build, which today gives `e8672e2b…`.
Files: to be determined by the diagnosis (`src/habu/aot-capture.f`, `aot-window`, `tools/native-build-core.f` are the first suspects).
Verify: spark: the cross-host build byte-identical; the gate.
Route: lands on master after the Linux gate; Alder pools the Mac gate.
Ownership: krait (Linux lane). G3 (habu-self-host-the-ccc31e78) does not wait on it: it blocks on G2 only.
Claim: unassigned.
