---
title: Prune the signature pool to the nameable surface
status: open
priority: 2
issue-type: task
created-at: "2026-08-18T19:38:29.083141+02:00"
---

User challenge 2026-08-18: the 245KB signature text serves checked code naming BAKED words - but only PUBLIC words are nameable. Measured split: 1606 public / 5192 private / 0 global. Private words live in sealed packages; no checked program can open or compile into one (the seal, checked-habu threat model), so their rows serve nobody BY CONSTRUCTION - the producibility argument. Prune the capture to public rows only: expected 245KB -> ~60KB. CASCADE, handle honestly: (a) the capture-side audit currently requires EVERY checked window record to carry a row - its clause set gains 'private = exempt' with the seal as the argument, and the partition assertion (6798=94+checked) re-derives; (b) verify the intake never needs a private row (the chain's own internal calls resolve by the SEED's relocation, not the checker pool - confirm by measurement); (c) SIGSCOPE's bare-family fixture uses an EXPORTed public word - unaffected; (d) a mutation: a private row smuggled in reds by name (the pool carries only what is nameable). Rides the buffers-at-startup landing or follows it - same artifact surgery region.
