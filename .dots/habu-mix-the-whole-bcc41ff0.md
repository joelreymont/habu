---
title: Mix the whole key in the hashmap probe
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-12T18:32:38.271212+03:00\""
---

Problem: lib/hashmap.f HM:HASH64 is 'x x 33 rshift xor', the identity for keys below 2^33, and HM:PROBE masks that value, so a key whose information sits in the low bits (an FNV fold of a one-to-three byte string xor'ed with a row identity, Maki's pad table) collapses into a handful of slots and the table degenerates into a linear scan with every test still green, because a slot table confirms its candidates and a bad key is only slow; Maki measured 104 us per probe on 10800 pads (rowan, #general 2026-09-12 15:31 UTC). Acceptance: HM:HASH64 is a real 64-bit finalizer (murmur3 fmix64 or splitmix64's finalizer: shift-xor, multiply by an odd constant, shift-xor, multiply, shift-xor) so any key shape spreads; the probe's cost is bounded for adversarial key shapes: a regression inserts 4096 keys whose entropy is only in the low 12 bits, another 4096 whose entropy is only above bit 33, and asserts the mean probe length stays under a stated bound for both (measured by counting probes, not by time); existing hashmap suites green; docs/forth.md's hashmap section states that keys need no pre-mixing; callers in the tree that pre-mix their keys because of the old trap (rg for HASH64 and xor-folded keys in src/, lib/, tools/) are simplified where the fold only existed to escape it. Files: lib/hashmap.f, test/hashmap*.f, docs/forth.md. Verify: the hashmap suites, test/run.f. Depends: none. Ownership: hazel. Claim: agent=hazel-worker workspace=.jj-ws/habu-mix-the-whole-bcc41ff0.
