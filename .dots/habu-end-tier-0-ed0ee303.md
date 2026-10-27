---
title: End tier 0 self-recursion in a named overflow
status: open
priority: 2
issue-type: task
created-at: "2026-10-27T19:57:12.423917+02:00"
---

test/engine-stack-machine.f REPRODUCERS "a word calling itself, tier 0" (SELF$ = `TRUSTED: R ( -- ) recurse ;` then `R`) times out at 120 s CPU-bound instead of ending in the named machine-stack overflow; the tier 1 and CELLS$ cases pass. Not load: it hangs alone, and `printf "TRUSTED: R ( -- ) recurse ;\nR\n" > self.f; timeout 10 bin/hb self.f` exits 124 on master-descended jerry-gap bin/hb (no batch 1) and on the batch 1 tip. Likely tier 0 now compiles the tail `recurse` as a jump, which makes the program a legal endless loop. Decide the layer: if tier 0 tail calls are intended, the reproducer needs a non-tail self call (as CELLS$ has); if not, fix tier 0. Owner: jerry (tier 0). Proof: engine-stack-machine row green in the full suite.
