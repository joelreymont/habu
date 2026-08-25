---
title: Define TIME-MONO-NS or repair bpe-full-test reference
status: open
priority: 2
issue-type: task
created-at: "2026-07-22T14:26:18.663403+02:00"
---

Problem: maki/examples/nanogpt/bpe-full-test.f calls TIME-MONO-NS which is defined nowhere in the tree (rg finds only the call site), so the test fails standalone with E-UNDEFINED. Pre-existing; not in maki/test.f; presence-gated on uncommitted vocab.bpe. Expected: either implement TIME-MONO-NS as a typed monotonic-clock word in the owning time package (lib/time.f) with a T{ }T test, or repair the test to use the existing timing surface. Acceptance: bpe-full-test.f loads and runs standalone (given vocab.bpe) with no undefined words; new word, if added, has typed effect + test. Files: lib/time.f or maki/examples/nanogpt/bpe-full-test.f. Verify: bin/hb --load maki/examples/nanogpt/bpe-full-test.f. Depends: none. Ownership: bpe-full-test timing reference. Claim: unassigned.
