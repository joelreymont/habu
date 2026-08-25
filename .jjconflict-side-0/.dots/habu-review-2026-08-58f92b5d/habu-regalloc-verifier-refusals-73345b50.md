---
title: regalloc verifier refusals have no mutation that flips them
status: open
priority: 1
issue-type: task
created-at: "2026-08-22T22:38:25.895140+02:00"
---

Problem: E-A64RAV-OVERLAP and E-A64RAV-CLOBBER occur once each in the tree - their throw sites (src/compiler/native/regalloc-verify.f:451-459, 431-439); same for E-A64RAV-INTERVAL (252-256) and E-A64RAV-SLOT (548-551); only E-A64RAV-EDGE is pinned (test/compiler/native-regalloc.f:2596). The verifier is the chain's stated independent authority (regalloc-verify.f:1825) and docs/proofs.md requires every clause falsified by mutation. Fifteen more refusal arms are thrown at one site and referenced nowhere: E-A64COMB-ADDEND, E-A64COMB-SOURCE, E-A64EMIT-ATTR, E-A64IR-FUN, E-A64IR-MASK, E-A64RA-FILE, E-A64SPILL-SOURCE, E-HIR-ADDR, E-HIR-CONTROL, E-NDICT-VALUE, E-NFEED-ORDER, E-NLOOP-SOURCE, E-NMIGRATE-ARITY, E-NPUB-HELD, E-NPUB-RELOC, E-NSTR-BODY. Acceptance: a claim seam that lets a test hand A64RAV:ACCEPT a module with a forged claim; one hostile fixture per verifier code and one negative case per remaining code, each red on a mutant that disables its arm. Files: src/compiler/native/regalloc-verify.f, test/compiler/native-regalloc.f and siblings. Verify: the suites plus a mutation run recorded in the commit. Depends: none. Ownership: native chain tests. Claim: unassigned.
