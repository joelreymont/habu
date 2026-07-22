---
title: "FMATH: clamp FEXP scaling; test FROUND"
status: open
priority: 2
issue-type: task
created-at: "2026-07-22T14:05:23.127093+02:00"
---

Problem: lib/fmath.f:15-17,36-39 FEXP computes k=FROUND(x/ln2) then F2^N loops |k| times — unbounded work for large |x| (demonstrated: 1e15 -> hang killed at 5s; 1e9 already ~1.4e9 iterations) though any |x|>~710 is +inf/0 in double; FROUND does unguarded f>s (engine-defined on overflow/NaN) and public FROUND has zero tests. Expected fix: clamp k structurally at the double exponent range (mirror FL-EXP-MAX precedent, lib/float.f:20) returning +inf/0 for out-of-range x before the loop; guard FROUND's f>s domain (NaN/overflow -> named throw or defined result, document which). Acceptance: T{ }T: FEXP 1e15 -> +inf immediately; FEXP -1e15 -> 0e; FEXP 700e vs known value; FROUND halfway cases (+-0.5), large-|x|, and domain edges pinned. Files: lib/fmath.f, lib/fmath-test.f. Verify: bin/hb --load lib/fmath-test.f; dependent AD/eval suites in maki stay green. Depends: none. Ownership: lib/fmath.f. Claim: unassigned.
