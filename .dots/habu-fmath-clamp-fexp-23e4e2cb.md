---
title: "FMATH: clamp FEXP scaling; test FROUND"
status: active
priority: 2
issue-type: task
created-at: "2026-07-22T14:05:23.127093+02:00"
---

Claim: alder, .jj-ws/alder-fexp-range, base b4efad25. FROUND already has
domain checks and halfway/large-integer regressions; finish the FEXP range
and scaling cases without changing its degree-six approximation.

Implementation: finite FEXP input is clamped before FROUND to the binary64
exponential range. Overflow returns +infinity, underflow +zero, and nonfinite
inputs retain E-DOMAIN. FEXP and the existing FPOW series use LDEXP-STEPS
instead of repeated doubling/halving: the bounded reduced exponent needs
at most two multiplications and subnormal output rounds once. FROUND is
unchanged, with its existing halfway, adjacent, large and domain-edge tests.

Proof: the base engine's FEXP(1e15) hit a five-second timeout; the changed
load exits immediately. Tests pin both signs of 1e15 and max-finite input,
exp(700), finite exp(709.5), least-subnormal exp(-745), zero at -746, and
nonfinite refusals. The full float-parse registry row, native fmath suite,
and current Maki RF caller suite pass on the private host/tree. The older
Maki AD/eval paths named above are no longer in that repository. Astra review
is clear; Hazel supplies the final full gate and integration.

Problem: lib/fmath.f:15-17,36-39 FEXP computes k=FROUND(x/ln2) then F2^N loops |k| times — unbounded work for large |x| (demonstrated: 1e15 -> hang killed at 5s; 1e9 already ~1.4e9 iterations) though any |x|>~710 is +inf/0 in double; FROUND does unguarded f>s (engine-defined on overflow/NaN) and public FROUND has zero tests. Expected fix: clamp k structurally at the double exponent range (mirror FL-EXP-MAX precedent, lib/float.f:20) returning +inf/0 for out-of-range x before the loop; guard FROUND's f>s domain (NaN/overflow -> named throw or defined result, document which). Acceptance: T{ }T: FEXP 1e15 -> +inf immediately; FEXP -1e15 -> 0e; FEXP 700e vs known value; FROUND halfway cases (+-0.5), large-|x|, and domain edges pinned. Files: lib/fmath.f, lib/fmath-test.f. Verify: bin/hb --load lib/fmath-test.f; dependent AD/eval suites in maki stay green. Depends: none. Ownership: lib/fmath.f. Claim: unassigned.
