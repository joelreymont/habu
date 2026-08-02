---
title: Reject overflowing decimal literal scaling
status: open
priority: 1
issue-type: task
created-at: "2026-07-21T22:33:32.482667+02:00"
---

Problem: the shared native literal path accepts -0.0085031157383406233, but C-NUM-FRAC-STEP accumulates the fractional numerator and power-of-ten denominator in signed cells without overflow checks. At 19 fractional digits the denominator wraps, the token still classifies as a float, and execution produces a positive value near 0.010066737744 instead of the negative literal. This is a compiler/parser soundness bug, not an application-data error.

Required investigation: prove the exact interpreted and checked/compiled bin/hb command paths are fail-closed for a separate known-bad checked definition, then capture exit status, stdout/stderr, parsed bits, and source site for the 18-digit boundary case and the 19-digit reproducer. Classify whether checker syntax acceptance, runtime LNUM conversion, emitted literal bytes, and lib/float parsing disagree. The minimal fixture must show the wrong 19-digit value is currently accepted without diagnostics.

Acceptance: replace the unchecked signed-cell numerator/scale recurrence with
one shared decimal-to-IEEE-754 binary64 contract. Either implement exact
correctly-rounded scaling for every admitted spelling or reject before
evaluation when exact conversion support or an accumulator bound is exceeded;
silent wrapping, digit truncation, and lucky precision caps are forbidden. The
interpreter, checked compiler, AOT path, and reusable string parser produce
identical binary64 bits or the same named rejection for the same token.

Add positive and negative matrices around 18/19 digits, leading-dot and signed
forms, halfway rounding, subnormal/normal/overflow boundaries, long zero tails,
and values whose numerator or power of ten crosses a cell. The reference oracle
must be independent. If policy rejects the spelling, the bad checked program
fails before runtime. Update the owning diagnostics, `docs/forth.md` literal
contract, and native/recovery source mirrors. Any surviving source `TRUST`
keeps only its source-local rationale, retirement owner, and focused production
test.

Files owned: `src/habu/habu1.f` literal conversion,
`src/core/checker.f` literal admission, `lib/float.f` shared parser, the recovery
mirror, exact engine/checker/AOT fixtures, and the literal documentation. No
local data-rounding workaround closes this dot.
