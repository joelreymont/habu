---
title: Keep a validated NEWTYPE out of zero-filled storage
status: open
priority: 2
issue-type: task
created-at: "2026-11-01T02:24:36.111562+02:00"
---

Problem (Astra, 2026-11-01, on the public hb 5b9b6e87): TYPED-VARIABLE D NUM:positive-divisor read before any store answers a positive-divisor whose number is 0, though NUM's validator refuses zero (src/lib/num-types.f:104) and src/lib/num-arithmetic.f:186 says a zero divisor is statically impossible. Zero-filled typed storage makes a value of a type whose owner validates every value. Acceptance: first reproduce in a rejected-program or runtime row; then decide at the owner whether zero-filled storage of a validated family is refused at declaration, or the claim and its callers change. Files: src/lib/num-types.f, src/lib/num-arithmetic.f, the storage definers. Depends: none. Claim: unassigned.
