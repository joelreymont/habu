---
title: Keep a validated NEWTYPE out of zero-filled storage
status: open
priority: 2
issue-type: task
created-at: "2026-11-01T02:24:36.111562+02:00"
---

Problem: TYPED-VARIABLE D NUM:alloc-byte-len read before any store answers an alloc-byte-len whose number is 0, though NUM's only maker refuses zero (AS-ALLOC-BYTE-LEN, src/lib/num-types.f:56) and MEM's allocation sinks take alloc-byte-len as a nonzero extent. Zero-filled typed storage makes a value of a type whose owner validates every value. Acceptance: first reproduce in a rejected-program or runtime row; then decide at the owner whether zero-filled storage of a validated family is refused at declaration, or the claim and its callers change. Files: src/lib/num-types.f, src/lib/num-arithmetic.f, the storage definers. Depends: none. Claim: unassigned.
