---
title: "Libraries: migrate regex hit"
status: closed
priority: 1
issue-type: task
created-at: "2026-07-24T13:02:19.344752+02:00"
closed-at: "2026-07-24T17:43:03.107353+02:00"
close-reason: Reviewed implementation landed and verified at master@origin 4fb6f52fb815.
---

Why: lib/regex.f still declares RX:hit with legacy PRODUCT although unified STRUCTURE already supplies the same generated record surface. Owner: lib/regex.f and its focused regex tests only. Replace PRODUCT hit 0 ... ;PRODUCT with STRUCTURE hit 0 ... ;STRUCTURE inside package RX, preserving fields off/len, their nominal schemas, declaration order, RX-HIT:MAKE/UNMAKE spelling, physical width, OPTION:SOME construction, RX-FIND-FROM and RX-COUNT behavior, and allocation behavior. Update comments. Forbidden: compatibility aliases, legacy parser edits, raw casts, regex algorithm changes, option changes, unrelated cleanup. Acceptance: real focused regex paths cover none, first match, nonzero start, zero-length match, count progression, MAKE, and UNMAKE before/after; exact effects and layout remain stable; token-aware executable census finds no legacy declaration in lib/regex.f; focused typed-local/package/trust gates pass. Dependency proof: master 227b5b349702 runs the unified STRUCTURE production suite green.

Claim: agent=claude-regex workspace=.jj-ws/habu-libs-migrate-regex-501110fc
