---
title: Unify JSON number grammar between validator and decoder
status: open
priority: 2
issue-type: task
created-at: "2026-07-22T14:32:22.658404+02:00"
---

Problem: validator/decoder split — JSON-NUMBER? certifies '1e+00000000000000000005' as T-FLOAT but JR:FLOAT -> STR>FLOAT -> STR>NUMBER? rejects it (E-JR-NUMBER) because the i64 bound at lib/string.f:221,229 is a digit-COUNT check that counts leading zeros (lib/float.f:97-102 exponent path). A token the validator accepts must decode; same-grammar-two-implementations is the checker-miss class this repo treats as structural. Confirmed still open at codex pair 03fe0bdc+ca1d4c65 (review 2026-07-22). Expected: one owned numeric-token grammar — STR>NUMBER?'s bound becomes value-based (accumulate with overflow check) or normalizes leading zeros before the digit bound; FL-EXP clamp (FL-EXP-MAX=400, lib/float.f:20) then handles huge exponents. Acceptance: red-first fixture '1e+00000000000000000005' JR:NEXT->T-FLOAT then JR:FLOAT -> 1e5 (no throw); '1e999' -> +inf clamp path; existing float/string/json suites green. Files: lib/string.f, lib/float.f, lib/float-test.f, lib/string-test.f, lib/json-read-test.f. Verify: bin/hb --load lib/float-test.f lib/string-test.f lib/json-read-test.f. Depends: none (sequence after codex JSON pair merges). Ownership: STR>NUMBER? numeric bound. Claim: unassigned.
