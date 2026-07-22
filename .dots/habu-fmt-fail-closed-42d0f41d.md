---
title: "FMT: fail closed on unsigned/fixed-point domain violations"
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-22T14:05:23.117233+02:00\""
---

Problem: lib/fmt.f:31-34 FMT:SB-U on negative input silently renders garbage ('-1' -> '/') or dies with uncaught E-STR-BOUNDS from SB-APPEND-C (-123 -> exit 67, wrong error class); FMT:SB-INT on $8000000000000000 negate-overflow hits same paths; lib/fmt.f:57-62 FMT:SB-FIX silently prints saturated wrong number when |x|*10^k overflows i64 (demonstrated: 1e18 with k=2 -> '92233720368547758.07', no throw). Expected fix: named-throw domain guards (E-FMT-DOMAIN or per-word codes): SB-U rejects negative, SB-INT handles INT_MIN correctly (special-case or widen), SB-FIX throws at the documented fits-i64 boundary instead of saturating. Acceptance: T{ }T negatives for each (negative SB-U -> throw; INT_MIN SB-INT -> correct digits; overflow SB-FIX -> throw; boundary value just below -> correct); public printers .U/.INT/F.N gain first tests. Files: lib/fmt.f, lib/fmt-test.f, lib/errors.f. Verify: bin/hb --load lib/fmt-test.f; grep gate reports for fmt consumers stay green (maki/test.f). Depends: none. Ownership: lib/fmt.f. Claim: agent=claude workspace=.jj-ws/habu-fmt-fail-closed-42d0f41d.
