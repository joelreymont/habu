---
title: "Move Habu's own hand-widened JSON spans to READ-ANY-SPAN$"
status: open
priority: 3
issue-type: task
created-at: "2026-11-11T00:00:35.651292+02:00"
---

Problem: JSON:READ-ANY-SPAN$ (src/lib/json/read.f) answers any value's source text, a string with its quotes, yet five places in Habu still widen a string's span by hand: src/lib/net/mcp.f:149-156 RAW$ (callers FIELD? :178, WRITE-MEMBER :298, PART :811, MEMBER :1517), tools/gate-json-assert-core.f:157-164 GJA-VALUE (callers :179, :193), src/lib/net/test-issuer.f:251 MEMBER-RAW, src/lib/net/model.f:3133 JOURNAL-FIELD, and src/lib/json/equal.f:424-425 ADD-ROW, which widens the token's offset and length inside the package. A change to how a string token is published has to be made in each. Acceptance: the four callers outside package JSON take their span from JSON:READ-ANY-SPAN$ and RAW$ and GJA-VALUE are deleted; equal.f ADD-ROW and read.f ANY-SPAN-INNER share one private word for a string token's widened place and length; no public word is added; mcp.f:300, which widens a key's span, is not a copy and stays. Files: src/lib/net/mcp.f, tools/gate-json-assert-core.f, src/lib/net/test-issuer.f, src/lib/net/model.f, src/lib/json/equal.f, src/lib/json/read.f. Verify: the mcp, gate-json-assert, test-issuer, model and json-equal test files, each once. Depends: none. Ownership: unassigned. Claim: unassigned. Consumers: none reported it; found by the review of habu-a-json-reader-cbca2d83.
