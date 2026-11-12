---
title: "JSON:COPY-VALUE recurses per nesting level"
status: open
priority: 2
issue-type: task
created-at: "2026-11-07T23:35:03.567507+02:00"
---

Problem: JSON:COPY-VALUE (src/lib/json/copy.f:11) calls RECURSE per container level (copy.f:33, :44), and the JSON reader has no depth limit, so a peer-supplied text nested deeply enough exhausts the return stack; docs/forth.md :966 allows RECURSE over a cursor only when depth is bounded. Found by tim (Maki) while reviewing JSON:VALUE=, which walks iteratively with sized frames (src/lib/json/equal.f). Acceptance: COPY-VALUE walks iteratively (an explicit frame stack, as equal.f does) and copies a 100000-level nested array and object as json-read-test's VALUE= deep test does. Files: src/lib/json/copy.f, src/lib/json-read-test.f. Verify: hb --load src/lib/json-read-test.f. Depends: none. Ownership: copy.f. Claim: unassigned.
