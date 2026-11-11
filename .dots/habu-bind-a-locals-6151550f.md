---
title: Bind a locals group inside a quotation
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T07:52:05.977092+02:00"
blocks:
  - habu-give-a-scope-24514a67
---

Problem: a quotation cannot bind a {: … :} group of its own (src/core/checker.f LOC-BEGIN refuses it, E-BAD-LOCAL-SHAPE), so a quotation body that needs a named value becomes a named word (docs/forth.md 'Words & factoring'). Left out of the quotations-read-locals change (dot habu-give-a-scope-24514a67, DECISIONS.md 'quotations read locals') as a separate limit: no site in Habu, Tender, Zen or Maki is known to wait on it. Acceptance: [: {: x:n :} x x * ;] loads and runs on every T0 target; E-BAD-LOCAL-SHAPE keeps only the dead-path case. Design: the quotation's T0 unit takes frame slots as a word does; do it after the read form has landed on all three targets. Files: src/core/checker.f, src/habu/t0.f, the emitters. Verify: a test through the real load path. Depends: habu-give-a-scope-24514a67. Ownership: unassigned. Claim: unassigned. Consumers: none meets this today.
