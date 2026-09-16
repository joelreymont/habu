---
title: Document that a throwing lifecycle hook stays registered
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T11:24:03.740682+03:00"
---

Problem: IMAGE-LIFECYCLE:PREPARE keeps a hook registered when that hook throws, so a resource owner whose release refuses must not clear its own registered flag before rethrowing, or the next PREPARE registers a second copy (Maki session table did exactly that; fixed on Maki). docs/forth.md does not state the rule. Acceptance: docs/forth.md image-lifecycle section states that a throwing hook stays registered and what an owner release must therefore do, citing lib/net/udp4.f REGISTER-CLEANUP as the shape. Files: docs/forth.md. Verify: read. Depends: none. Ownership: hazel line. Claim: unassigned.
