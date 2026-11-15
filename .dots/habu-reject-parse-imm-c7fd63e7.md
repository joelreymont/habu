---
title: Reject parse-imm on a word not immediate
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T22:59:25.083273+01:00"
---

Problem: a parse-imm row may name a word that is not immediate; the load compiles its payload tokens as calls while the walk (check-walk.f CALL-STEP) and the scan (verify-source.f SKIP-PAYLOAD) skip them, so check and load disagree. Acceptance: a parse-imm declaration naming a word that is not immediate is rejected with a rendered rejection under every loader. Depends: habu-reject-an-undeclared-3533942e (the checker's own immediacy fact). Claim: agent=worker workspace=.jj-ws.noindex/carl-vfix.
