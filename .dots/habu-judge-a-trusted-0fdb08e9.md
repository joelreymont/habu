---
title: "Judge a TRUSTED: body's requires in the check"
status: active
priority: 2
issue-type: task
created-at: "2026-11-25T16:54:10.255690+01:00"
---

Problem: tools/check.f reads a TRUSTED: body (src/habu/verify-source.f SCAN-TRUSTED-BODY) without judging its calls, so a file whose TRUSTED: word calls an unrequired word passes the check (rc 0) and is E-UNREQUIRED on the load. Acceptance: the check gives the load's verdict at the same position, for a plain TRUSTED: and a definer's does> clause, with a fixture in test/require-check.f; no new false rejection in src, tools or test. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-rq-trust.
