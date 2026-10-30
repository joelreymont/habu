---
title: Refuse unreachable code in T0
status: closed
priority: 2
issue-type: task
created-at: "2026-10-31T10:31:34.499972+02:00"
closed-at: "2026-10-31T12:11:58.020183+02:00"
close-reason: "Landed with the T0 release (ttloqwsx b17a1ee3): full suite 510/510, two generations converge, edit matrix F53/F105/F156 only, as master"
---

Problem: after rtvsttwt, T0 refused dead immediates in TRUSTED: bodies (1 exit s" ab", .", [char], if) with a false E-UNDEFINED. Compiling dead code (lane t0fd, xkpxtruz) was rejected in review: wrong code (/tmp/jerry-rv2-tmp/p/w1.f printed 1 2 4 where the JIT refuses) and acceptance of malformed dead code. Ruling: T0 refuses at the first dead token, rc 70, no E-UNDEFINED, as checked bodies and tier 1 (E-NELAB-CTRL) do. Acceptance: /tmp/jerry-big/t0fe-brief.md; probes /tmp/jerry-big/rv/dx1-6.f rc 70 named with no E-UNDEFINED. Files: src/core/checker.f, src/habu/t0.f, src/lib/errors.f, test/load-reject-diag-test.f. Verify: brief's Proof. Depends: none. Ownership: lane t0fe. Claim: agent=jerry worker workspace=.jj-ws.noindex/jerry-t0fc
