---
title: Preserve finite values when scaling decimal exponents
status: closed
priority: 1
issue-type: task
created-at: "\"2026-09-13T20:46:47.063402+03:00\""
closed-at: "2026-09-14T14:22:20.594695+03:00"
close-reason: Reviewed original 79f6766e independently; Hazel reviewed the negative-POW10+ followup. Integrated both into compiler line. Float tests pass through real source loads at tiers0/1, including signed zero, normal/subnormal values, compensating long exponents and invalid syntax. Public negative POW10+ behavior preserved; converter explicitly remains approximate rather than correctly rounded at arbitrary decimal boundaries.
---

Problem: current lib/float.f constructs an overflowing positive power before taking its reciprocal. Source and both-tier native probes confirm 0e400 yields SOME NaN, 1000e-309 yields SOME 0 rather than normal 1e-306, and 1e-309 loses a nonzero subnormal. Own significand/exponent conversion; combine without overflowing the intermediate scale or give a typed refusal where the documented range ends. Preserve zero/sign, normalization and invalid-exponent handling. Test external expected binary64 bits and compensating exponents through real source loads at both tiers. The native decimal overflow guard is already repaired and is not this bug. Source: current review D06; earlier bundle F03. Claim: unassigned.
