---
title: Divide rounding toward either infinity
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T05:57:02.091127+01:00"
---

Problem: Habu's `/` truncates toward zero and its only rounded quotient, NUM's private CEIL-QUOT (src/lib/num-arithmetic.f:53), takes v >= 0 and u > 0, so a caller that needs the ceiling or floor of a signed quotient writes its own from `/` and `mod`: Maki's drawing.f Y-AT (the y at x of a line, rounded toward +y so a label sits on or below ink) does so today. Generic integer mathematics belongs in Habu. Acceptance: public words answer the floor and the ceiling of n / d for every sign of n and every nonzero d, exactly (no overflow beyond n / d's own), each with its refusal for d = 0 as `/`'s; CEIL-QUOT's callers may move to them. Depends: none. Claim: unassigned.
