---
title: Design an exact one-token binary64 literal
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T12:47:18.982297+02:00"
---

Problem: Habu has no one-token literal for an exact binary64, so Maki's generated sources write each number as `$403E000000000000 IEEE754:BITS>F64`, two tokens and a call (tim, Maki lead, 2026-11-06; /Users/joel/Work/maki/.jj-ws.noindex/tim/.out/audit/rca-load/habu-request.md, /Users/joel/Work/maki/.jj-ws.noindex/tim/.out/audit/rca-load/report.md). In Maki's 17.6 MB design these are 279,092 literals, 41% of all tokens; replacing the 278,389 that are not parameters by one decimal token each cut the check by 26% and the load by 31% (CHECK 157.2e9 to 116.2e9 instructions, included 184.0e9 to 127.6e9), but the decimals were not exact: num-parse refuses the long decimals an exact round trip needs (FLOAT-LITERAL-ADMITTED?, src/core/checker.f :18107), and Maki's parameters must state exact bits. src/lib/f64-text.f already parses and formats exact shortest round-trip text. Acceptance: a design, Opus on Plan, of the one-token form (an existing literal made exact, or a new one), every public item and the path that reaches it; one question per new public item to Joel; then lanes. Measure Maki's design-8 check before and after. Depends: none. Ownership: carl (lead). Claim: unassigned.
