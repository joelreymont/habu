---
title: Tell a ticked body keyword it is no word
status: active
priority: 2
issue-type: task
created-at: "2026-11-16T07:07:10.288336+01:00"
---

Problem: ticking a body keyword, top-level `' if` or `['] if` in a body, gives E-UNDEFINED "cannot find word `if`" with help "define `if` before this point, or check its spelling" (src/habu/interpreter.f TICKED through LOOKUP and UNDEFINED; check-walk.f TOP-ANSWER with runs false), from the load and check.f alike; `if` is a construct the body compiler reads, which no tick can take, so the help names a fix that does not work. A ticked top-level keyword (`' public drop`) gets the same wrong help. Acceptance: both ticks of a body keyword, `postpone` of one and a tick of a top-level keyword, from the load and check.f, give one record that says the keyword has no execution token and names what to tick instead; a test pins it. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-w195.
