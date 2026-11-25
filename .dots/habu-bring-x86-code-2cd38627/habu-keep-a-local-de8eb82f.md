---
title: "Keep a local's register through a fetch"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-25T17:47:58.514999+01:00"
closed-at: "2026-11-25T21:49:05.325422+01:00"
---

Problem: src/habu/t0.f drops a register's known local in four places that need not: FETCH overwrites in place the vreg that holds the local's cell (`p @ p 8 + @` reloads p); STEER picks LOWEST, so a fill can overwrite a holder; each join drops every fact, also an else, where the registers are those at the if; a reading quotation's ENV-LOAD cells have no fact. Acceptance: tenderd's code shrinks on both hosts with no miscompile in the suite. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-facts.
