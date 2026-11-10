---
title: Checker refusals that name a token and no reason
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T10:28:37.014478+02:00"
---

Problem: three refusals print only the token: `: T ( n -- n ) 0 ?do 1 loop ;` prints `habu: in t: at 'loop'`; `: T ( -- ) 3 0 do i drop loop i drop ;` prints `habu: in t: at 'i'`; `: T ( n -- n ) {: a b | c -- d :} a ;` prints `habu: in t: at '--'`. Each is followed only by `hook: non-certified definition` and `ncomp: cannot compile`. Cases ~/Work/reviews/habu-10x-2026-11-10/refusals/p19.f, p20.f, p14.f with their outputs, run as `hb --load <case>` with the installed hb. Measured 2026-11-10 over the Claude Code transcripts of habu, maki, tender and zen since 2026-11-01 (453,140 tool calls): 229 refusal episodes print no reason at all, 887 calls; report ~/Work/reviews/habu-10x-2026-11-10/measure-f4.md. Other refusals in the same report state the rows or the rule (p1.f, p2.f), so a reason line exists and these paths do not reach it. Acceptance: first a reported cause, with no code change: for each of the three, the word in the checker that refuses and why no reason reaches the output, and whether the bare `ncomp: cannot compile X` (132 episodes) is the same cause. Then each refusal states what was expected and what was found, as the others do; the wording is a change to diagnostic output and comes to Joel as a fact before a worker writes it. Files: named by the cause. Verify: the three cases print a reason; the checker's tests that pin refusal text. Depends: none. Ownership: the checker's refusal output. Claim: agent=bob workspace=.jj-ws.noindex/habu-checker-refusals-that-3152182a
