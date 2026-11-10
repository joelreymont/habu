---
title: Measure what a landed change costs and where it goes
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T17:47:00.775391+02:00"
---

Problem: nobody can say which change would make Habu ten times better for agents, because no measure says what one landed change costs or what the cost is spent on. Measured so far: refusals are 0.94% of agent calls (/Users/joel/Work/reviews/habu-10x-2026-11-10/measure-f4.md); reading and searching are 68% of calls and 38% of input tokens (/Users/joel/Work/reviews/habu-10x-2026-11-10/measure-r.md). Not measured: tokens and dollars per landed change by activity, what a read costs while it stays in context, time spent waiting on builds and full runs, agent runs and review rounds per landed lane, runs that stop short, and work that never lands. Source: Joel, 2026-11-10, asked 'so how exactly, in your opinion, should we make habu 10x better for agentic use?', answered that a measure comes first (/Users/joel/Work/reviews/habu-10x-2026-11-10/opinion-10x.md), then told the typed-state stage is built only if a measure says stack mismatches cost enough: 'measure then'.
Acceptance: two reports from the transcripts since 2026-11-01 for habu, maki, tender and zen, each with a hand-check: (1) calls, tokens and list-price dollars per landed code commit, split by activity and role, with the cost each tool result carries while it stays in context and the files that carry the most; (2) hours inside builds, test runs and waits, runs and review rounds per landed lane, stops by cause, the share of tokens on lanes that did not land, and the lead's share. From them, a ranked list for Joel of what each lever acts on (less reading, shorter loop, fewer rounds, discovery words, typed state), each with its measured share.
Files: none in the repository; briefs /Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/Measure-landed-change-cost.md and Measure-waiting-rounds-waste.md; scratch /Users/joel/.cache/tmp/bob-measure-c1 and bob-measure-c2; reports /Users/joel/Work/reviews/habu-10x-2026-11-10/measure-c1.md and measure-c2.md.
Verify: each report's hand-check and its counts given beside every share.
Depends: none. Ownership: the two reports. Claim: agent=bob scratch=/Users/joel/.cache/tmp/bob-measure-c1,/Users/joel/.cache/tmp/bob-measure-c2

2026-11-10: the full-run gap is partly counted (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/measure-c2-gap-stop1.md): 608 code commits went out in 225 pushes; 390 of them (64%) after a full run in the push's window, 156 after focused runs only, 62 after none. Its hand-check and the per-tree check are in work; the dot closes when they are in.
