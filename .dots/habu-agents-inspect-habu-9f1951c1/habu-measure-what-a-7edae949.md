---
title: Measure what a landed change costs and where it goes
status: closed
priority: 2
issue-type: task
created-at: "2026-11-10T17:47:00.775391+02:00"
closed-at: "2026-11-10T20:13:39.692743+02:00"
close-reason: "both reports and the ranked list delivered; full-run gap closed after seven rounds: 275 full runs for 608 code commits, lane table green 412/85, red 48/18, unknown 1/1; not covered: the exact tree of each run and the 4 h read bound of three status rules, which moves no push class"
---

Problem: nobody can say which change would make Habu ten times better for agents, because no measure says what one landed change costs or what the cost is spent on. Measured so far: refusals are 0.94% of agent calls; reading and searching are 68% of calls and 38% of input tokens. Not measured: tokens and dollars per landed change by activity, what a read costs while it stays in context, time spent waiting on builds and full runs, agent runs and review rounds per landed lane, runs that stop short, and work that never lands. Source: Joel, 2026-11-10, asked 'so how exactly, in your opinion, should we make habu 10x better for agentic use?', answered that a measure comes first, then told the typed-state stage is built only if a measure says stack mismatches cost enough: 'measure then'.
Acceptance: two reports from the transcripts since 2026-11-01 for habu, maki, tender and zen, each with a hand-check: (1) calls, tokens and list-price dollars per landed code commit, split by activity and role, with the cost each tool result carries while it stays in context and the files that carry the most; (2) hours inside builds, test runs and waits, runs and review rounds per landed lane, stops by cause, the share of tokens on lanes that did not land, and the lead's share. From them, a ranked list for Joel of what each lever acts on (less reading, shorter loop, fewer rounds, discovery words, typed state), each with its measured share.
Files: none in the repository.
Verify: each report's hand-check and its counts given beside every share.
Depends: none. Ownership: the two reports. Claim: agent=bob

2026-11-10: the full-run gap is partly counted: 608 code commits went out in 225 pushes; 390 of them (64%) after a full run in the push's window, 156 after focused runs only, 62 after none. Its hand-check and the per-tree check are in work; the dot closes when they are in.

Closed 2026-11-10. Both reports stand and the ranked list went to Joel. The full-run gap took seven rounds: 275 full runs for 608 code commits (222 counted, 53 missed by the first rule), 0.45 per code commit, 1.22 per push that carried code, 0.49 per push. By the last full run of the push's own lane before it, code commits/pushes: green 412/85, red 48/18, unknown 1/1, another tree only 26/19, focused tests only 113/94, none 8/8 (608/225). Hand-checked over seven samples; every error found was fixed by a rule and each class it moved was judged against its transcript. Not covered: the exact tree of each run; three status rules (the run's own result, a background notification, a later rc line) accept reads up to 4 h after the run starts and not up to the push, which can only decide some of the 24 runs still unknown and moves no push class (the one unknown push, 60a99e9f, has no surviving read).
