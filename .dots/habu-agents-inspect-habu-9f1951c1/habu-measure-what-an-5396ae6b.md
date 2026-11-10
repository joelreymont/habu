---
title: Measure what an undefined-word refusal could have said
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T09:16:39.231420+02:00"
---

Problem: the campaign builds inspection words for a live REPL, and nothing had measured where agents spend their calls. Second review, 2026-11-10, over the Claude Code transcripts of this repository since 2026-11-01 (regular-expression counts, a hand sample of the matches checked): reading and searching are 74% of agents' tool calls, with a median 29 calls before the first edit; about a quarter of calls look up Habu words; hb is fed a REPL on stdin in 185 of 13,857 runs, so a word reachable only in a live session is rarely where agents are. Of 817 undefined-word refusals, 53% name a word defined in the Habu source but not visible where it was used and 31% a word Habu does not have (bye, 112 times); for a library or private word that is not loaded, SEE prints 'xref: not found'. A stack mismatch, the moment the typed-state observation serves, is 148 of 988 refusal episodes, each reaching a clean run in a median 2-3 calls.
Acceptance: recorded here, with no code. For the 391 undefined-word episodes: from a scan of the source, where each word is defined and why it was not visible where used; the calls each episode took until a clean run (the second review: median 3, 90th percentile 18); and from both, the most a refusal that stated the defining file and the reason could save. A changed refusal is a change to diagnostic output: its design goes to Joel before a worker starts.
Files: none; scripts over the transcripts and the source.
Verify: the recorded numbers and the scripts that produced them.
Depends: none.
Ownership: measurement only. Joel, 2026-11-10: this dot leads the campaign. Claim: unassigned.
