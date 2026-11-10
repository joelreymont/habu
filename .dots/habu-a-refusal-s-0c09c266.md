---
title: "A refusal's JSON packet shows a wrong declared effect"
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T10:40:13.013434+02:00"
---

Problem: `hb --load tools/check.f -- --json-errors /Users/joel/Work/reviews/habu-10x-2026-11-10/refusals/p14.f` reports `declared_effect` as `a b c d e n -- a b c d e n` for a definition declared `( n -- n )` with the locals group `{: a b | c -- d :}`. Seen once by the hunt; not explained. Source: the root-cause hunt of habu-checker-refusals-that-3152182a, 2026-11-10; Joel, 2026-11-10: "you must dot and launch a fix automatically for any issues you identify". Acceptance: first a reported cause, with no code change: where the packet's declared effect is taken from and why it carries the locals' names; whether it is wrong for any definition without the malformed group. Then the packet states the signature's effect. Files: named by the cause. Verify: the case's packet. Depends: none. Ownership: the checker's JSON diagnostics. Claim: agent=bob workspace=.jj-ws.noindex/habu-a-refusal-s-0c09c266
