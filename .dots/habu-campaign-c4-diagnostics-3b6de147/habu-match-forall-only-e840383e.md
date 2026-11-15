---
title: "Match forall only at a token's start"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T23:45:20.600740+01:00"
closed-at: "2026-11-16T04:07:35.478917+01:00"
---

Problem: src/core/checker.f FORALL-AT matches 'forall<' inside a longer name, so '{: q:forallish<forall<p,[--]>> :}' gets a help that does not parse (re-review 9a finding 3). Acceptance: the local's record carets the scheme and helps 'q:forallish<[--]>', pinned beside SN-LP in test/signature-nesting.f. Depends: none. Claim: agent=W143 workspace=.jj-ws.noindex/jerry-w143
