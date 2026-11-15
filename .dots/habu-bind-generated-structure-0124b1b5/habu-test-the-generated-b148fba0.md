---
title: "Test the generated-structure campaign's cases"
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T11:41:40.815476+01:00"
---

Problem: two of campaign 0124b1b5's acceptance cases have no test: a package with a private part named ARENA, whose STRUCTURE field is ptr ARENA:arena, gets generated words bound to the global ARENA:arena type; and a warm build where a compiled file names a linked file's +FIELD children checks. Acceptance: a test of each, passing on master. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/carl-cases.
