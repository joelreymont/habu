---
title: Follow a moved source root in the check
status: active
priority: 2
issue-type: task
created-at: "2026-11-16T09:06:07.941786+01:00"
---

Problem: a top-level call that moves the source root (SOURCE-ROOT:ROOT!, CD, POPPATH) is not modelled by tools/check.f's composition walk (src/habu/verify-source.f TOP-TOKEN, BOUND-CALL), which resolves later requires against the old root and rejects them: Zen master's src/server/server.f (SOURCE-ROOT:ACTIVE-SOURCE 2drop 2drop SOURCE-ROOT:DIRNAME SOURCE-ROOT:DIRNAME SOURCE-ROOT:ROOT!) gives E-MISSING-SOURCE for ../src/zen.f at 8:9, Tender main's src/office-source.f (ZEN-ROOT:ROOT$ SOURCE-ROOT:ROOT!) gives it for document/read-office.f at 4:9, under --verify-only and so in plain check.f's pre-pass; both load and build. The root each sets is computed, so the check cannot know it: the root-moving words need a control fact the walk reads, inherited by their callers, and a call carrying it leaves the rest of the source to the run. Run consumers from /tmp copies with a scratch HOME. Acceptance: check.f gives the load's answer for both entries; a test with a program that moves its root pins it. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-w206.
