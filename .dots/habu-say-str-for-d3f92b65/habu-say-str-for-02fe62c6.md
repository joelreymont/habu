---
title: Say str for arch, os and compiler byte inputs
status: closed
priority: 2
issue-type: task
created-at: "2026-11-22T15:17:22.913009+01:00"
closed-at: "2026-11-23T10:11:21.200999+01:00"
---

Problem: rows in this lane's arch, os and compiler files still say ^u8 n for byte strings (parent's rule). Acceptance: /tmp/jerry-str/rules.md applied to the files in /tmp/jerry-str/brief-e5.md; the engine and x86-64 builds pass and the named checks are clean. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-e5.
