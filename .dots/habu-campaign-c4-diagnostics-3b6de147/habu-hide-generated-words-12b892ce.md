---
title: Hide generated words by privacy, not int-mark
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T08:09:22.168109+01:00"
closed-at: "2026-11-16T15:50:25.195143+01:00"
---

Problem: xml/c2.f HIDE and test/policy/dep.f MARK-LAST mark words internal with the int-mark primitive instead of declaring them in a private section. Acceptance: both use privacy; int-mark, its row and habu1.f BINTMARK deleted (x86 copy left to its step). Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-w216.
