---
title: Convert split-str files with a converter, batch plain-02
status: active
priority: 2
issue-type: task
created-at: "2026-11-24T22:25:48.528020+01:00"
---

Problem: 844 marked files fail unmarked (census /tmp/jerry-p1/unmark/result.txt; 62 batches in /tmp/jerry-p1/unmark/batches-*.txt); about 6800 definitions bind a str as two locals. Acceptance: a converter applies design-str.md decision 6 to definitions whose locals follow their effect; batch plain-02's 25 files carry no marker, pass check.f and their suites. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-unm-p02.
