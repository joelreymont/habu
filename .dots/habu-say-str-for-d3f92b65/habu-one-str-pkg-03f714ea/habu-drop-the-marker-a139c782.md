---
title: Drop the marker where files pass unmarked
status: active
priority: 2
issue-type: task
created-at: "2026-11-24T22:25:48.516675+01:00"
---

Problem: 289 of the 1133 split-str files pass tools/check.f --verify-only with the marker deleted (census /tmp/jerry-p1/unmark/pass.txt); deleting it moves their lines, which 6 suites name. Acceptance: those files carry no marker; the suites pass with expectations at the new places; a file that fails for any other cause keeps its marker for a conversion lane. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-unmark1.
