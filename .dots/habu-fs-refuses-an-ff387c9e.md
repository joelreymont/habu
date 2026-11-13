---
title: FS refuses an over-long temp path outside the FS band
status: active
priority: 2
issue-type: task
created-at: "2026-11-13T17:58:10.052720+01:00"
---

Problem: src/lib/fs-mutate.f BUILD-SIBLING (:439, reached by FS:RESERVE-SIBLING and the atomic writes), the temp base check (:764), the TMPDIR check (:834-836) and src/lib/fs.f's walk root (:810) refuse a path that does not fit PATH-CAP with E-SPAN-CAPACITY, while a join that does not fit is E-FS-CAPACITY (fs.f:331, :823). A caller that maps the E-FS band (dots' REPORT-FAILURE) dies on an uncaught -6101 for a long dot path. Acceptance: FS refuses every path that does not fit its buffer with E-FS-CAPACITY; fs-mutate-test.f and fs-test.f expect it; Zen, Tender and Maki sites that catch E-SPAN-CAPACITY around these words are told to their owners. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-fscode.
