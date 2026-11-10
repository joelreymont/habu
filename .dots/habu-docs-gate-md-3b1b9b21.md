---
title: docs/gate.md misstates what hb does with a piped stdin
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T16:39:14.015088+02:00"
---

Problem: docs/gate.md:184-185 says 'bin/hb file.f' drops to a REPL after a clean load and blocks on stdin. src/habu/main.f:250 ROUTED reads a non-terminal stdin before the file and runs that text instead of the file whenever it holds bytes: 'echo "1 drop" | bin/hb tools/gate-json-assert.f diag-contract /nonexistent/x.jsonl' gives rc 0 and no output, while with '</dev/null' it gives rc 1 'cannot open JSON fixture'. So a tool run with a stdin that holds bytes passes without running. Found by a review (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/toprec-review.md, Outside the commit). Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: the doc says what happens; and a run that names a file never drops the file for stdin silently (it runs the file, or refuses with a message). Files: docs/gate.md, src/habu/main.f. Verify: the two commands above. Depends: none. Ownership: the engine's entry. Claim: agent=bob workspace=.jj-ws.noindex/habu-docs-gate-md-3b1b9b21

Settled 2026-11-10 by the long-term rule (reproduced on master: the piped run exits 0 without running the file; with --load it runs it): any argument names what runs and stdin is then never read as source, on a terminal or off one; stdin is the source only with no argument. Brief /Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/Run-the-named-file-over-stdin.md; the worker first lists callers that rely on the old rule and stops if one exists.
