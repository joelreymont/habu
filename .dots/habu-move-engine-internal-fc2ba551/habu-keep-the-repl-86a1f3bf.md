---
title: "Keep the REPL's lines while a text is open"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-18T20:19:25.225918+01:00"
closed-at: "2026-11-18T20:42:32.210226+01:00"
---

Problem: the REPL evaluates each line in its reader's buffer, which the next line reuses, so a token a definition or declaration keeps past its line names other bytes: src/lib/ffi-abi.f FUNCTION: keeps its opener's pointer (OPENER-A), and a second FUNCTION: on a later line prints a record naming the wrong bytes; a rejection naming an earlier line's token is placed under the running token. Acceptance: test/repl-open-definition.f FN-LINES-CASE: the record names `FUNCTION:` and shows the first line under 'opened here'. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-dop.
