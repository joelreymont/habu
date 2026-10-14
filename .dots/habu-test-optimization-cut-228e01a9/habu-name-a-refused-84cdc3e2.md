---
title: "Name a refused does> clause in check.f's pre-pass"
status: closed
priority: 3
issue-type: task
created-at: "\"2026-10-01T12:03:31.936678+02:00\""
closed-at: "2026-10-14T19:33:10.324417+03:00"
close-reason: "Implemented in c465c5b2: check.f pre-pass reports refused does> clauses with the definer name, code, token and location in all modes; failing-before check cases and convergence passed."
---

Problem: when tools/check.f's pre-pass refuses a does> clause, it prints only the check.f trailer with no diagnostic (CHECK-DOES-RUN has no DIAGXT call). Measured by review 125 on r4-render 66fbaced with no rendering: $HOME/.cache/tmp/kestrel-r4-rev125/cases/m1g-plain-bad-clause.f (`does> ( -- n ) @ dup`), also m1f, m1h. Acceptance: a refused clause prints the same diagnostic shape (word, code, token, place) as a refused colon body in default, --json-errors and --all-errors output; the three cases run through tools/check-test-lib.f, seen failing first. Files: src/habu/verify-source.f or tools/check-core.f (wherever CHECK-DOES-RUN lives), tools/check-test-lib.f. Verify: tools/check-test.f rc 0; rebuild if the word is baked.
