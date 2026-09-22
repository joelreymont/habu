---
title: Refuse a SUITE row that runs into the next row
status: open
priority: 3
issue-type: task
created-at: "2026-09-22T11:52:42.417698+03:00"
---

Problem (measured in chain FH): a SUITE os-memory row in test/gate-stdlib-cases.f without ;SUITE made lib/test/suite.f PARSE-ARGS take 'SUITE shadow-lint tools/lint/shadow-lint.f tools/lint/shadow-lint-test.f' as the os-memory child's argv - the engine included SUITE as a file (exit 74, 'include: cannot open .../SUITE') - and the shadow-lint row was never registered; the suite count fell by one and nothing named the missing terminator. Acceptance: PARSE-ARGS refuses by name an argument token that is a row keyword (SUITE, WHITEBOX-SUITE, SUITE-STDIN, GROUP, ;GROUP) and an end of input inside a row - 'test: row <name> has no ;SUITE' - with a rejected cases file pinned in the suite that tests lib/test/suite.f; every gate cases file still parses and the gate count is unchanged. Files: lib/test/suite.f and its test. Verify: that test, test/run.f. Depends: none. Ownership: hazel. Claim: agent=hazel workspace=.jj-ws/hazel-suite-row.
