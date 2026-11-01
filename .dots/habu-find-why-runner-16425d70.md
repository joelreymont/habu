---
title: Find why runner fixtures failed in a suite run
status: open
priority: 3
issue-type: task
created-at: "2026-11-01T02:24:36.132921+02:00"
---

Observed 2026-11-01: the full suite on the heapsort lane (engine /tmp/carl-order/eng2, master 1d883da0 + 'Heapsort the checker handover order') failed row stdlib-runner-fixtures with two asserts after 'fixture progress': 'assert 40 expected 0 got 1', 'assert 41 expected true got false' (/tmp/carl-order/suite.log:1574-1585). The row's own logs were removed with the suite's TMPDIR. src/lib/test/runner-test.f passed alone on the same engine (15.6 s), and its signal-child mode passed three times under 30 busy loops. The two failures fit GTT-TEST-PROGRESS-SIGNAL or GTT-TEST-PROGRESS-CAPTURE-FLUSH (child rc 1, 'test: ok' missing). Next: if it recurs, keep the row's out/err and read which child failed and why. Claim: unassigned.
