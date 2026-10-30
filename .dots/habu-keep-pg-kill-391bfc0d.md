---
title: "Keep pg-kill-test's socket path short whatever TMPDIR is"
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T22:41:59.564850+02:00"
---

Problem: test/db/pg-kill-test.f REPORT-READ deadline expires (exit 67, 60.3 s) when TMPDIR is long: postgres refuses a Unix socket path over 103 bytes on macOS and the cluster's socket directory lives under TMPDIR. Proof: TMPDIR=/tmp/jerry-lead-tmp/suite3/habu-native-suite-432839681319125-0 -> rc 67; TMPDIR=/tmp/jerry-lead-tmp/s -> rc 0, 1.4 s. The suite's per-row TMPDIR is long, so the row is red in a suite run from a deep TMPDIR. Acceptance: the socket directory sits at a short root that does not depend on the caller's TMPDIR (and is removed with the cluster); a long TMPDIR passes. Files: test/db/pg-kill-test.f and the pg test harness it uses. Verify: the pg row with both TMPDIRs above. Depends: none. Ownership: those files. Claim: unassigned.
