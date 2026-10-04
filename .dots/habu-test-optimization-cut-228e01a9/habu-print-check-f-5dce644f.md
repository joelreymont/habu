---
title: "Print check.f's pre-pass records in the chosen mode"
status: open
priority: 3
issue-type: task
created-at: "2026-10-03T16:26:25.404827+03:00"
---

Found by review 458 (b952622e): tools/check.f without --json-errors prints the pre-pass's checker records as JSON lines (tools/check-core.f CHK-RUN-PREVERIFY sets LINT-TRUE DIAG-JSON!; comment ~1596-1598 'The checker's own diagnostics are JSON lines in either mode') while its own E-STATEMENT-THROW and duplicate records follow the flag as prose, so a default-mode run mixes two formats (fixture: a definition with a mismatched effect, E-MISMATCH printed as JSON with no flag). Acceptance: every record check.f prints follows the mode (prose by default, JSON under --json-errors), seen failing first; the contract tools that parse the child's stream keep working.
