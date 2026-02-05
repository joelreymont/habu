---
title: Fix parity docs
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-02-05T12:11:01.492519+01:00\\\"\""
closed-at: "2026-02-05T12:47:20.374136+01:00"
close-reason: Fix CL parity docs
---

docs/cl-spec-status.md:1, docs/PROGRESS.md:1: remove incorrect "100% complete" + stale counts; reference the audited table + list missing/partial.
docs/cl-symbols.md:1: clarify row vs symbol counts; fix header counts; point to the audit tool.
tools/cl_symbols_audit.py:1: compute/print row vs symbol rollups and list missing/partial symbols.

Root cause: docs drifted + multiple trackers with conflicting totals.
Fix: make the docs depend on the audited table; make the audit tool report both row and per-symbol status.
Proof: `python3 tools/cl_symbols_audit.py` prints `sym status: ✓ 955 | ⚠ 10 | ✗ 13`.
