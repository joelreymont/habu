---
title: Parse argv and load files in main.f
status: closed
priority: 2
issue-type: task
created-at: "2026-09-29T13:12:28.965848+03:00"
blocks:
  - habu-move-pkgs-using-22f18b81
closed-at: "2026-10-01T13:51:46.000000+03:00"
close-reason: "src/habu/main.f ENGINE-MAIN:RUN: test/main-argv.f 31 cases (27 agree with bin/hb on rc, stdout and stderr) pass on spark e1602d3d"
---

Problem: argv parsing and file loading for the product engine are assembly. First of I10a-c.
Acceptance: `src/habu/main.f`: `--load`, `--build`, `--`, files, usage exit 64 on an unknown flag, shebang skip, `included` per file; testable by calling its words with a synthetic file list.
Files: `src/habu/main.f`, a test that calls its words with a synthetic file list.
Verify: spark: that test; gate.
Depends: habu-scan-and-interpret-eea996a2 (I4), habu-move-pkgs-using-22f18b81 (I8).
Route: Alder (shared: src/habu/main.f and the test).
Ownership: krait (Intel lane).
Claim: unassigned.
