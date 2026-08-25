---
title: Habu TRUST re-pin tool
status: closed
priority: 2
issue-type: task
created-at: "2026-07-02T22:34:25.974410+02:00"
---

Every edit to a heavily-TRUSTed emitter file (src/habu/habu2.f has 91 TRUSTED.md line-pins) drifts the pins and fails trust-lint (SITE-DRIFT + STALE-ROW) in the full gate. There is NO re-pin tool - only reporting scanners (tools/trusted-inventory.f, tools/trust-lint.f). Build a checked Habu tool that reads TRUSTED.md, re-scans each row's word for its current file:line by name (authoritative, offset-independent), and rewrites the pins in place; keep trust-lint green after. Prototype exists as a throwaway (/tmp/repin.f, offset-function based) used to land habu-emit-open-failure - promote to a proper name-keyed tool with FILEMAP row + test. Without it, every emitter commit needs a manual 91-row re-pin (error-prone; non-Habu sed/awk forbidden).

## Resolution — superseded by habu-replace-trusted-md-381aeab3

No re-pin tool is needed. Dot 381aeab3 re-keyed TRUSTED.md rows by (word name +
file) and deleted the `file:line` pins entirely, so there are no line pins left
to drift or re-pin. `SITE-DRIFT` is gone; `tools/trust-lint.f` resolves each
row's word to its current line at report time. An edit that shifts 100 lines in
an emitter file keeps trust-lint green with zero row edits, which is exactly the
toil this dot proposed a tool to automate. Closing as superseded (the one-time
name-keyed migration shipped in 381aeab3's commit).
