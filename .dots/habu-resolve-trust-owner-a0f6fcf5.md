---
title: Resolve TRUST owner citations in the build
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T10:36:19.640089+02:00"
---

Issue 37 + suggestion 10. insn-schema.f:104-175 carries 62 TRUST declarations citing 'a dot' with no ID; tree-wide there are 27 dangling TRUSTED owner references including 63 checker-scan-index shims citing a nonexistent dot. Under the hard cut most of this surface deletes; whatever survives needs a real owner. Fix direction: a TRUST site's owner citation is a reference the build resolves or refuses at compile time — no ledger, no lint, one checker rule. Scope after phase-4 deletions land so the surviving TRUST set is the real one.
