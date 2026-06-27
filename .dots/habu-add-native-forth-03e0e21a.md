---
title: Add native Forth xref words
status: open
priority: 1
issue-type: task
created-at: "2026-06-27T08:30:59.650560+02:00"
---

Root cause: semantic dictionary/code inspection currently depends on ad hoc source searches or external wrappers. Native bin/hb resolves latest/body> but not xref/see/WORDS-style public xref words, so agents cannot inspect call/reference ownership from the Forth dictionary. Fix: add checked in-image xref vocabulary words such as XREF, SEE, USES/USED-BY or equivalent over the live dictionary/signature/call metadata, document them in docs/debugging.md and docs/forth.md, and keep any CLI wrapper as a thin hb entry point. Validate with dictionary/xref focused tests and the native gate.
