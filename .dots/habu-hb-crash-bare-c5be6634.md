---
title: "hb crash: bare undefined U-TYPE token SIGSEGVs"
status: open
priority: 1
issue-type: task
created-at: "2026-07-12T03:52:11.885033+02:00"
---

FOR THE CORE LANE. Found by the fmt-rename worker probing the interpreter 2026-07-12: feeding the bare undefined word U-TYPE to bin/hb SIGSEGVs (habu-crash regs, signal 0xb) where near-identical tokens X-TYPE / U-TYPX correctly report E-UNDEFINED. Reproducible on the current fixpoint. Likely an internal name collision (U-TYPE is an internal checker word?) reachable from user input - a crash on arbitrary input is a fail-closed violation regardless. Debugger-evidence-first per docs/debugging.md.
