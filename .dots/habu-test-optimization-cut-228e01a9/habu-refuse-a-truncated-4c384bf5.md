---
title: Refuse a truncated /proc/self/exe path
status: closed
priority: 3
issue-type: task
created-at: "\"2026-10-02T12:02:57.351048+02:00\""
closed-at: "2026-10-14T19:41:03.050546+03:00"
close-reason: "Implemented in 73ec7f9d: engine-id refuses a Linux readlink count at its buffer cap through a shared cache boundary; macOS focused suites and convergence passed. Linux deep-path runtime proof remains tracked separately by 46d88dfe."
---

Problem (lane 291 r4-ptree): src/lib/engine-id.f:81-83 ENGINE-SELF-LINUX accepts a readlink result equal to EID-PATH-CAP, which means the path was truncated; ENGINE-SELF-MACOS (:77-79) refuses it (n >= EID-PATH-CAP gives 0). A Linux engine at a path of 1025+ bytes reports a truncated path as its identity and source root. Acceptance: ENGINE-SELF-LINUX refuses n >= EID-PATH-CAP like the macOS arm; shown on Linux with an engine copied under a directory chain longer than EID-PATH-CAP (needs a Linux host, see 46d88dfe). Files: src/lib/engine-id.f.
