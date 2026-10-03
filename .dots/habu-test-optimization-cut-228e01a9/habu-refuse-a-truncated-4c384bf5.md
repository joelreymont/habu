---
title: Refuse a truncated /proc/self/exe path
status: open
priority: 3
issue-type: task
created-at: "2026-10-02T12:02:57.351048+02:00"
---

Problem (lane 291 r4-ptree): lib/engine-id.f:81-83 ENGINE-SELF-LINUX accepts a readlink result equal to EID-PATH-CAP, which means the path was truncated; ENGINE-SELF-MACOS (:77-79) refuses it (n >= EID-PATH-CAP gives 0). A Linux engine at a path of 1025+ bytes reports a truncated path as its identity and source root. Acceptance: ENGINE-SELF-LINUX refuses n >= EID-PATH-CAP like the macOS arm; shown on Linux with an engine copied under a directory chain longer than EID-PATH-CAP (needs a Linux host, see 46d88dfe). Files: lib/engine-id.f.
