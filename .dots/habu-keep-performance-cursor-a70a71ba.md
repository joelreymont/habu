---
title: Keep performance cursor typed
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T23:07:54.170807+02:00"
---

tools/ptx/perf-registry.f:25-29 reopens package CAD-NUM and publishes consumer-specific CAD-NUM:PF-BO>N/PF-BL>N solely to pierce private BYTE-OFF>N/BYTE-LEN>N. The PERF prefix inside CAD-NUM recreates pseudo-scope in the wrong owner, expands CAD-NUM's public representation authority, and will break package sealing. Keep STR:SPLIT-NEXT's byte-off/byte-len cursor typed through the row parser, or add one owner-sanctioned generic projection at the string/CAD-NUM boundary that does not name PERF. PERF must not reopen another package to mint privileged bridges. Remove the two words with no aliases; prove old/wrong-package access rejects, cursor arithmetic stays checked, malformed/overflow rows preserve exact errors, canonical registry behavior is unchanged, and public dictionary/JIT/CODELEN shrink. Files: tools/ptx/perf-registry.f/tests plus the existing owning conversion API only if required.
