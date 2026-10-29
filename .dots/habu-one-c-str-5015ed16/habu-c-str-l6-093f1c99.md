---
title: "C-string L6 tools A: open tool paths through FS"
status: active
priority: 2
issue-type: task
created-at: "2026-10-30T00:24:17.482438+02:00"
---

Problem: tools/tier-census.f:289, tier-dump.f:50, imgdump.f:106, imagedisasm.f:40, repair-packet-core.f:61, json-only.f:43 each keep a private C-path copier and PATH-CAP+1 buffer that writes a terminator and never refuses a 0 byte (census C29). Acceptance: copiers, buffers and caps deleted; opens go through FS:>PATHZ, FS:OPEN-SIZED, FS:WRITE-ALL; the tool tests and CLIs green; check.f and lints clean. Files: those six. Verify: carl-dots/.out/carl-file/brief-l6.md proof list. Depends: habu-c-str-l3-0c9f0875. Ownership: the six files. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-cstr-l6
