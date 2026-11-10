---
title: build-profile can write hb over a source
status: closed
priority: 2
issue-type: task
created-at: "2026-11-10T21:34:33.141972+02:00"
closed-at: "2026-11-10T21:55:03.364720+02:00"
close-reason: "landed in 1af0a0dd: the line is [--target name] (--engine | source.f) out text json, usage status 64; review ACCEPT"
---

Problem (review of soxqmyus, 2026-11-10): tools/build-profile.f tells hb's build from a program's by the count of paths alone, so a program line one path short (-- app.f out app.txt) builds hb and LINKER:WRITE renames it over app.f; its usage and unknown-target exits end with E-BUILD-PROFILE-IO (-7405, 'a report file this tool cannot write'), status 67, where tools/hb-build.f exits 64; its header says an arm64 <out> keeps the ad hoc signature, false for Linux; docs/debugging.md ties --target to the program build only. Acceptance: the line is [--target name] (--engine | source.f) <out> <text-report> <json-report>, four arguments after the option, anything else the usage message with status 64 and no file touched; the old three-path line is gone; the header and docs/debugging.md 'Profiling the build' state the new line. Files: tools/build-profile.f, docs/debugging.md. Verify: the brief's six command lines (.jj-ws.noindex/jerry-wasm/.out/briefs/pf.md). Depends: none. Ownership: jerry. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-pf.
