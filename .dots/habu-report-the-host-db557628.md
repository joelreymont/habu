---
title: "Report the host's usable cores and memory"
status: open
priority: 2
issue-type: task
created-at: "2026-10-31T19:29:11.624800+02:00"
---

Maki design r2 (/Users/joel/Work/maki/.jj-ws.noindex/tim/.out/chat-tools/design-chatgpt-r2.md §4.5, §5 item 3, lane H5): OS-CPU:PERFORMANCE-CORES ( -- n ) (macOS hw.perflevel0.physicalcpu, else the online count; Linux the cgroup CPU quota when set) beside OS-CPU:ONLINE (src/lib/os-cpu.f), and OS-MEMORY:TOTAL ( -- n ), bytes, in a new package OS-MEMORY (macOS hw.memsize; Linux the cgroup memory limit when set, else physical memory). Caller: Maki PROJECTS:START sizes its job runners (Joel: "automatically calculated, based on memory and cores"). Test against sysctl/getconf. Joel chose the names (2026-10-31, LESSONS.md). Ownership: carl. Claim: unassigned.
