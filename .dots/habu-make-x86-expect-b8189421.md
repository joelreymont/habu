---
title: Make x86 EXPECT-BALANCED independent of invocation
status: open
priority: 2
issue-type: task
created-at: "2026-10-28T23:41:58.177170+02:00"
---

On the x86-64 peer (pulsar-ua-106), boot-harness images pass EXPECT-BALANCED only when run from their own directory as ./image; by absolute path, with extra argv, under env -i or from another directory the first BUILD check EXPECT-BALANCED fails (base syscalls image exits 21, spawn-fds images 30/24), 6 of 6 times, on base images too. A harness or x86 boot defect that depends on argv/environment/cwd. Source: lane 1a review 2026-10-28 (probes /tmp/carl-spawn-rev).
