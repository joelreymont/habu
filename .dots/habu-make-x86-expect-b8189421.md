---
title: Make x86 EXPECT-BALANCED independent of invocation
status: open
priority: 2
issue-type: task
created-at: "2026-10-28T23:41:58.177170+02:00"
---

On the x86-64 peer (pulsar-ua-106), boot-harness images pass EXPECT-BALANCED only when run from their own directory as ./image; by absolute path, with extra argv, under env -i or from another directory the first BUILD check EXPECT-BALANCED fails (base syscalls image exits 21, spawn-fds images 30/24), 6 of 6 times, on base images too. A harness or x86 boot defect that depends on argv/environment/cwd. Source: lane 1a review 2026-10-28 (probes /tmp/carl-spawn-rev).

Lane 1a fold-in worker 2026-10-28: in a fresh mktemp directory on pulsar, every x86 row image run as ./image still exits on its last check EXPECT-BALANCED (dir 30, dup 29, fcntl 29, fork 29, io 30, spawn 30, spawn-fds 30, spawn-fds-fail 24, syscalls 21, ...; armed images 83); direct or under timeout, login shell or not. The reviewer's passing invocation did not reproduce, so the failure may not depend on invocation at all. Log: carl-spawn/.out/fold-peer.log.
