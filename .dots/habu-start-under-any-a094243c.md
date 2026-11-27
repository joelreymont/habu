---
title: Start under any file-size limit
status: open
priority: 2
issue-type: task
created-at: "2026-11-27T13:40:21.211468+01:00"
---

Problem: every Linux start of an image with a code alias (hb, an unstripped program, every x86-64 program) makes the region a memfd and ftruncates it to REGION (src/habu/habu1.f:2393-2407 REGION-MEMFD, src/habu/boot-x64.f:74-94 REGION-MEMFD,), which the kernel charges to RLIMIT_FSIZE: under ulimit -f below 35840 KiB the start exits 71, and the message names ulimit -n and ulimit -v (habu2.f:1414, boot-x64.f:909-910). FORK makes its child's copy the same way (habu1.f:2486-2560, kernel-x64.f:1040-1100). Reproducer: /tmp/norbert-habu-x86-fsize.md (reported by norbert via carl). Acceptance: on arm64 and x86-64 Linux, hb and built programs start and fork under ulimit -f 1 as they do with no limit; a region mapping that fails names the limit that can fail it. Depends: none. Claim: unassigned.
