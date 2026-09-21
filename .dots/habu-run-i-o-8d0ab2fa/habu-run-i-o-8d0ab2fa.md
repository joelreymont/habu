---
title: Run I/O through an io_uring loop
status: open
priority: 1
issue-type: task
created-at: "2026-09-21T18:07:21.876870+03:00"
---

Problem: every wait in the tree parks a thread in a host call: lib/net/tcp4.f, udp4.f, serial.f, pty.f, process-pty-io.f and signal.f each poll(2) one fd from the calling task, lib/net/curl.f blocks in curl_easy_perform, and Tender runs one thread per transfer. Joel, 2026-09-21: 'linux has async io, why are we not using it and launching threads around blocking ops?', then 'land the async primitives first, then build curl, etc. on top of those'; interface chosen by Joel: io_uring. Plan: TASK gains STOP/WAKE (the hosted row of docs/tasking-models.md Decision); package AIO owns one io_uring ring and one loop task that waits on the completion queue; any task submits an operation and STOPs until the loop WAKEs it with the completion; the libraries' waits and the curl multi loop are rebuilt on that. Children in order: STOP/WAKE, the readiness loop (POLL-ADD, TIMEOUT, CANCEL, AWAIT, AWAIT-ANY), then in parallel the completion operations, the poll(2) migrations, the curl multi loop (habu-multiplex-http-transfers-ccf9029a) and the compile-ban question. Acceptance: the children closed; docs/aio.md and docs/threads.md describe the model; Tender's acceptance in ccf9029a met. Kernel floor 5.10 (Tender: Azure VM, systemd @system-service admits @aio; 6.1 or 6.8; x86_64 possible). Depends: none. Ownership: hazel. Claim: campaign, not claimable.
