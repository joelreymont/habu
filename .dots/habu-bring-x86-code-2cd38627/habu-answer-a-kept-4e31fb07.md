---
title: Answer a kept-alive HTTP request in the handoffs it needs
status: open
priority: 2
issue-type: task
created-at: "2026-11-26T14:37:49.192879+01:00"
---

Problem: a kept-alive request through src/lib/net/http.f costs about nine thread switches (AIO loop 3, listener 4, worker 2; reproducer /tmp/jerry-p1/ctxsw), at about 24 us each on pulsar (x86-64 under WSL2), so Zen, Tender and Maki serve slower on x86-64 than on arm64; the server's contract needs one listener-to-worker and one worker-to-listener handoff. Causes: io_uring interrupts the submitting listener to complete its polls (aio.f:848), every completion hops through the AIO loop task (aio.f:1-3), READ-REQUEST polls a socket the listener just saw readable (http.f:250-251), and a response is two sendto calls. Acceptance: the reproducer's switches per request fall to what the contract needs on spark and pulsar, with every answer correct. Depends: none. Claim: unassigned.
