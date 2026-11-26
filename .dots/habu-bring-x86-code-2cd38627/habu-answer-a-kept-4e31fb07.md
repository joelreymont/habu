---
title: Answer a kept-alive HTTP request in the handoffs it needs
status: open
priority: 2
issue-type: task
created-at: "2026-11-26T14:37:49.192879+01:00"
---

Problem: a kept-alive request through src/lib/net/http.f costs more thread switches than the server's contract needs (reproducer /tmp/jerry-p1/ctxsw), at about 24 us each on pulsar (x86-64 under WSL2), so Zen, Tender and Maki serve slower on x86-64 than on arm64; the contract needs one listener-to-worker and one worker-to-listener handoff. Causes: READ-REQUEST polls a socket the listener just saw readable (http.f:251-252), and a response is two sendto calls. Acceptance: the reproducer's switches per request fall to what the contract needs on spark and pulsar, with every answer correct. Depends: none. Claim: unassigned.
