---
title: Prove a progressing body cannot renew its deadline
status: open
priority: 3
issue-type: task
created-at: "2026-11-01T20:23:22.264535+02:00"
---

src/lib/net/http-test.f BODY-TIMEOUT-CASES 'progressing body expires' does not show that progress leaves the body deadline unchanged: a review mutation giving PUMP-BODY a fresh deadline on every read (http-request.f PUMP-BODY :661) still passed: with renewal the server closes about 1000 ms after the last byte, near 2280 ms, inside RAW-AWAIT's 2000 ms window from about 1280 ms, and the case never checks when the close happened. So the case header's 'progress cannot renew it' and http.f BODY-TIMEOUT!'s 'Progress renews neither' are unproven. Make the case assert the close time against the deadline from BODY$, so the renewal mutation fails it. The 300 ms sleep at http-test.f:1807 (default body deadline) is redundant beside CLOSED-WITHIN?, which waits up to COLLECT-MS: drop it in the same change. Found by phil's body-lane review, 2026-11-01. Owner: carl.
