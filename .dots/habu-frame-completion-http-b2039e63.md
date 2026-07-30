---
title: Frame completion HTTP response
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T07:39:43.841174+02:00"
---

Why: response capacity and resumable writing are independent from request parsing. Result: package HTTP-COMP defines RESPONSE-BOUND ( n -- n ) for the complete status line, fixed headers, decimal Content-Length width at the maximum JSON body, and body; FRAME initializes a cursor over one immutable stable body; ADVANCE consumes and returns that cursor across caller-reported short writes until complete. All arithmetic is checked and the cursor owns no socket. Owner: completion HTTP response bound and framing cursor only in one response module. Production red: a stable JSON body cannot become a bounded HTTP response. Acceptance: zero and maximum body, every decimal-width edge, arithmetic overflow, every short-write split, exact wire bytes, and mutation attempts against the stable body select exact results. Forbidden: request parser, socket call, allocation, JSON, timeout, keep-alive, chunking, version, compatibility, metric, or lint. Smallest owning check: bin/hb --load maki/serve/http-response-test.f through FRAME and ADVANCE. Claim: unassigned.
