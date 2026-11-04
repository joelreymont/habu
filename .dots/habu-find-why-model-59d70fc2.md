---
title: "Find why MODEL:CHAT background streams hang"
status: active
priority: 1
issue-type: task
created-at: "2026-11-04T13:20:30.303547+02:00"
---

Problem (kevin, 2026-11-04, installed hb): MODEL:CHAT calls for ~48 KB document prompts to gpt-6.1-terra at effort high, sent as background streams (store false, background true, stream true, strict json_schema, src/lib/net/model.f:1155-1208), stayed open 10-48 min, receiving ~200 B/min after 22-48 KB of opening events. The same prompt as one plain request answered in 52.7 s. Two calls ended at 13:15, 9 s apart, with curl 28 and a cancel; ids resp_0eb6662739bd0199006aeb089de45887d2a5653fd8522140f3 and resp_009d09ebbce9ed96006aeb0e6b7ac887d28b53a313cb7b21b7. model.f has no whole-call deadline; its only guard is curl's low-speed limit (STALL-SECONDS, :180, :1683), which keepalives satisfy. Diagnosis lane running (brief .out/carl-file/brief-chat-hang.md, scratch /tmp/carl-chat, no workspace, no repository change). Acceptance: the responsible layer named with evidence, then a fix brief. Blocks Tender's T6 live check. Claim: agent=carl.

Lane K result 2026-11-04 (probes /tmp/carl-chat, GET only):
- The provider showed resp_0eb6… and resp_009d… still `queued` 48 and 23 min after creation: no output, usage or error. MODEL's cancels timed out (curl 28).
- resp_0528… (pid 36702) is `failed`, server_error. MODEL reported it as E-HTTP with no message: ERROR-EVENT (model.f:2205) reads a flat code and message that the live event does not carry (its shape inferred, not logged), and the 400 came from cancelling a response that had already failed.
- A queued response's stream carries only keepalives, about one every 33 s.
- Responsible layer: MODEL has no bound on a whole call or on time in the queue (:180, :1683, :2352), so it never ends.
- Lane K2 running (brief .out/carl-file/brief-chat-queue.md, scratch /tmp/carl-chat2): 16 tiny requests timing how long the provider holds each before starting, background vs held. It decides the fix, which goes to Joel because it touches his 2026-11-01 "background mode" choice.

Fact for the fix (kevin, 2026-11-04 13:40): kevin cancelled resp_0eb6… and resp_009d… (now `cancelled`). Six calls killed by a signal (five score processes and pid 30572, killed at carl's word) each held a queued response whose id was never logged, because MODEL logs the id only on a fault (model.f:2808). The provider cannot list responses, so those six cannot be cancelled and may still run and bill. A call killed by a signal leaves its response unreachable unless the id is logged when the response is created.

K2 (2026-11-04, /tmp/carl-chat2, 16 tiny POSTs): held requests were in_progress 0.4-1.9 s after the POST (median 0.8 s) and never queued; background requests sat queued a median 46 s, and 2 of 7 never started in 10 minutes (cancelled). All background ids end cancelled or completed, except the first r1 background request: killed at ~120 s before its id was logged, so it may still be queued and cannot be cancelled. Two transport failures on this host (curl 35 on a POST, a failed connect on a GET). K3 (running, worker, /tmp/carl-chat3, brief carl-file/brief-chat-held.md): whether a held stream that thinks for minutes ever goes 60 s without a byte (MODEL's stall bound, model.f:180), and whether a held request keeps running at the provider after its connection is cut.
