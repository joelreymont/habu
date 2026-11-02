---
title: "Measure OpenAI's silence during a long streamed response"
status: open
priority: 2
issue-type: task
created-at: "2026-11-02T16:17:28.159200+02:00"
---

Problem (Fable feature review of MODEL's streamed background responses, D1, 2026-11-02): MODEL ends an exchange after STALL-SECONDS (60) with under 1 byte/s (src/lib/net/model.f EXCHANGES, CURL:LOW-SPEED!) and resumes only when the exchange heard an event, so a provider silent for over 60 s twice in a row ends the call E-TRANSPORT and cancels the response. The design assumed keepalive events keep bytes flowing; the live probe (/tmp/carl-model-probe/live.out, 12 s, 68 events) saw none. The 95-300 s high-effort calls that lost Tender's documents are the case at risk. Acceptance: one live streamed background CHAT at high reasoning effort lasting several minutes, with each event's arrival time logged; the longest gap between bytes and whether keepalive events arrive, recorded here; if a gap can exceed STALL-SECONDS, the fix at the owner (MODEL) designed from that evidence before Tender switches to src/lib/net/model.f. Files: a probe under /tmp, src/lib/net/model.f if a fix follows. Verify: the probe's log. Depends: the MODEL stack landed (dot habu-let-a-model-c56be9a7). Live run by the lead under Joel's 'you run it' (LESSONS 'a live check of MODEL'). Ownership: carl. Claim: unassigned.
