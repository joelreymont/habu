---
title: "Measure OpenAI's silence during a long streamed response"
status: open
priority: 2
issue-type: task
created-at: "2026-11-02T16:17:28.159200+02:00"
---

Problem: MODEL ends an exchange after STALL-SECONDS (60, src/lib/net/model.f:274) with under 1 byte/s (CURL:LOW-SPEED!, :3829) and resumes only when the exchange heard an event, so a provider silent for over 60 s twice in a row ends the call E-TRANSPORT and cancels the response. The design assumed keepalive events keep bytes flowing; a 12 s live probe with 68 events saw none. The 95-300 s high-effort calls that lost Tender's documents are the case at risk. Joel 2026-11-02, asked to run the live check of MODEL: "you run it" (the lead writes and runs the probe). Acceptance: one live streamed background CHAT at high reasoning effort lasting several minutes, with each event's arrival time logged; the longest gap between bytes and whether keepalive events arrive, recorded here; if a gap can exceed STALL-SECONDS, the fix at the owner (MODEL) designed from that evidence. Depends: none. Claim: unassigned, owner carl.
