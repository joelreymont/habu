---
title: Place x64 trap fixture in reach
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-09-30T11:31:49.416210+02:00\\\"\""
closed-at: "2026-09-30T12:09:04.382177+02:00"
close-reason: Darwin host reproduced E-X64EMIT-REACH from fake trap placement16; aligned target-near placement passes existing positive bytes and independent far-call refusal. Astra PASS; current-source full native gate499/499 exit0.
---

The full native gate's compiler-x64-emit row fails E-X64EMIT-REACH on Darwin arm64 because the trap fixture places code at address 16 while calling the live engine die entry above 4GB. Keep the emitter's signed rel32 contract and existing far-range refusal. Place the synthetic trap module at an aligned address near its actual trap target and derive the expected field from that placement. Acceptance is the unchanged x64 emitter suite failing before and passing after on the fresh current-source host; independent Astra review and the final native gate cover the integration.
