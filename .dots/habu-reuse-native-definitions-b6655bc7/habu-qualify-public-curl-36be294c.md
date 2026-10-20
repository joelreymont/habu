---
title: Qualify public curl proxy control
status: closed
priority: 1
issue-type: task
created-at: "2026-10-19T14:59:39.930163+03:00"
closed-at: "2026-10-19T15:19:51.577736+03:00"
close-reason: "Reviewed feature9deab0c3 CURL:PROXY! is present in selected public bea3fdac. Existing local native CURL suite passes from that exact installed prefix (exit0,test:ok,curl-test:ok), exercising environment proxy routing, explicit empty bypass and explicit replacement; independent Astra review clear. The prior600/484 staged/installed source-only candidate also passed but is redundant and retired. Selector preserved; this qualifies the CURL library interface, not the unknown publishers whole-engine gate/convergence evidence."
---

Tender Ollama wire caller requires CURL:PROXY!, absent from selected600eb464-mkdir-errno-aabc93c4. Reviewed library feature9deab0c3 differs from selected curl.f by only its proxy option/setter/docs/test delta. Qualify exact selected immutable source/engine/map plus those three reviewed files through existing local native CURL E2E, preserving unrelated libraries and public signatures. Stage source-only successor without replacing current until qualification, then publish exact identity and notify Kevin/Stuart. No engine rebuild, Tender edit, resolver workaround or full server retry.
