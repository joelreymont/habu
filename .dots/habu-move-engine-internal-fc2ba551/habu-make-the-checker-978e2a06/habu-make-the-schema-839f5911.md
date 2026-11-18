---
title: Make the schema and type-declaration internals private
status: closed
priority: 2
issue-type: task
created-at: "2026-11-18T16:11:36.399804+01:00"
closed-at: "2026-11-18T16:36:15.627581+01:00"
---

Problem: the SCHEMA-REG internals (src/core/type-schema.f) and the TYPE-DECL internals (src/core/sumtype.f: TDECL-FAM-REG, TDECL-TXN-XT, TDECL-TXN-ARMED, TDECL-NAME-PREFLIGHT-XT, TDECL-CAPACITY-PREFLIGHT-XT, TDPLAN-N) listed in the parent are public. TYPE-DECL's error codes stay public: Maki reads TYPE-DECL:E-ENUM-TAG (maki/src/worker.f:850). Acceptance: each listed word is E-UNDEFINED outside its package; engine callers take it by EXPORT. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-sr.
