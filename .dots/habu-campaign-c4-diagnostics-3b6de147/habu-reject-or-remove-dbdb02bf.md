---
title: Reject or remove the stops public internal words reach
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T08:22:36.745444+01:00"
closed-at: "2026-11-16T04:07:34.398122+01:00"
---

Problem: habu2.f OVERLAY-EMIT:LDIE (rc 108), xref.f XREF-UNDEFINE-GUARD (rc 108) and sumtype.f TXN-INSTALL's 76 die are reachable from a program through the public internal words CHECKER-SCOPE-START-NEUTRAL and TYPE-DECL:TXN-INSTALL. Acceptance: the overlay stop and the undefine guard each write one record (CHECKER-SCOPE-START-NEUTRAL stays public: tools call it); TXN-INSTALL's install-once check and its die go once hb's name stripping hides TYPE-DECL:TXN-INSTALL from programs. Depends: habu-re-enable-hb-a2404c26 for TXN-INSTALL. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
