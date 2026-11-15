---
title: "Make MAKE, UNMAKE and DEFTYPE's casts typed children"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-14T19:06:02.599345+01:00"
closed-at: "2026-11-14T20:44:51.393942+01:00"
---

Problem: a product's MAKE and UNMAKE (src/core/sumtype.f TDECL-PROD-WORD, empty bodies passed under CTOR-PEND) and DEFTYPE's >NAME and NAME>N casts (src/lib/type/deftype.f VNOM:MINT through NG-EVAL, replayed as text by src/habu/verify-source.f RECORD-DEFTYPE) are evaluated source text. Acceptance: each is a typed child made through CHILD-RUN with an identity CHILD-ABI shape whose body is COMP-BODY:cast (as DEF-CAST-ROW) and whose rows come from the family's schema, replayed as TDECL-ADDR-REG replays a child; verify-source records them without text; the type-decl, structure-decl and deftype suites pass. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-make.
