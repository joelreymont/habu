---
title: "Keep a matched payload's inner quotation"
status: active
priority: 2
issue-type: task
created-at: "2026-11-25T20:46:50.540276+01:00"
---

Problem: a quotation type inside a constructor's or MATCH's payload quotation is not judged kept (src/core/type-family.f:3800 raises the scope only in TFC-STORE mode, so MATCH's payload leaves it unkept), though docs/forth-card.md says a quotation type inside a quotation type always counts as kept. With ': PASSQ ( [ -- ] -- [ -- ] ) ;', 'TYPED-VARIABLE KV [ -- ]' and 'ENUM act VARIANT idle ;VARIANT VARIANT fire FIELD q [ [ -- ] -- [ -- ] ] ;VARIANT ;ENUM', the word ': RUN ( act -- ) MATCH act idle OF ENDOF fire OF [: parse-name type ;] swap execute KV ! ENDOF ;MATCH ;' loads and KV then holds a quotation that parses; with a quotation that reads a local the run dies on a dangling local. Acceptance: load and check reject both at the execute or the store; a generic enum's, structure's, cell's, newtype's and option's answers stay. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-payload.
