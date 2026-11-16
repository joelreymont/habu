---
title: Reject a SUMTYPE view or init payload at its token
status: active
priority: 2
issue-type: task
created-at: "2026-11-16T04:31:11.944308+01:00"
---

Problem: SUMTYPE rejects a read-view payload at its token ('unknown payload type') but accepts other scope- or region-parameter families: src/core/sumtype.f TDECL-VPAY-FAM? excludes only C2-READ-FAM, and TDV-ARGS-BUILD never checks argument kinds. 'SUMTYPE stm<a,b> VARIANT some mut-view<a,a,b,n> ;VARIANT ;SUMTYPE' and 'SUMTYPE sti<a,b> VARIANT some n a ;VARIANT VARIANT two init<a,b> ;VARIANT ;SUMTYPE' (/tmp/jerry-kinds/stm.f, sti.f) load to ';SUMTYPE' and fail there E-BAD-STORED-SIGNATURE 'the signature does not parse here', whose help asks for a stack effect the program never wrote. Acceptance: each such payload is rejected at its token with a declaration record whose label states the rule and whose help names a payload that loads; a sumtype suite case pins it. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-w176.
